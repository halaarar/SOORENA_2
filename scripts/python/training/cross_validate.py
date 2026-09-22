import sys
from pathlib import Path

# Add repository root to Python path (4 levels up from scripts/python/training/)
REPO_ROOT = Path(__file__).resolve().parent.parent.parent.parent
sys.path.insert(0, str(REPO_ROOT))

import argparse
import os
import random
import time
import numpy as np
import pandas as pd
import torch
from torch.utils.data import DataLoader
from transformers import AutoTokenizer, AutoModelForSequenceClassification, get_linear_schedule_with_warmup
from torch.optim import AdamW
from sklearn.model_selection import StratifiedKFold, KFold, train_test_split
from sklearn.metrics import precision_recall_fscore_support
from sklearn.utils.class_weight import compute_class_weight
from tqdm import tqdm
import config
from utils.dataset import MechanismDataset
from utils.metrics import compute_binary_metrics, compute_multiclass_metrics

OUT_DIR = f"{config.RESULTS_DIR}/cross_validation"
VAL_FRACTION = 0.15  # share of each fold's training portion held out for epoch selection
MICRO_BATCH = 8      # examples per forward pass; gradients are accumulated to config.BATCH_SIZE


def free_device_memory(device):
    if device.type == 'mps':
        torch.mps.empty_cache()
    elif device.type == 'cuda':
        torch.cuda.empty_cache()


def set_seed(seed):
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)


def get_device():
    if torch.cuda.is_available():
        return torch.device('cuda')
    if torch.backends.mps.is_available():
        return torch.device('mps')
    return torch.device('cpu')


def load_data(limit=None):
    """Labelled publications (with Stage 2 label ids) and the unlabelled pool."""
    full_df = pd.read_csv(config.MODELING_DATASET_FILE)
    labeled = full_df[full_df['has_mechanism']].copy()
    labeled['label_id'] = labeled['Terms'].apply(lambda x: config.LABEL_TO_ID[x.split(',')[0].strip()])
    unlabeled = full_df[~full_df['has_mechanism']].copy()

    if limit:
        # Stratified subsample for quick smoke tests
        labeled, _ = train_test_split(labeled, train_size=limit, stratify=labeled['label_id'],
                                      random_state=config.RANDOM_SEED)
    return labeled.reset_index(drop=True), unlabeled


def make_folds(labeled, unlabeled, n_folds):
    """Yield (fold, train, val, test) DataFrames.

    Positives: stratified k-fold by mechanism class, then a stratified validation split
    inside each training portion. Stage 1 negatives: one 2:1 sample of the unlabelled pool,
    partitioned into k parts, so negatives are never shared between train, val and test.
    """
    negatives = unlabeled.sample(n=len(labeled) * 2, random_state=config.RANDOM_SEED).reset_index(drop=True)
    neg_folds = list(KFold(n_folds, shuffle=True, random_state=config.RANDOM_SEED).split(negatives))
    pos_folds = StratifiedKFold(n_folds, shuffle=True, random_state=config.RANDOM_SEED).split(
        labeled, labeled['label_id'])

    for fold, ((pos_trval, pos_test), (neg_trval, neg_test)) in enumerate(zip(pos_folds, neg_folds), start=1):
        pos_train, pos_val = train_test_split(
            labeled.iloc[pos_trval], test_size=VAL_FRACTION,
            stratify=labeled.iloc[pos_trval]['label_id'], random_state=config.RANDOM_SEED)
        neg_train, neg_val = train_test_split(
            negatives.iloc[neg_trval], test_size=VAL_FRACTION, random_state=config.RANDOM_SEED)
        yield fold, (pos_train, pos_val, labeled.iloc[pos_test]), (neg_train, neg_val, negatives.iloc[neg_test])


def stage1_frames(pos, neg):
    """Combine positives and negatives with binary labels, as in train_stage1.py."""
    frames = []
    for p, n in zip(pos, neg):
        p = p.assign(binary_label=1)
        n = n.assign(binary_label=0)
        frames.append(pd.concat([p, n]).sample(frac=1, random_state=config.RANDOM_SEED))
    return frames


def predict(model, dataloader, device):
    model.eval()
    preds, labels = [], []
    with torch.no_grad():
        for batch in dataloader:
            outputs = model(input_ids=batch['input_ids'].to(device),
                            attention_mask=batch['attention_mask'].to(device))
            preds.extend(torch.argmax(outputs.logits, dim=1).cpu().numpy())
            labels.extend(batch['labels'].numpy())
    return np.array(preds), np.array(labels)


def run_fold(stage, fold, train_df, val_df, test_df, tokenizer, device, epochs):
    """Train one fold; return per-epoch rows and the test predictions of the best epoch."""
    set_seed(config.RANDOM_SEED)
    label_col = 'binary_label' if stage == 1 else 'label_id'
    num_labels = config.STAGE1_NUM_LABELS if stage == 1 else config.STAGE2_NUM_LABELS
    warmup_ratio = config.STAGE1_WARMUP_RATIO if stage == 1 else config.STAGE2_WARMUP_RATIO
    select_key = 'f1' if stage == 1 else 'macro_f1'
    metrics_fn = compute_binary_metrics if stage == 1 else compute_multiclass_metrics

    model = AutoModelForSequenceClassification.from_pretrained(
        config.MODEL_NAME, num_labels=num_labels, use_safetensors=True).to(device)

    loaders = {name: DataLoader(MechanismDataset(df, tokenizer, label_column=label_col,
                                                 max_length=config.MAX_LENGTH),
                                batch_size=config.BATCH_SIZE, shuffle=(name == 'train'))
               for name, df in [('train', train_df), ('val', val_df), ('test', test_df)]}

    optimizer = AdamW(model.parameters(), lr=config.LEARNING_RATE)
    total_steps = len(loaders['train']) * epochs
    scheduler = get_linear_schedule_with_warmup(
        optimizer, num_warmup_steps=int(total_steps * warmup_ratio), num_training_steps=total_steps)

    if stage == 1:
        class_weights = torch.ones(num_labels)
    else:
        # Balanced class weights from this fold's training data, as in train_stage2.py
        class_weights = torch.tensor(
            compute_class_weight('balanced', classes=np.arange(num_labels), y=train_df['label_id']),
            dtype=torch.float)
    class_weights = class_weights.to(device)
    loss_fn = torch.nn.CrossEntropyLoss(weight=class_weights, reduction='none')

    epoch_rows, best = [], None
    for epoch in range(1, epochs + 1):
        model.train()
        total_loss = 0
        for batch in tqdm(loaders['train'], desc=f"Stage {stage} fold {fold} epoch {epoch}"):
            # Each batch of 16 is processed in micro-batches to fit in memory. Summing the
            # per-example weighted losses and dividing by the full batch's total weight gives
            # exactly the same loss and gradient as CrossEntropyLoss(weight=...) on the whole batch.
            labels = batch['labels'].to(device)
            denom = class_weights[labels].sum()
            optimizer.zero_grad()
            batch_loss = 0.0
            for idx in torch.arange(len(labels)).split(MICRO_BATCH):
                outputs = model(input_ids=batch['input_ids'][idx].to(device),
                                attention_mask=batch['attention_mask'][idx].to(device))
                loss = loss_fn(outputs.logits, labels[idx.to(device)]).sum() / denom
                loss.backward()
                batch_loss += loss.item()
            optimizer.step()
            scheduler.step()
            total_loss += batch_loss

        free_device_memory(device)
        val_metrics = metrics_fn(*predict(model, loaders['val'], device))
        test_preds, test_labels = predict(model, loaders['test'], device)
        test_metrics = metrics_fn(test_preds, test_labels)
        free_device_memory(device)

        row = {'stage': stage, 'fold': fold, 'epoch': epoch,
               'train_loss': total_loss / len(loaders['train'])}
        row.update({f'val_{k}': v for k, v in val_metrics.items()})
        row.update({f'test_{k}': v for k, v in test_metrics.items()})
        epoch_rows.append(row)
        print(f"  epoch {epoch}: val {select_key} {val_metrics[select_key]:.4f}, "
              f"test {select_key} {test_metrics[select_key]:.4f}")

        # Checkpoint selection on validation only; the test fold is never used for choosing
        if best is None or val_metrics[select_key] > best['row'][f'val_{select_key}']:
            best = {'row': row, 'preds': test_preds, 'labels': test_labels}

    del model, optimizer
    free_device_memory(device)
    return epoch_rows, best


def per_class_rows(fold, preds, labels):
    p, r, f, s = precision_recall_fscore_support(
        labels, preds, labels=np.arange(config.STAGE2_NUM_LABELS), zero_division=0)
    return [{'fold': fold, 'class': config.ID_TO_LABEL[i], 'precision': p[i], 'recall': r[i],
             'f1': f[i], 'support': s[i]} for i in range(config.STAGE2_NUM_LABELS)]


def summarize(folds_df, stage):
    keys = (['accuracy', 'precision', 'recall', 'f1'] if stage == 1 else
            ['accuracy', 'macro_precision', 'macro_recall', 'macro_f1', 'weighted_f1'])
    rows = []
    for split in ['val', 'test']:
        for k in keys:
            vals = folds_df[f'{split}_{k}']
            rows.append({'split': split, 'metric': k, 'mean': vals.mean(), 'sd': vals.std(ddof=1)})
    return pd.DataFrame(rows)


def main():
    parser = argparse.ArgumentParser(description="Stratified k-fold cross-validation of Stage 1 or Stage 2")
    parser.add_argument('--stage', type=int, choices=[1, 2], required=True)
    parser.add_argument('--folds', type=int, default=5)
    parser.add_argument('--epochs', type=int, default=None, help="override config (smoke tests only)")
    parser.add_argument('--limit', type=int, default=None, help="subsample labelled data (smoke tests only)")
    parser.add_argument('--max-folds', type=int, default=None, help="stop after this many folds (smoke tests only)")
    parser.add_argument('--out', default=OUT_DIR)
    parser.add_argument('--resume', action='store_true', help="skip folds already saved in --out")
    args = parser.parse_args()

    stage = args.stage
    epochs = args.epochs or (config.STAGE1_EPOCHS if stage == 1 else config.STAGE2_EPOCHS)
    os.makedirs(args.out, exist_ok=True)
    prefix = f"{args.out}/stage{stage}"
    device = get_device()
    print(f"Stage {stage}, {args.folds}-fold CV, {epochs} epochs, device: {device}")

    labeled, unlabeled = load_data(args.limit)
    tokenizer = AutoTokenizer.from_pretrained(config.MODEL_NAME)

    all_epochs, fold_rows, class_rows, pred_rows = [], [], [], []
    done_folds = set()
    if args.resume and os.path.exists(f"{prefix}_folds.csv"):
        # Pick up the folds saved by an interrupted run and skip them
        prev_folds = pd.read_csv(f"{prefix}_folds.csv")
        done_folds = set(prev_folds['fold'])
        fold_rows = [{**r, 'epoch': r['best_epoch']} for r in prev_folds.to_dict('records')]
        all_epochs = pd.read_csv(f"{prefix}_epochs.csv").query("fold in @done_folds").to_dict('records')
        pred_rows = [pd.read_csv(f"{prefix}_test_predictions.csv").query("fold in @done_folds")]
        if stage == 2:
            class_rows = pd.read_csv(f"{prefix}_per_class.csv").query("fold in @done_folds").to_dict('records')
        print(f"Resuming: folds {sorted(done_folds)} already done")

    for fold, pos, neg in make_folds(labeled, unlabeled, args.folds):
        if args.max_folds and fold > args.max_folds:
            break
        if fold in done_folds:
            continue
        if stage == 1:
            train_df, val_df, test_df = stage1_frames(pos, neg)
        else:
            train_df, val_df, test_df = pos
        print(f"\nFold {fold}: train {len(train_df)}, val {len(val_df)}, test {len(test_df)}")

        start = time.time()
        epoch_rows, best = run_fold(stage, fold, train_df, val_df, test_df, tokenizer, device, epochs)
        all_epochs.extend(epoch_rows)
        fold_rows.append({**best['row'], 'best_epoch': best['row']['epoch'],
                          'n_train': len(train_df), 'n_val': len(val_df), 'n_test': len(test_df),
                          'minutes': (time.time() - start) / 60})
        pred_rows.append(pd.DataFrame({'fold': fold, 'PMID': test_df['PMID'].values,
                                       'true': best['labels'], 'pred': best['preds']}))
        if stage == 2:
            class_rows.extend(per_class_rows(fold, best['preds'], best['labels']))

        # Save after every fold so partial results survive an interruption
        pd.DataFrame(all_epochs).to_csv(f"{prefix}_epochs.csv", index=False)
        pd.DataFrame(fold_rows).drop(columns=['epoch']).to_csv(f"{prefix}_folds.csv", index=False)
        pd.concat(pred_rows).to_csv(f"{prefix}_test_predictions.csv", index=False)
        if stage == 2:
            pd.DataFrame(class_rows).to_csv(f"{prefix}_per_class.csv", index=False)
        print(f"Fold {fold} done in {fold_rows[-1]['minutes']:.1f} min (best epoch {best['row']['epoch']})")

    summary = summarize(pd.DataFrame(fold_rows), stage)
    summary.to_csv(f"{prefix}_summary.csv", index=False)
    print("\nMean ± SD across folds:")
    for _, r in summary.iterrows():
        print(f"  {r['split']:4} {r['metric']:16} {100 * r['mean']:.1f} ± {100 * r['sd']:.1f}")


if __name__ == "__main__":
    main()
