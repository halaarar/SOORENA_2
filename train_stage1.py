import pandas as pd
import torch
from torch.utils.data import DataLoader
from transformers import AutoTokenizer, AutoModelForSequenceClassification, get_linear_schedule_with_warmup
from torch.optim import AdamW
from tqdm import tqdm
import config
from utils.dataset import MechanismDataset
from utils.metrics import compute_binary_metrics

def load_stage1_data():
    """Load and prepare Stage 1 data (binary classification)."""
    # Load labeled splits
    train_df = pd.read_csv(config.TRAIN_FILE)
    val_df = pd.read_csv(config.VAL_FILE)
    test_df = pd.read_csv(config.TEST_FILE)
    
    # Load full dataset to sample unlabeled
    full_df = pd.read_csv(config.MODELING_DATASET_FILE)
    unlabeled_df = full_df[~full_df['has_mechanism']].copy()
    
    # Sample unlabeled for train/val/test (2:1 ratio)
    unlabeled_train = unlabeled_df.sample(n=len(train_df) * 2, random_state=config.RANDOM_SEED)
    remaining = unlabeled_df.drop(unlabeled_train.index)
    
    unlabeled_val = remaining.sample(n=len(val_df) * 2, random_state=config.RANDOM_SEED)
    remaining = remaining.drop(unlabeled_val.index)
    
    unlabeled_test = remaining.sample(n=len(test_df) * 2, random_state=config.RANDOM_SEED)
    
    # Combine positive + unlabeled, add binary labels
    train_df['binary_label'] = 1
    unlabeled_train['binary_label'] = 0
    stage1_train = pd.concat([train_df, unlabeled_train]).sample(frac=1, random_state=config.RANDOM_SEED)
    
    val_df['binary_label'] = 1
    unlabeled_val['binary_label'] = 0
    stage1_val = pd.concat([val_df, unlabeled_val]).sample(frac=1, random_state=config.RANDOM_SEED)
    
    test_df['binary_label'] = 1
    unlabeled_test['binary_label'] = 0
    stage1_test = pd.concat([test_df, unlabeled_test]).sample(frac=1, random_state=config.RANDOM_SEED)
    
    return stage1_train, stage1_val, stage1_test


def train_epoch(model, dataloader, optimizer, scheduler, device):
    """Train for one epoch."""
    model.train()
    total_loss = 0
    all_predictions = []
    all_labels = []
    
    for batch in tqdm(dataloader, desc="Training"):
        # Move to device
        input_ids = batch['input_ids'].to(device)
        attention_mask = batch['attention_mask'].to(device)
        labels = batch['labels'].to(device)
        
        # Forward pass
        outputs = model(input_ids=input_ids, attention_mask=attention_mask, labels=labels)
        loss = outputs.loss
        
        # Backward pass
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()
        scheduler.step()
        
        total_loss += loss.item()
        
        # Get predictions
        preds = torch.argmax(outputs.logits, dim=1)
        all_predictions.extend(preds.cpu().numpy())
        all_labels.extend(labels.cpu().numpy())
    
    avg_loss = total_loss / len(dataloader)
    metrics = compute_binary_metrics(all_predictions, all_labels)
    
    return avg_loss, metrics

def evaluate(model, dataloader, device):
    """Evaluate the model."""
    model.eval()
    total_loss = 0
    all_predictions = []
    all_labels = []
    
    with torch.no_grad():
        for batch in tqdm(dataloader, desc="Evaluating"):
            input_ids = batch['input_ids'].to(device)
            attention_mask = batch['attention_mask'].to(device)
            labels = batch['labels'].to(device)
            
            outputs = model(input_ids=input_ids, attention_mask=attention_mask, labels=labels)
            loss = outputs.loss
            
            total_loss += loss.item()
            
            preds = torch.argmax(outputs.logits, dim=1)
            all_predictions.extend(preds.cpu().numpy())
            all_labels.extend(labels.cpu().numpy())
    
    avg_loss = total_loss / len(dataloader)
    metrics = compute_binary_metrics(all_predictions, all_labels)
    
    return avg_loss, metrics


def main():
    """Train Stage 1 binary classifier."""
    print("=" * 50)
    print("STAGE 1: Binary Classification Training")
    print("=" * 50)
    
    # Setup device
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Using device: {device}\n")
    
    # Load data
    print("Loading Stage 1 data...")
    train_df, val_df, test_df = load_stage1_data()
    print(f"Train: {len(train_df)}, Val: {len(val_df)}, Test: {len(test_df)}\n")
    
    # Load tokenizer and model
    print(f"Loading {config.MODEL_NAME}...")
    tokenizer = AutoTokenizer.from_pretrained(config.MODEL_NAME)
    model = AutoModelForSequenceClassification.from_pretrained(
        config.MODEL_NAME,
        num_labels=config.STAGE1_NUM_LABELS,
        use_safetensors=True
    )
    model = model.to(device)
    print(f"Model loaded with {model.num_parameters():,} parameters\n")
    
    # Create datasets and dataloaders
    train_dataset = MechanismDataset(train_df, tokenizer, label_column='binary_label', max_length=config.MAX_LENGTH)
    val_dataset = MechanismDataset(val_df, tokenizer, label_column='binary_label', max_length=config.MAX_LENGTH)
    test_dataset = MechanismDataset(test_df, tokenizer, label_column='binary_label', max_length=config.MAX_LENGTH)
    
    train_loader = DataLoader(train_dataset, batch_size=config.BATCH_SIZE, shuffle=True)
    val_loader = DataLoader(val_dataset, batch_size=config.BATCH_SIZE)
    test_loader = DataLoader(test_dataset, batch_size=config.BATCH_SIZE)
    
    # Setup optimizer and scheduler
    optimizer = AdamW(model.parameters(), lr=config.LEARNING_RATE)
    total_steps = len(train_loader) * config.STAGE1_EPOCHS
    warmup_steps = int(total_steps * config.STAGE1_WARMUP_RATIO)
    
    scheduler = get_linear_schedule_with_warmup(
        optimizer,
        num_warmup_steps=warmup_steps,
        num_training_steps=total_steps
    )
    
    print(f"Training for {config.STAGE1_EPOCHS} epochs")
    print(f"Total steps: {total_steps}, Warmup steps: {warmup_steps}\n")
    
    # Training loop
    best_f1 = 0
    
    for epoch in range(config.STAGE1_EPOCHS):
        print(f"\nEpoch {epoch + 1}/{config.STAGE1_EPOCHS}")
        print("-" * 50)
        
        train_loss, train_metrics = train_epoch(model, train_loader, optimizer, scheduler, device)
        val_loss, val_metrics = evaluate(model, val_loader, device)
        
        print(f"Train Loss: {train_loss:.4f}, Acc: {train_metrics['accuracy']:.4f}, F1: {train_metrics['f1']:.4f}")
        print(f"Val Loss: {val_loss:.4f}, Acc: {val_metrics['accuracy']:.4f}, F1: {val_metrics['f1']:.4f}")
        print(f"Val Precision: {val_metrics['precision']:.4f}, Recall: {val_metrics['recall']:.4f}")
        
        # Save best model
        if val_metrics['f1'] > best_f1:
            best_f1 = val_metrics['f1']
            torch.save(model.state_dict(), config.STAGE1_MODEL_PATH)
            print(f"✓ Saved best model (F1: {best_f1:.4f})")
    
    # Evaluate on test set
    print("\n" + "=" * 50)
    print("Evaluating on test set...")
    model.load_state_dict(torch.load(config.STAGE1_MODEL_PATH))
    test_loss, test_metrics = evaluate(model, test_loader, device)
    
    print(f"\nTest Results:")
    print(f"Accuracy: {test_metrics['accuracy']:.4f}")
    print(f"Precision: {test_metrics['precision']:.4f}")
    print(f"Recall: {test_metrics['recall']:.4f}")
    print(f"F1 Score: {test_metrics['f1']:.4f}")
    print("\n✓ Stage 1 training complete!")

if __name__ == "__main__":
    main()