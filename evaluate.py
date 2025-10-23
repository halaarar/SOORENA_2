import pandas as pd
import torch
import numpy as np
from torch.utils.data import DataLoader
from transformers import AutoTokenizer, AutoModelForSequenceClassification
import matplotlib.pyplot as plt
import seaborn as sns
import config
from utils.dataset import MechanismDataset
from utils.metrics import get_confusion_matrix, get_classification_report

def evaluate_model(model_path, test_file, num_labels, label_column, label_names=None):
    """Evaluate a trained model and generate report."""
    
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    
    # Load model and tokenizer
    tokenizer = AutoTokenizer.from_pretrained(config.MODEL_NAME)
    model = AutoModelForSequenceClassification.from_pretrained(
        config.MODEL_NAME,
        num_labels=num_labels,
        use_safetensors=True
    )
    model.load_state_dict(torch.load(model_path, map_location=device))
    model = model.to(device)
    model.eval()
    
    # Load test data
    test_df = pd.read_csv(test_file)
    test_dataset = MechanismDataset(test_df, tokenizer, label_column=label_column)
    test_loader = DataLoader(test_dataset, batch_size=16)
    
    # Get predictions
    all_preds = []
    all_labels = []
    
    with torch.no_grad():
        for batch in test_loader:
            input_ids = batch['input_ids'].to(device)
            attention_mask = batch['attention_mask'].to(device)
            labels = batch['labels']
            
            outputs = model(input_ids=input_ids, attention_mask=attention_mask)
            preds = torch.argmax(outputs.logits, dim=1)
            
            all_preds.extend(preds.cpu().numpy())
            all_labels.extend(labels.numpy())
    
    all_preds = np.array(all_preds)
    all_labels = np.array(all_labels)
    
    # Generate reports
    print("\nClassification Report:")
    print(get_classification_report(all_preds, all_labels, label_names))
    
    # Confusion matrix
    cm = get_confusion_matrix(all_preds, all_labels)
    
    plt.figure(figsize=(10, 8))
    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues',
                xticklabels=label_names if label_names else range(num_labels),
                yticklabels=label_names if label_names else range(num_labels))
    plt.xlabel('Predicted')
    plt.ylabel('True')
    plt.title('Confusion Matrix')
    plt.tight_layout()
    
    return all_preds, all_labels, cm

def main():
    """Generate evaluation reports for both stages."""
    
    print("=" * 50)
    print("STAGE 1 EVALUATION")
    print("=" * 50)
    
    # Prepare Stage 1 test data
    test_df = pd.read_csv(config.TEST_FILE)
    full_df = pd.read_csv(config.MODELING_DATASET_FILE)
    unlabeled_df = full_df[~full_df['has_mechanism']]
    unlabeled_test = unlabeled_df.sample(n=len(test_df) * 2, random_state=config.RANDOM_SEED)
    
    test_df['binary_label'] = 1
    unlabeled_test['binary_label'] = 0
    stage1_test = pd.concat([test_df, unlabeled_test])
    stage1_test.to_csv('data/processed/stage1_test_eval.csv', index=False)
    
    preds1, labels1, cm1 = evaluate_model(
        config.STAGE1_MODEL_PATH,
        'data/processed/stage1_test_eval.csv',
        config.STAGE1_NUM_LABELS,
        'binary_label',
        ['No Mechanism', 'Has Mechanism']
    )
    plt.savefig('reports/stage1_confusion_matrix.png', dpi=300, bbox_inches='tight')
    print("✓ Saved Stage 1 confusion matrix\n")
    
    print("\n" + "=" * 50)
    print("STAGE 2 EVALUATION")
    print("=" * 50)
    
    # Prepare Stage 2 test data
    test_df = pd.read_csv(config.TEST_FILE)
    test_df['label_id'] = test_df['Terms'].apply(lambda x: config.LABEL_TO_ID[x.split(',')[0].strip()])
    test_df.to_csv('data/processed/stage2_test_eval.csv', index=False)
    
    preds2, labels2, cm2 = evaluate_model(
        config.STAGE2_MODEL_PATH,
        'data/processed/stage2_test_eval.csv',
        config.STAGE2_NUM_LABELS,
        'label_id',
        list(config.LABEL_TO_ID.keys())
    )
    plt.savefig('reports/stage2_confusion_matrix.png', dpi=300, bbox_inches='tight')
    print("✓ Saved Stage 2 confusion matrix\n")
    
    print("\n✓ Evaluation complete!")

if __name__ == "__main__":
    import os
    os.makedirs('reports', exist_ok=True)
    main()