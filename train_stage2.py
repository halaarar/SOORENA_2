import pandas as pd
import torch
import numpy as np
from torch.utils.data import DataLoader
from transformers import AutoTokenizer, AutoModelForSequenceClassification, get_linear_schedule_with_warmup
from torch.optim import AdamW
from sklearn.utils.class_weight import compute_class_weight
from tqdm import tqdm
import config
from utils.dataset import MechanismDataset
from utils.metrics import compute_multiclass_metrics

def load_stage2_data():
    """Load Stage 2 data (multi-class classification)."""
    # Load labeled splits
    train_df = pd.read_csv(config.TRAIN_FILE)
    val_df = pd.read_csv(config.VAL_FILE)
    test_df = pd.read_csv(config.TEST_FILE)
    
    # Map labels to IDs
    train_df['label_id'] = train_df['Terms'].apply(lambda x: config.LABEL_TO_ID[x.split(',')[0].strip()])
    val_df['label_id'] = val_df['Terms'].apply(lambda x: config.LABEL_TO_ID[x.split(',')[0].strip()])
    test_df['label_id'] = test_df['Terms'].apply(lambda x: config.LABEL_TO_ID[x.split(',')[0].strip()])
    
    return train_df, val_df, test_df

def get_class_weights(train_df, device):
    """Calculate class weights for imbalanced data."""
    class_weights = compute_class_weight(
        'balanced',
        classes=np.unique(train_df['label_id']),
        y=train_df['label_id']
    )
    return torch.tensor(class_weights, dtype=torch.float).to(device)


def train_epoch(model, dataloader, optimizer, scheduler, loss_fn, device):
    """Train for one epoch with weighted loss."""
    model.train()
    total_loss = 0
    all_predictions = []
    all_labels = []
    
    for batch in tqdm(dataloader, desc="Training"):
        input_ids = batch['input_ids'].to(device)
        attention_mask = batch['attention_mask'].to(device)
        labels = batch['labels'].to(device)
        
        # Forward pass (don't use model's internal loss, use weighted loss)
        outputs = model(input_ids=input_ids, attention_mask=attention_mask)
        loss = loss_fn(outputs.logits, labels)
        
        # Backward pass
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()
        scheduler.step()
        
        total_loss += loss.item()
        
        preds = torch.argmax(outputs.logits, dim=1)
        all_predictions.extend(preds.cpu().numpy())
        all_labels.extend(labels.cpu().numpy())
    
    avg_loss = total_loss / len(dataloader)
    metrics = compute_multiclass_metrics(all_predictions, all_labels)
    
    return avg_loss, metrics

def evaluate(model, dataloader, device):
    """Evaluate the model."""
    model.eval()
    all_predictions = []
    all_labels = []
    
    with torch.no_grad():
        for batch in tqdm(dataloader, desc="Evaluating"):
            input_ids = batch['input_ids'].to(device)
            attention_mask = batch['attention_mask'].to(device)
            labels = batch['labels'].to(device)
            
            outputs = model(input_ids=input_ids, attention_mask=attention_mask)
            preds = torch.argmax(outputs.logits, dim=1)
            
            all_predictions.extend(preds.cpu().numpy())
            all_labels.extend(labels.cpu().numpy())
    
    metrics = compute_multiclass_metrics(all_predictions, all_labels)
    
    return metrics

def main():
    """Train Stage 2 multi-class classifier."""
    print("=" * 50)
    print("STAGE 2: Multi-Class Classification Training")
    print("=" * 50)
    
    # Setup device
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    print(f"Using device: {device}\n")
    
    # Load data
    print("Loading Stage 2 data...")
    train_df, val_df, test_df = load_stage2_data()
    print(f"Train: {len(train_df)}, Val: {len(val_df)}, Test: {len(test_df)}\n")
    
    # Calculate class weights
    class_weights = get_class_weights(train_df, device)
    print("Class weights:")
    for label, weight in zip(config.LABEL_TO_ID.keys(), class_weights):
        print(f"  {label:20} {weight:.2f}")
    print()
    
    # Load tokenizer and model
    print(f"Loading {config.MODEL_NAME}...")
    tokenizer = AutoTokenizer.from_pretrained(config.MODEL_NAME)
    model = AutoModelForSequenceClassification.from_pretrained(
        config.MODEL_NAME,
        num_labels=config.STAGE2_NUM_LABELS,
        use_safetensors=True
    )
    model = model.to(device)
    print(f"Model loaded with {model.num_parameters():,} parameters\n")
    
    # Create datasets and dataloaders
    train_dataset = MechanismDataset(train_df, tokenizer, label_column='label_id', max_length=config.MAX_LENGTH)
    val_dataset = MechanismDataset(val_df, tokenizer, label_column='label_id', max_length=config.MAX_LENGTH)
    test_dataset = MechanismDataset(test_df, tokenizer, label_column='label_id', max_length=config.MAX_LENGTH)
    
    train_loader = DataLoader(train_dataset, batch_size=config.BATCH_SIZE, shuffle=True)
    val_loader = DataLoader(val_dataset, batch_size=config.BATCH_SIZE)
    test_loader = DataLoader(test_dataset, batch_size=config.BATCH_SIZE)
    
    # Setup optimizer, scheduler, and weighted loss
    optimizer = AdamW(model.parameters(), lr=config.LEARNING_RATE)
    total_steps = len(train_loader) * config.STAGE2_EPOCHS
    warmup_steps = int(total_steps * config.STAGE2_WARMUP_RATIO)
    
    scheduler = get_linear_schedule_with_warmup(
        optimizer,
        num_warmup_steps=warmup_steps,
        num_training_steps=total_steps
    )
    
    loss_fn = torch.nn.CrossEntropyLoss(weight=class_weights)
    
    print(f"Training for {config.STAGE2_EPOCHS} epochs")
    print(f"Total steps: {total_steps}, Warmup steps: {warmup_steps}\n")
    
    # Training loop
    best_macro_f1 = 0
    
    for epoch in range(config.STAGE2_EPOCHS):
        print(f"\nEpoch {epoch + 1}/{config.STAGE2_EPOCHS}")
        print("-" * 50)
        
        train_loss, train_metrics = train_epoch(model, train_loader, optimizer, scheduler, loss_fn, device)
        val_metrics = evaluate(model, val_loader, device)
        
        print(f"Train Loss: {train_loss:.4f}, Acc: {train_metrics['accuracy']:.4f}")
        print(f"Val Acc: {val_metrics['accuracy']:.4f}")
        print(f"Val Macro F1: {val_metrics['macro_f1']:.4f} (all classes equal)")
        print(f"Val Weighted F1: {val_metrics['weighted_f1']:.4f} (by class size)")
        
        # Save best model based on macro F1
        if val_metrics['macro_f1'] > best_macro_f1:
            best_macro_f1 = val_metrics['macro_f1']
            torch.save(model.state_dict(), config.STAGE2_MODEL_PATH)
            print(f"✓ Saved best model (Macro F1: {best_macro_f1:.4f})")
    
    # Evaluate on test set
    print("\n" + "=" * 50)
    print("Evaluating on test set...")
    model.load_state_dict(torch.load(config.STAGE2_MODEL_PATH))
    test_metrics = evaluate(model, test_loader, device)
    
    print(f"\nTest Results:")
    print(f"Accuracy: {test_metrics['accuracy']:.4f}")
    print(f"Macro F1: {test_metrics['macro_f1']:.4f}")
    print(f"Weighted F1: {test_metrics['weighted_f1']:.4f}")
    print("\n✓ Stage 2 training complete!")

if __name__ == "__main__":
    main()