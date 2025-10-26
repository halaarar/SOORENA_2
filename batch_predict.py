import pandas as pd
from tqdm import tqdm
from predict import MechanismPredictor
import config
import os

def main():
    """Predict on papers NOT in train/val/test."""
    
    # Create results directory
    os.makedirs('results', exist_ok=True)
    
    # Load all data
    full_df = pd.read_csv(config.MODELING_DATASET_FILE)
    train_df = pd.read_csv(config.TRAIN_FILE)
    val_df = pd.read_csv(config.VAL_FILE)
    test_df = pd.read_csv(config.TEST_FILE)
    
    # Get PMIDs used in training
    used_pmids = set(train_df['PMID']) | set(val_df['PMID']) | set(test_df['PMID'])
    
    # Get unused papers only
    unused_df = full_df[~full_df['PMID'].isin(used_pmids)].copy()
    print(f"Total papers: {len(full_df):,}")
    print(f"Used in train/val/test: {len(used_pmids):,}")
    print(f"Unused papers to predict: {len(unused_df):,}\n")
    
    # Check for existing checkpoint
    checkpoint_file = 'results/predictions_checkpoint.csv'
    if os.path.exists(checkpoint_file):
        print(f"Found checkpoint file. Loading...")
        existing_df = pd.read_csv(checkpoint_file)
        already_predicted = set(existing_df['PMID'])
        unused_df = unused_df[~unused_df['PMID'].isin(already_predicted)]
        print(f"Already predicted: {len(already_predicted):,}")
        print(f"Remaining: {len(unused_df):,}\n")
        results = existing_df.to_dict('records')
    else:
        results = []
    
    # Initialize predictor
    predictor = MechanismPredictor()
    
    # Predict with checkpoint every 10K papers
    checkpoint_interval = 10000
    
    for idx, row in tqdm(unused_df.iterrows(), total=len(unused_df), desc="Predicting"):
        pred = predictor.predict(row['text'], '')
        results.append({
            'PMID': row['PMID'],
            'has_mechanism': pred['has_mechanism'],
            'stage1_confidence': pred['stage1_confidence'],
            'mechanism_type': pred['mechanism_type'] if pred['mechanism_type'] else 'none',
            'stage2_confidence': pred['stage2_confidence'] if pred['stage2_confidence'] else 0.0
        })
        
        # Save checkpoint
        if len(results) % checkpoint_interval == 0:
            pd.DataFrame(results).to_csv(checkpoint_file, index=False)
            print(f"\n✓ Checkpoint saved at {len(results):,} predictions")
    
    # Save final predictions
    results_df = pd.DataFrame(results)
    results_df.to_csv('results/unused_predictions.csv', index=False)
    
    # Remove checkpoint
    if os.path.exists(checkpoint_file):
        os.remove(checkpoint_file)
    
    print(f"\n✓ Saved predictions to results/unused_predictions.csv")
    print(f"  Papers with mechanisms: {results_df['has_mechanism'].sum():,}")
    print(f"  Papers without mechanisms: {(~results_df['has_mechanism']).sum():,}")
    
    # Show mechanism type breakdown
    if results_df['has_mechanism'].sum() > 0:
        print("\nMechanism type distribution:")
        print(results_df[results_df['has_mechanism']]['mechanism_type'].value_counts())

if __name__ == "__main__":
    main()