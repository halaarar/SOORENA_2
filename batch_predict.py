import pandas as pd
from tqdm import tqdm
from predict import MechanismPredictor
import config

def main():
    """Predict on papers NOT in train/val/test."""
    
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
    
    # Initialize predictor
    predictor = MechanismPredictor()
    
    # Predict
    results = []
    for idx, row in tqdm(unused_df.iterrows(), total=len(unused_df), desc="Predicting"):
        pred = predictor.predict(row['text'], '')
        results.append({
            'PMID': row['PMID'],
            'has_mechanism': pred['has_mechanism'],
            'stage1_confidence': pred['stage1_confidence'],
            'mechanism_type': pred['mechanism_type'] if pred['mechanism_type'] else 'none',
            'stage2_confidence': pred['stage2_confidence'] if pred['stage2_confidence'] else 0.0
        })
    
    # Save predictions
    results_df = pd.DataFrame(results)
    results_df.to_csv('results/unused_predictions.csv', index=False)
    
    print(f"\n✓ Saved predictions to results/unused_predictions.csv")
    print(f"  Papers with mechanisms: {results_df['has_mechanism'].sum():,}")
    print(f"  Papers without mechanisms: {(~results_df['has_mechanism']).sum():,}")
    
    # Show mechanism type breakdown
    if results_df['has_mechanism'].sum() > 0:
        print("\nMechanism type distribution:")
        print(results_df[results_df['has_mechanism']]['mechanism_type'].value_counts())

if __name__ == "__main__":
    main()