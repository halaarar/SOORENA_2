import pandas as pd
import pyreadr

def main():
    """Merge predictions with PubMed and AutoregDB metadata for Shiny app."""
    
    print("Loading data...")
    
    # Load all data sources
    train_df = pd.read_csv('data/processed/train.csv')
    val_df = pd.read_csv('data/processed/val.csv')
    test_df = pd.read_csv('data/processed/test.csv')
    predictions_df = pd.read_csv('results/unused_predictions.csv')
    
    pubmed = pyreadr.read_r('data/raw/pubmed.rds')
    pubmed_df = list(pubmed.values())[0]
    
    autoreg = pyreadr.read_r('data/raw/autoregulatoryDB.rds')
    autoreg_df = list(autoreg.values())[0]
    
    # Prepare labeled data
    labeled_df = pd.concat([train_df, val_df, test_df], ignore_index=True)
    labeled_df['mechanism_type'] = labeled_df['Terms'].apply(lambda x: x.split(',')[0].strip())
    labeled_df['stage1_confidence'] = 1.0
    labeled_df['stage2_confidence'] = 1.0
    labeled_df['has_mechanism'] = True
    labeled_df['is_training_data'] = True
    
    labeled_df = labeled_df[['PMID', 'has_mechanism', 'stage1_confidence', 
                             'mechanism_type', 'stage2_confidence', 'is_training_data']]
    
    # Prepare predictions
    predictions_df['is_training_data'] = False
    
    # Combine all papers
    all_df = pd.concat([labeled_df, predictions_df], ignore_index=True)
    
    # Add PubMed metadata
    all_df = all_df.merge(pubmed_df, on='PMID', how='left')
    
    # Add protein info from AutoregDB
    autoreg_df['PMID'] = pd.to_numeric(
        autoreg_df['RX'].str.extract(r'PubMed=(\d+)')[0],
        errors='coerce'
    )
    
    autoreg_agg = autoreg_df.groupby('PMID', as_index=False).agg({
        'AC': lambda x: ', '.join(x.dropna().astype(str).unique()),
        'OS': 'first'
    })
    
    all_df = all_df.merge(autoreg_agg, on='PMID', how='left')
    
    # Create final columns
    all_df['Protein ID'] = all_df.apply(
        lambda row: f"{row['AC'].split(', ')[0]}_{row['PMID']}" if pd.notna(row['AC']) else f"NA_{row['PMID']}",
        axis=1
    )
    
    all_df['Has Mechanism'] = all_df['has_mechanism'].map({True: 'Yes', False: 'No'})
    all_df['Mechanism Probability'] = all_df['stage1_confidence']
    all_df['Source'] = all_df['is_training_data'].map({True: 'UniProt', False: 'Non-UniProt'})
    all_df['Autoregulatory Type'] = all_df['mechanism_type']
    all_df['Type Confidence'] = all_df['stage2_confidence']
    
    # Select final columns
    final_df = all_df[[
        'Protein ID', 'AC', 'OS', 'PMID', 'Title', 'Abstract', 'Journal', 'Authors',
        'Has Mechanism', 'Mechanism Probability', 'Source', 'Autoregulatory Type', 'Type Confidence'
    ]]
    
    # Save for Shiny app
    final_df.to_csv('shiny_app/data/predictions_for_app.csv', index=False)
    
    print(f"\n✓ Saved {len(final_df):,} papers to shiny_app/data/predictions_for_app.csv")
    print(f"\nSummary:")
    print(f"  UniProt (labeled): {(final_df['Source'] == 'UniProt').sum():,}")
    print(f"  Non-UniProt (predicted): {(final_df['Source'] == 'Non-UniProt').sum():,}")
    print(f"  With mechanisms: {(final_df['Has Mechanism'] == 'Yes').sum():,}")
    print(f"  Without mechanisms: {(final_df['Has Mechanism'] == 'No').sum():,}")

if __name__ == "__main__":
    import os
    os.makedirs('shiny_app/data', exist_ok=True)
    main()