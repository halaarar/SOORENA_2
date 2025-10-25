import pandas as pd
import config

def main():
    """Merge predictions with PubMed metadata for Shiny app."""
    
    print("Loading data...")
    
    # Load predictions
    predictions_df = pd.read_csv('results/unused_predictions.csv')
    
    # Load original PubMed data
    pubmed_df = pd.read_csv(config.MODELING_DATASET_FILE)
    
    print(f"Predictions: {len(predictions_df):,}")
    print(f"PubMed data: {len(pubmed_df):,}\n")
    
    # Merge on PMID
    merged_df = predictions_df.merge(
        pubmed_df[['PMID', 'text']],
        on='PMID',
        how='left'
    )
    
    # Split text back into Title and Abstract (assume first sentence is title)
    merged_df['Title'] = merged_df['text'].str.split('. ', n=1).str[0]
    merged_df['Abstract'] = merged_df['text'].str.split('. ', n=1).str[1]
    
    # Rename columns to match Shiny app expectations
    shiny_df = pd.DataFrame({
        'PMID': merged_df['PMID'],
        'Title': merged_df['Title'],
        'Abstract': merged_df['Abstract'],
        'Autoregulatory Type': merged_df['mechanism_type'],
        'Term Probability': merged_df['stage2_confidence'],
        'Has Mechanism': merged_df['has_mechanism'],
        'Stage1 Confidence': merged_df['stage1_confidence']
    })
    
    # Save for Shiny app
    shiny_df.to_csv('shiny_app/data/predictions_for_app.csv', index=False)
    
    print(f"✓ Saved {len(shiny_df):,} papers to shiny_app/data/predictions_for_app.csv")
    print(f"\nBreakdown:")
    print(f"  With mechanism: {shiny_df['Has Mechanism'].sum():,}")
    print(f"  Without mechanism: {(~shiny_df['Has Mechanism']).sum():,}")

if __name__ == "__main__":
    import os
    os.makedirs('shiny_app/data', exist_ok=True)
    main()