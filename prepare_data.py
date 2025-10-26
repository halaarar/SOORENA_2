import pandas as pd
import numpy as np
import pyreadr
import re
from html import unescape
from sklearn.model_selection import train_test_split
import config

def load_data():
    """Load raw PubMed and Autoregulatory datasets"""
    pubmed = pyreadr.read_r(config.PUBMED_FILE)
    pubmed_df = list(pubmed.values())[0]
    
    autoreg = pyreadr.read_r(config.AUTOREG_FILE)
    autoreg_df = list(autoreg.values())[0]
    
    return pubmed_df, autoreg_df


def clean_text(text):
    """Clean text data"""
    if pd.isna(text):
        return ""
    
    text = str(text)
    text = unescape(text)               # Fix HTML entities
    text = re.sub(r'http\S+', '', text) # Remove URLs
    text = re.sub(r'\S+@\S+', '', text) # Remove emails
    text = re.sub(r'\s+', ' ', text)    # Normalize whitespace
    
    return text.strip()

def merge_terms(row):
    """Merge terms from three columns into one"""
    cols = ['Term_in_RP', 'Term_in_RT', 'Term_in_RC']
    terms = []
    for col in cols:
        val = row[col]
        if pd.notna(val):
            terms.extend([t.strip() for t in str(val).split(',') if t.strip()])
    return ', '.join(sorted(set(terms))) if terms else ''


def process_autoreg(autoreg_df):
    """Process autoregulatory dataset"""
    # Extract PMID from RX column
    autoreg_df['PMID'] = autoreg_df['RX'].str.extract(r'PubMed=(\d+)', expand=False)
    autoreg_df['PMID'] = pd.to_numeric(autoreg_df['PMID'], errors='coerce')
    
    # Merge terms from three columns
    autoreg_df['Terms'] = autoreg_df.apply(merge_terms, axis=1)
    
    # Drop rows with missing PMID
    autoreg_df = autoreg_df.dropna(subset=['PMID'])
    
    # Aggregate by PMID - combine all terms for same paper
    autoreg_aggregated = (
        autoreg_df.groupby('PMID', as_index=False)
        .agg({
            'Terms': lambda x: ', '.join(sorted(set(
                term.strip()
                for terms_str in x
                for term in str(terms_str).split(',')
                if term.strip()
            )))
        })
    )
    
    autoreg_aggregated['has_mechanism'] = autoreg_aggregated['Terms'] != ''
    
    return autoreg_aggregated


def merge_datasets(pubmed_df, autoreg_aggregated):
    """Merge PubMed with autoregulatory labels"""
    # Ensure same data type
    pubmed_df['PMID'] = pubmed_df['PMID'].astype('int64')
    autoreg_aggregated['PMID'] = autoreg_aggregated['PMID'].astype('int64')
    
    # Left join - keep all PubMed papers
    merged_df = pubmed_df.merge(
        autoreg_aggregated[['PMID', 'Terms', 'has_mechanism']],
        on='PMID',
        how='left'
    )
    
    # Fill missing values
    merged_df['Terms'] = merged_df['Terms'].fillna('')
    merged_df['has_mechanism'] = merged_df['has_mechanism'].fillna(False)
    
    # Drop papers without abstracts
    merged_df = merged_df[merged_df['Abstract'].notna()]
    
    # Create combined text column
    merged_df['text'] = (merged_df['Title'].fillna('') + '. ' + 
                         merged_df['Abstract'].fillna(''))
    merged_df['text'] = merged_df['text'].apply(clean_text)
    
    return merged_df

def normalize_terms(terms):
    """Normalize spelling variations to common forms"""
    if pd.isna(terms) or terms == '':
        return terms
    
    normalization_rules = {
        'autoregulatory': 'autoregulation',
        'autoinhibitory': 'autoinhibition',
        'autocatalysis': 'autocatalytic',
        'autoinduction': 'autoinducer'
    }
    
    normalized = []
    for term in terms.split(','):
        term = term.strip().lower()
        normalized.append(normalization_rules.get(term, term))
    
    return ', '.join(sorted(set(normalized)))

def filter_terms(merged_df):
    """Filter to keep only terms with enough examples"""
    # Normalize
    merged_df['Terms_normalized'] = merged_df['Terms'].apply(normalize_terms)
    
    # Count normalized terms
    all_terms = []
    for terms_str in merged_df[merged_df['has_mechanism']]['Terms_normalized']:
        if pd.notna(terms_str) and terms_str != '':
            all_terms.extend([t.strip() for t in terms_str.split(',')])
    
    term_counts = pd.Series(all_terms).value_counts()
    keep_terms = term_counts[term_counts >= config.STAGE2_MIN_EXAMPLES].index.tolist()
    
    # Filter to kept terms only
    def filter_to_kept(terms_str):
        if pd.isna(terms_str) or terms_str == '':
            return ''
        terms = [t.strip() for t in terms_str.split(',')]
        kept = [t for t in terms if t in keep_terms]
        return ', '.join(sorted(set(kept)))
    
    merged_df['Terms'] = merged_df['Terms_normalized'].apply(filter_to_kept)
    merged_df['has_mechanism'] = (merged_df['Terms'] != '')
    
    # Remove papers that only had rare terms
    merged_df = merged_df[
        (~merged_df['has_mechanism']) | 
        (merged_df['Terms'] != '')
    ]
    
    return merged_df[['PMID', 'text', 'Terms', 'has_mechanism']]


def create_splits(df):
    """Create stratified train/val/test splits"""
    # Get labeled papers only
    labeled_df = df[df['has_mechanism']].copy()
    
    # Create label column (use first term for multi-label)
    labeled_df['label'] = labeled_df['Terms'].apply(
        lambda x: x.split(',')[0].strip() if x else ''
    )
    
    # Split: 70% train, 30% temp
    train_df, temp_df = train_test_split(
        labeled_df,
        test_size=0.3,
        stratify=labeled_df['label'],
        random_state=config.RANDOM_SEED
    )
    
    # Split temp: 15% val, 15% test
    val_df, test_df = train_test_split(
        temp_df,
        test_size=0.5,
        stratify=temp_df['label'],
        random_state=config.RANDOM_SEED
    )
    
    return train_df, val_df, test_df


def main():
    """Run the full data preparation pipeline"""
    print("Loading raw data...")
    pubmed_df, autoreg_df = load_data()
    
    print("Processing autoregulatory data...")
    autoreg_aggregated = process_autoreg(autoreg_df)
    
    print("Merging datasets...")
    merged_df = merge_datasets(pubmed_df, autoreg_aggregated)
    
    print("Normalizing and filtering terms...")
    final_df = filter_terms(merged_df)
    
    # Save full cleaned dataset
    print(f"Saving full dataset to {config.MODELING_DATASET_FILE}...")
    final_df.to_csv(config.MODELING_DATASET_FILE, index=False)
    print(f"  Total papers: {len(final_df):,}, Labeled: {final_df['has_mechanism'].sum():,}")
    
    print("Creating train/val/test splits...")
    train_df, val_df, test_df = create_splits(final_df)
    
    # Save splits
    train_df.to_csv(config.TRAIN_FILE, index=False)
    val_df.to_csv(config.VAL_FILE, index=False)
    test_df.to_csv(config.TEST_FILE, index=False)
    
    print(f"\n✓ Data preparation complete!")
    print(f"  Train: {len(train_df)}, Val: {len(val_df)}, Test: {len(test_df)}")

if __name__ == "__main__":
    main()