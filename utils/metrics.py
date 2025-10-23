import torch
from torch.utils.data import Dataset
import pandas as pd

class MechanismDataset(Dataset):
    """
    Dataset for mechanism classification.
    Works for both binary (Stage 1) and multi-class (Stage 2).
    """
    def __init__(self, dataframe, tokenizer, label_column='has_mechanism', max_length=512):
        """
        Args:
            dataframe: pandas DataFrame with 'text' and label columns
            tokenizer: HuggingFace tokenizer
            label_column: column name containing labels
            max_length: maximum sequence length for tokenizer
        """
        self.texts = dataframe['text'].values
        self.tokenizer = tokenizer
        self.max_length = max_length
        
        # Convert labels to integers
        if label_column in dataframe.columns:
            self.labels = dataframe[label_column].astype(int).values
        else:
            self.labels = None
    
    def __len__(self):
        return len(self.texts)
    
    def __getitem__(self, idx):
        text = str(self.texts[idx])
        
        # Tokenize
        encoding = self.tokenizer(
            text,
            truncation=True,
            padding='max_length',
            max_length=self.max_length,
            return_tensors='pt'
        )
        
        item = {
            'input_ids': encoding['input_ids'].flatten(),
            'attention_mask': encoding['attention_mask'].flatten(),
        }
        
        # Add labels if available
        if self.labels is not None:
            item['labels'] = torch.tensor(self.labels[idx], dtype=torch.long)
        
        return item