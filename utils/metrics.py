import numpy as np
from sklearn.metrics import (
    accuracy_score, 
    precision_recall_fscore_support,
    confusion_matrix,
    classification_report
)

def compute_binary_metrics(predictions, labels):
    """
    Compute metrics for binary classification (Stage 1).
    
    Args:
        predictions: numpy array of predicted labels
        labels: numpy array of true labels
        
    Returns:
        dict with accuracy, precision, recall, f1
    """
    accuracy = accuracy_score(labels, predictions)
    precision, recall, f1, _ = precision_recall_fscore_support(
        labels, predictions, average='binary', zero_division=0
    )
    
    return {
        'accuracy': accuracy,
        'precision': precision,
        'recall': recall,
        'f1': f1
    }

def compute_multiclass_metrics(predictions, labels):
    """
    Compute metrics for multi-class classification (Stage 2).
    
    Args:
        predictions: numpy array of predicted labels
        labels: numpy array of true labels
        
    Returns:
        dict with accuracy, macro/weighted metrics
    """
    accuracy = accuracy_score(labels, predictions)
    
    # Macro metrics (treat all classes equally)
    macro_p, macro_r, macro_f1, _ = precision_recall_fscore_support(
        labels, predictions, average='macro', zero_division=0
    )
    
    # Weighted metrics (account for class imbalance)
    weighted_p, weighted_r, weighted_f1, _ = precision_recall_fscore_support(
        labels, predictions, average='weighted', zero_division=0
    )
    
    return {
        'accuracy': accuracy,
        'macro_precision': macro_p,
        'macro_recall': macro_r,
        'macro_f1': macro_f1,
        'weighted_precision': weighted_p,
        'weighted_recall': weighted_r,
        'weighted_f1': weighted_f1
    }

def get_confusion_matrix(predictions, labels):
    """Get confusion matrix."""
    return confusion_matrix(labels, predictions)

def get_classification_report(predictions, labels, label_names=None):
    """Get detailed classification report."""
    return classification_report(
        labels, predictions, 
        target_names=label_names,
        zero_division=0,
        digits=4
    )