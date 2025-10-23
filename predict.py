import torch
from transformers import AutoTokenizer, AutoModelForSequenceClassification
import config

class MechanismPredictor:
    """Predict mechanism types for new papers."""
    
    def __init__(self):
        self.device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
        
        # Load tokenizer
        self.tokenizer = AutoTokenizer.from_pretrained(config.MODEL_NAME)
        
        # Load Stage 1 model (binary)
        self.model_stage1 = AutoModelForSequenceClassification.from_pretrained(
            config.MODEL_NAME,
            num_labels=config.STAGE1_NUM_LABELS,
            use_safetensors=True
        )
        self.model_stage1.load_state_dict(torch.load(config.STAGE1_MODEL_PATH, map_location=self.device))
        self.model_stage1 = self.model_stage1.to(self.device)
        self.model_stage1.eval()
        
        # Load Stage 2 model (7-class)
        self.model_stage2 = AutoModelForSequenceClassification.from_pretrained(
            config.MODEL_NAME,
            num_labels=config.STAGE2_NUM_LABELS,
            use_safetensors=True
        )
        self.model_stage2.load_state_dict(torch.load(config.STAGE2_MODEL_PATH, map_location=self.device))
        self.model_stage2 = self.model_stage2.to(self.device)
        self.model_stage2.eval()
        
        print(f"✓ Models loaded on {self.device}")
    
    def predict(self, title, abstract):
        """
        Predict mechanism type for a paper.
        
        Args:
            title: Paper title
            abstract: Paper abstract
            
        Returns:
            dict with predictions and confidence scores
        """
        # Combine title and abstract
        text = f"{title}. {abstract}"
        
        # Tokenize
        inputs = self.tokenizer(
            text,
            truncation=True,
            padding='max_length',
            max_length=config.MAX_LENGTH,
            return_tensors='pt'
        )
        inputs = {k: v.to(self.device) for k, v in inputs.items()}
        
        # Stage 1: Check if has mechanism
        with torch.no_grad():
            outputs1 = self.model_stage1(**inputs)
            probs1 = torch.softmax(outputs1.logits, dim=1)
            has_mechanism = torch.argmax(probs1, dim=1).item()
            confidence1 = probs1[0, has_mechanism].item()
        
        result = {
            'has_mechanism': bool(has_mechanism),
            'stage1_confidence': confidence1
        }
        
        # Stage 2: If has mechanism, classify type
        if has_mechanism:
            with torch.no_grad():
                outputs2 = self.model_stage2(**inputs)
                probs2 = torch.softmax(outputs2.logits, dim=1)
                mechanism_id = torch.argmax(probs2, dim=1).item()
                confidence2 = probs2[0, mechanism_id].item()
            
            result['mechanism_type'] = config.ID_TO_LABEL[mechanism_id]
            result['stage2_confidence'] = confidence2
        else:
            result['mechanism_type'] = None
            result['stage2_confidence'] = None
        
        return result

def main():
    """Example usage."""
    predictor = MechanismPredictor()
    
    # Example paper
    title = "Regulation of activity and localization of the WNK1 protein kinase"
    abstract = """Mutations within the WNK1 gene cause Gordon's hypertension syndrome. 
    Little is known about how WNK1 is regulated. We demonstrate that WNK1 is rapidly activated 
    and phosphorylated at its activation loop following hyperosmotic stress."""
    
    print("\nExample prediction:")
    print(f"Title: {title}")
    print(f"Abstract: {abstract[:100]}...")
    
    result = predictor.predict(title, abstract)
    
    print(f"\nResults:")
    print(f"Has mechanism: {result['has_mechanism']} (confidence: {result['stage1_confidence']:.2%})")
    if result['has_mechanism']:
        print(f"Mechanism type: {result['mechanism_type']} (confidence: {result['stage2_confidence']:.2%})")

if __name__ == "__main__":
    main()