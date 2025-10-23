# Paths
DATA_DIR = "data"
RAW_DATA_DIR = f"{DATA_DIR}/raw"
PROCESSED_DATA_DIR = f"{DATA_DIR}/processed"
MODEL_DIR = "models"

# Raw data files
PUBMED_FILE = f"{RAW_DATA_DIR}/pubmed.rds"
AUTOREG_FILE = f"{RAW_DATA_DIR}/autoregulatoryDB.rds"

# Processed data files
MODELING_DATASET_FILE = f"{PROCESSED_DATA_DIR}/modeling_dataset.csv"
TRAIN_FILE = f"{PROCESSED_DATA_DIR}/train.csv"
VAL_FILE = f"{PROCESSED_DATA_DIR}/val.csv"
TEST_FILE = f"{PROCESSED_DATA_DIR}/test.csv"


# Model settings
MODEL_NAME = "microsoft/BiomedNLP-PubMedBERT-base-uncased-abstract-fulltext"
MAX_LENGTH = 512

# Stage 1 (binary)
STAGE1_MODEL_PATH = f"{MODEL_DIR}/stage1_best.pt"
STAGE1_NUM_LABELS = 2

# Stage 2 (7-class)
STAGE2_MODEL_PATH = f"{MODEL_DIR}/stage2_best.pt"
STAGE2_NUM_LABELS = 7
STAGE2_EPOCHS = 4
STAGE2_WARMUP_RATIO = 0.1
STAGE2_MIN_EXAMPLES = 35  # Minimum examples per class to keep

# Label mappings
LABEL_TO_ID = {
    'autophosphorylation': 0,
    'autoregulation': 1,
    'autocatalytic': 2,
    'autoinhibition': 3,
    'autoubiquitination': 4,
    'autolysis': 5,
    'autoinducer': 6
}

ID_TO_LABEL = {v: k for k, v in LABEL_TO_ID.items()}

# Random seed for reproducibility
RANDOM_SEED = 42