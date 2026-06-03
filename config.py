import os
import re

import numpy as np
import torch


SEED = 42
torch.manual_seed(SEED)
np.random.seed(SEED)

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
torch.set_num_threads(os.cpu_count())


DATA_PATH = "Dataset.csv"
CACHE_DIR = "t5_cache"
FEATURES_DIR = "saved_features"

TRAIN_FEATURES_PATH = os.path.join(FEATURES_DIR, "X_train.npy")
TEST_FEATURES_PATH = os.path.join(FEATURES_DIR, "X_test.npy")
TRAIN_LABELS_PATH = os.path.join(FEATURES_DIR, "y_train.npy")
TEST_LABELS_PATH = os.path.join(FEATURES_DIR, "y_test.npy")

MODEL_WEIGHTS_PATH = "los_model_weights.pt"
LGBM_MODEL_PATH = "lgbm_los_model.txt"
EXTRACTOR_CONFIG = "t5_extractor_config.json"


MAX_TOKENS = 512
MAX_NOTES = 36
HIDDEN_DIM = 512
LSTM_HIDDEN = 256
LSTM_OUT_DIM = LSTM_HIDDEN * 2
NUM_HEADS = 4
DROPOUT = 0.2

T5_BATCH_SIZE = 128
EPOCHS = 20
LR = 1e-3
WEIGHT_DECAY = 1e-2
BATCH_SIZE = 64
PATIENCE = 10
GRAD_CLIP = 1.0

T5_PROMPT = "predict patient length of stay: "

SECTION_LIST = [
    "hpi",
    "social_history",
    "chief_complaint",
    "vitals",
    "labs",
    "conditions",
    "medications",
    "admission_type",
    "admitted_time",
    "admitted_date",
]

SECTION_SET = set(SECTION_LIST)
SHORT_SECTIONS = {"admission_type", "admitted_time", "admitted_date"}

SECTION_PATTERN = re.compile(r"<([^>]+)>\s*(.*?)\s*(?=(<[^>]+>|$))", re.S)
CLOSE_TAG_PATTERN = re.compile(r"</s>")
