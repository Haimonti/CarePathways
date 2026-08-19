"""
Pure constants for the manual clinical entry pipeline (T5 + BiLSTM + LightGBM).
No file paths here — those come from ManualPipelineSettings in settings.py.
"""
from __future__ import annotations

import re

SEED = 42

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

DEFAULT_T5_PROMPT = "predict patient length of stay: "

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
