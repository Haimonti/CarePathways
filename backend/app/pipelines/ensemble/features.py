"""Feature construction for the tabular LOS ensemble.

Ported verbatim from Isaac's `esembles/inference.py` (LOSPredictor.build_features)
so inference matches training exactly: 3 demographic codes, 12 token-count
features, and T5-small per-section mean-pooled embeddings (6 x 512) that are
standardized and PCA-reduced. The resulting 154 columns keep the names the
tree models were trained with, which XGBoost and LightGBM validate.

Do not "tidy" this module without re-running tools/check_ensemble_parity.py.
"""
from __future__ import annotations

import numpy as np
import pandas as pd
import torch
from gensim.utils import simple_preprocess

from .stop_words import STOP_WORDS

TEXT_COLUMNS = ['hpi', 'social_history', 'chief_complaint', 'labs', 'conditions', 'medications']
PHYSICAL_COLUMNS = ['gender', 'race', 'ethnicity']

GENDER_MAP = {'F': 1, 'M': 2}
RACE_MAP = {'white': 1, 'black': 2, 'asian': 3, 'native': 4, 'other': 5}
ETHNICITY_MAP = {'nonhispanic': 0, 'hispanic': 1}

T5_MODEL_NAME = "t5-small"
T5_MAX_LENGTH = 256
T5_BATCH_SIZE = 32


def tokenize_without_stopwords(text):
    tokens = simple_preprocess(text)
    return [token for token in tokens if token not in STOP_WORDS]


def embed_text_batch(texts, tokenizer, model, device):
    encoded = tokenizer(
        texts,
        padding=True,
        truncation=True,
        max_length=T5_MAX_LENGTH,
        return_tensors="pt"
    ).to(device)
    with torch.no_grad():
        outputs = model(**encoded)
    hidden_states = outputs.last_hidden_state
    attention_mask = encoded["attention_mask"].unsqueeze(-1).float()
    summed = (hidden_states * attention_mask).sum(dim=1)
    counts = attention_mask.sum(dim=1).clamp(min=1e-9)
    return (summed / counts).cpu().numpy()


def embed_text_series(text_series, tokenizer, model, device):
    texts = text_series.fillna("").astype(str).tolist()
    embeddings = []
    for start in range(0, len(texts), T5_BATCH_SIZE):
        batch = texts[start:start + T5_BATCH_SIZE]
        embeddings.append(embed_text_batch(batch, tokenizer, model, device))
    return np.concatenate(embeddings, axis=0)


def build_features(raw_df, tokenizer, model, device, text_embedding_scaler, text_pca):
    df = raw_df.copy()
    df[TEXT_COLUMNS] = df[TEXT_COLUMNS].fillna('')
    df['gender'] = df['gender'].map(GENDER_MAP).fillna(0).astype(int)
    df['race'] = df['race'].astype(str).str.lower().map(RACE_MAP).fillna(0).astype(int)
    df['ethnicity'] = df['ethnicity'].astype(str).str.lower().map(ETHNICITY_MAP).fillna(0).astype(int)

    tokenized = {col: df[col].apply(tokenize_without_stopwords) for col in TEXT_COLUMNS}

    count_blocks = []
    count_feature_names = []
    for col in TEXT_COLUMNS:
        lengths = tokenized[col].apply(len).to_numpy().reshape(-1, 1)
        unique_counts = tokenized[col].apply(lambda tokens: len(set(tokens))).to_numpy().reshape(-1, 1)
        count_blocks.append(np.concatenate([lengths, unique_counts], axis=1))
        count_feature_names.append(f"{col}_token_count")
        count_feature_names.append(f"{col}_unique_token_count")
    count_features = np.concatenate(count_blocks, axis=1)

    text_embeddings = []
    for col in TEXT_COLUMNS:
        text_embeddings.append(embed_text_series(df[col], tokenizer, model, device))
    raw_text_embeddings = np.concatenate(text_embeddings, axis=1)

    scaled_embeddings = text_embedding_scaler.transform(raw_text_embeddings)
    reduced_embeddings = text_pca.transform(scaled_embeddings)
    embedding_feature_names = [f"text_pca_{j}" for j in range(reduced_embeddings.shape[1])]

    physical_features = df[PHYSICAL_COLUMNS].astype(float).to_numpy()

    feature_col_names = PHYSICAL_COLUMNS + count_feature_names + embedding_feature_names
    X = pd.DataFrame(
        np.concatenate([physical_features, count_features, reduced_embeddings], axis=1),
        columns=feature_col_names
    )
    return X
