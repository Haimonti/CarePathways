import json
import logging
import os

import lightgbm as lgb
import numpy as np
import optuna
import pandas as pd
import torch
import torch.nn as nn
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from sklearn.model_selection import train_test_split
from torch.utils.data import DataLoader, Dataset
from tqdm import tqdm

from los_predictor.config import (
    BATCH_SIZE,
    DATA_PATH,
    DROPOUT,
    EPOCHS,
    EXTRACTOR_CONFIG,
    FEATURES_DIR,
    GRAD_CLIP,
    HIDDEN_DIM,
    LGBM_MODEL_PATH,
    LR,
    LSTM_OUT_DIM,
    MAX_TOKENS,
    MODEL_WEIGHTS_PATH,
    NUM_HEADS,
    PATIENCE,
    SEED,
    T5_BATCH_SIZE,
    T5_PROMPT,
    TEST_FEATURES_PATH,
    TEST_LABELS_PATH,
    TRAIN_FEATURES_PATH,
    TRAIN_LABELS_PATH,
    WEIGHT_DECAY,
    device,
)
from los_predictor.models import (
    LOSModel,
    T5Extractor,
    extract_features,
    precompute_pooled,
)


VAL_SIZE = 0.1
N_TRIALS = 50
EARLY_STOP = 100
NUM_ROUNDS = 5000

optuna.logging.set_verbosity(optuna.logging.ERROR)
logging.getLogger("lightgbm").setLevel(logging.ERROR)


def sequential_split(df: pd.DataFrame, test_size: float = 0.2):
    df = df.copy()
    df["admittime"] = pd.to_datetime(df["admittime"])
    df = df.sort_values("admittime").reset_index(drop=True)

    cutoff_idx = int(len(df) * (1 - test_size))
    cutoff_time = df.iloc[cutoff_idx]["admittime"]

    train_df = df[df["admittime"] < cutoff_time].reset_index(drop=True)
    test_df = df[df["admittime"] >= cutoff_time].reset_index(drop=True)

    print(f"Test cutoff : {cutoff_time}")
    print(
        f"Train       : {len(train_df)}"
        f"  ({train_df['admittime'].min()} - {train_df['admittime'].max()})"
    )
    print(
        f"Test        : {len(test_df)}"
        f"  ({test_df['admittime'].min()} - {test_df['admittime'].max()})"
    )
    return train_df, test_df


class DOCDataset(Dataset):
    def __init__(self, pooled_docs: list, targets: np.ndarray):
        self.pooled_docs = pooled_docs
        self.targets = torch.tensor(targets, dtype=torch.float32)

    def __len__(self):
        return len(self.pooled_docs)

    def __getitem__(self, idx):
        return self.pooled_docs[idx], self.targets[idx]


def collate_fn(batch):
    pooled_batch = [item[0] for item in batch]
    targets = torch.stack([item[1] for item in batch])
    return pooled_batch, targets


def run_epoch(model, loader, optimizer, criterion, train: bool = True):
    model.train() if train else model.eval()
    total_loss, total_mae, n = 0.0, 0.0, 0
    ctx = torch.enable_grad() if train else torch.no_grad()

    with ctx:
        for pooled_batch, targets in loader:
            preds = torch.stack(
                [model(doc_pooled) for doc_pooled in pooled_batch]
            )
            loss = criterion(preds, targets)

            if train:
                optimizer.zero_grad()
                loss.backward()
                nn.utils.clip_grad_norm_(model.parameters(), GRAD_CLIP)
                optimizer.step()

            total_loss += loss.item() * len(targets)
            total_mae += (preds.detach() - targets).abs().sum().item()
            n += len(targets)

    return total_loss / n, total_mae / n


def make_lgbm_objective(X_tr, y_tr, X_val, y_val):
    def objective(trial):
        params = {
            "objective": "regression",
            "metric": "rmse",
            "boosting_type": "gbdt",
            "seed": SEED,
            "verbose": -1,
            "num_leaves": trial.suggest_int("num_leaves", 16, 256),
            "max_depth": trial.suggest_int("max_depth", -1, 16),
            "min_data_in_leaf": trial.suggest_int("min_data_in_leaf", 10, 200),
            "lambda_l1": trial.suggest_float("lambda_l1", 1e-8, 10.0, log=True),
            "lambda_l2": trial.suggest_float("lambda_l2", 1e-8, 10.0, log=True),
            "feature_fraction": trial.suggest_float("feature_fraction", 0.5, 1.0),
            "bagging_fraction": trial.suggest_float("bagging_fraction", 0.5, 1.0),
            "bagging_freq": trial.suggest_int("bagging_freq", 1, 10),
            "learning_rate": trial.suggest_float(
                "learning_rate", 0.005, 0.1, log=True
            ),
        }

        train_set = lgb.Dataset(X_tr, label=y_tr)
        val_set = lgb.Dataset(X_val, label=y_val, reference=train_set)

        model = lgb.train(
            params,
            train_set,
            num_boost_round=NUM_ROUNDS,
            valid_sets=[val_set],
            callbacks=[lgb.early_stopping(EARLY_STOP, verbose=False)],
        )

        preds = model.predict(X_val)
        return np.sqrt(mean_squared_error(y_val, preds))

    return objective


def train_bilstm(pooled_tr, y_tr, pooled_val, y_val_arr):
    los_model = LOSModel().to(device)

    optimizer = torch.optim.AdamW(
        [
            {"params": los_model.section_emb.parameters()},
            {"params": los_model.note_bilstm.parameters()},
            {"params": los_model.bilstm.parameters()},
            {"params": los_model.attention.parameters()},
            {"params": los_model.norm.parameters()},
            {"params": los_model.regressor.parameters(), "lr": LR * 2},
        ],
        lr=LR,
        weight_decay=WEIGHT_DECAY,
    )

    scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
        optimizer, mode="min", factor=0.5, patience=3
    )
    criterion = nn.HuberLoss(delta=1.0)

    train_loader = DataLoader(
        DOCDataset(pooled_tr, y_tr),
        batch_size=BATCH_SIZE,
        shuffle=False,
        collate_fn=collate_fn,
    )
    val_loader = DataLoader(
        DOCDataset(pooled_val, y_val_arr),
        batch_size=BATCH_SIZE,
        shuffle=False,
        collate_fn=collate_fn,
    )

    best_val_mae = float("inf")
    patience_left = PATIENCE
    best_state = None

    print(f"{'Epoch':>6}  {'Train Loss':>11}  {'Train MAE':>10}  {'Val MAE':>9}")
    print("-" * 46)

    for epoch in range(1, EPOCHS + 1):
        tr_loss, tr_mae = run_epoch(
            los_model, train_loader, optimizer, criterion, train=True
        )
        _, val_mae = run_epoch(
            los_model, val_loader, optimizer, criterion, train=False
        )
        scheduler.step(val_mae)
        print(
            f"{epoch:>6}  {tr_loss:>11.4f}  {tr_mae:>10.3f}  {val_mae:>9.3f}"
        )

        if val_mae < best_val_mae:
            best_val_mae = val_mae
            patience_left = PATIENCE
            best_state = {k: v.clone() for k, v in los_model.state_dict().items()}
        else:
            patience_left -= 1
            if patience_left == 0:
                print(
                    f"Early stop at epoch {epoch} "
                    f"(best val MAE: {best_val_mae:.3f})"
                )
                break

    los_model.load_state_dict(best_state)
    print(f"\nBest val MAE: {best_val_mae:.3f} days")
    return los_model


def save_weights(los_model, final_model):
    torch.save(
        {
            "model_state_dict": los_model.state_dict(),
            "config": {
                "hidden_dim": HIDDEN_DIM,
                "lstm_hidden": LSTM_OUT_DIM // 2,
                "num_heads": NUM_HEADS,
                "dropout": DROPOUT,
                "t5_prompt": T5_PROMPT,
            },
        },
        MODEL_WEIGHTS_PATH,
    )
    print(f"Saved BiLSTM weights      : {MODEL_WEIGHTS_PATH}")

    final_model.save_model(LGBM_MODEL_PATH)
    print(f"Saved LightGBM model      : {LGBM_MODEL_PATH}")

    with open(EXTRACTOR_CONFIG, "w") as f:
        json.dump(
            {
                "model_name": "t5-small",
                "max_tokens": MAX_TOKENS,
                "t5_batch_size": T5_BATCH_SIZE,
            },
            f,
            indent=2,
        )
    print(f"Saved T5 extractor config : {EXTRACTOR_CONFIG}")


def main():
    print(f"Device: {device}  |  Threads: {torch.get_num_threads()}\n")

    df = pd.read_csv(DATA_PATH)
    print(f"Dataset: {len(df)} rows")
    train_df, test_df = sequential_split(df)

    extractor = T5Extractor(model_name="t5-small", device=device)

    docs_train, y_train = extractor.extract(train_df)
    docs_test, y_test = extractor.extract(test_df)

    print(f"\nTrain: {len(docs_train)} docs   Test: {len(docs_test)} docs")
    print(f"LOS mean: {y_train.mean():.1f} d   std: {y_train.std():.1f} d\n")

    val_size = max(1, int(0.1 * len(docs_train)))
    train_size = len(docs_train) - val_size

    docs_tr = docs_train[:train_size]
    docs_val = docs_train[train_size:]
    y_tr = y_train[:train_size]
    y_val_arr = y_train[train_size:]

    pooled_tr = precompute_pooled(docs_tr, extractor, desc="Train embeddings")
    pooled_val = precompute_pooled(docs_val, extractor, desc="Val embeddings")

    print(
        f"\nTraining BiLSTM — "
        f"{train_size} train / {val_size} val  |  "
        f"{EPOCHS} epochs  |  batch {BATCH_SIZE}"
    )
    los_model = train_bilstm(pooled_tr, y_tr, pooled_val, y_val_arr)

    pooled_test = precompute_pooled(
        docs_test, extractor, desc="Test embeddings"
    )
    X_train = extract_features(
        los_model, pooled_tr + pooled_val, desc="Train"
    )
    X_test = extract_features(los_model, pooled_test, desc="Test")

    print(f"\nFeature shape — train: {X_train.shape}   test: {X_test.shape}")

    os.makedirs(FEATURES_DIR, exist_ok=True)
    np.save(TRAIN_FEATURES_PATH, X_train)
    np.save(TEST_FEATURES_PATH, X_test)
    np.save(TRAIN_LABELS_PATH, y_train)
    np.save(TEST_LABELS_PATH, y_test)
    print("Features saved.\n")

    X_tr_lgb, X_val_lgb, y_tr_lgb, y_val_lgb = train_test_split(
        X_train, y_train, test_size=VAL_SIZE, random_state=SEED
    )

    print(f"Tuning LightGBM — {N_TRIALS} Optuna trials")
    study = optuna.create_study(direction="minimize")
    study.optimize(
        make_lgbm_objective(X_tr_lgb, y_tr_lgb, X_val_lgb, y_val_lgb),
        n_trials=N_TRIALS,
        show_progress_bar=True,
    )

    print("\nBest hyperparameters:")
    for key, val in study.best_params.items():
        print(f"  {key}: {val}")
    print(f"Best validation RMSE: {study.best_value:.4f}\n")

    final_params = {
        "objective": "regression",
        "metric": "rmse",
        "boosting_type": "gbdt",
        "seed": SEED,
        "verbose": -1,
        **study.best_params,
    }

    full_train_set = lgb.Dataset(X_train, label=y_train)
    test_set = lgb.Dataset(X_test, label=y_test, reference=full_train_set)

    final_model = lgb.train(
        final_params,
        full_train_set,
        num_boost_round=NUM_ROUNDS,
        valid_sets=[test_set],
        callbacks=[lgb.early_stopping(EARLY_STOP, verbose=False)],
    )

    lgbm_preds = final_model.predict(X_test)
    mae = mean_absolute_error(y_test, lgbm_preds)
    rmse = np.sqrt(mean_squared_error(y_test, lgbm_preds))
    r2 = r2_score(y_test, lgbm_preds)

    print("Test-set performance:")
    print(f"  MAE  : {mae:.2f} days")
    print(f"  RMSE : {rmse:.2f} days")
    print(f"  R2   : {r2:.4f}\n")

    save_weights(los_model, final_model)


main()