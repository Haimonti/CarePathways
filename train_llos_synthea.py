"""
Deep Patient – LOS Regression on the LLOS Cohort (Synthea)
==========================================================
The notebook output contains only LLOS encounters (LOS ≥ mean + 2σ).
The external test set is temporal (earlier admissions → train, later → test).

This script runs two regression ablations:
  with_notes      -- structured features + clinical-note embeddings
  structured_only -- structured features only

Clinical-note features = cc_embedding_* + hpi_embedding_* columns.
All other features are structured (demographics, dx codes, px codes, meds).
"""

import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader, TensorDataset
import pandas as pd
import numpy as np
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from sklearn.preprocessing import StandardScaler
import warnings, textwrap, json
from config_utils import load_data_paths
warnings.filterwarnings('ignore')

# -----------------------------
# Denoising Autoencoder Module
# -----------------------------
class DenoisingAutoencoder(nn.Module):
    def __init__(self, input_dim, hidden_dim, noise_std=0.1, dropout=0.2):
        super().__init__()
        self.noise_std = noise_std
        self.encoder = nn.Sequential(
            nn.Linear(input_dim, hidden_dim),
            nn.BatchNorm1d(hidden_dim),
            nn.ReLU(),
            nn.Dropout(dropout)
        )
        self.decoder = nn.Sequential(
            nn.Linear(hidden_dim, input_dim),
        )

    def forward(self, x):
        if self.training:
            noise = torch.randn_like(x) * self.noise_std
            x = x + noise
        z = self.encoder(x)
        x_hat = self.decoder(z)
        return x_hat, z


# -----------------------------
# DeepPatient Stack
# -----------------------------
class DeepPatient(nn.Module):
    def __init__(self, layer_dims, noise_std=0.1, dropout=0.2):
        super().__init__()
        self.autoencoders = nn.ModuleList([
            DenoisingAutoencoder(layer_dims[i], layer_dims[i+1], noise_std, dropout)
            for i in range(len(layer_dims)-1)
        ])

    def forward(self, x):
        for ae in self.autoencoders:
            _, x = ae(x)
        return x

    def pretrain(self, data, epochs=50, lr=1e-3, batch_size=32, verbose=True):
        """Layer-wise pretraining of autoencoders."""
        x = data
        for i, ae in enumerate(self.autoencoders):
            if verbose:
                print(f"\nPretraining layer {i+1}/{len(self.autoencoders)}")
            
            dataset = DataLoader(TensorDataset(x), batch_size=batch_size, shuffle=True)
            optimizer = optim.Adam(ae.parameters(), lr=lr, weight_decay=1e-5)
            loss_fn = nn.MSELoss()

            ae.train()
            for epoch in range(epochs):
                epoch_loss = 0
                for (batch,) in dataset:
                    optimizer.zero_grad()
                    recon, _ = ae(batch)
                    loss = loss_fn(recon, batch)
                    loss.backward()
                    optimizer.step()
                    epoch_loss += loss.item()
                
                if verbose and (epoch + 1) % 10 == 0:
                    print(f"  Epoch {epoch+1}/{epochs}: loss={epoch_loss/len(dataset):.4f}")

            # Transform data for next layer
            ae.eval()
            with torch.no_grad():
                _, x = ae(x)
            ae.train()
        
        return x


# -----------------------------
# Regression head
# -----------------------------
class RegressionHead(nn.Module):
    def __init__(self, embedding_dim, hidden_dim=64, dropout=0.3):
        super().__init__()
        self.head = nn.Sequential(
            nn.Linear(embedding_dim, hidden_dim),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(hidden_dim, 1),
        )

    def forward(self, z):
        return self.head(z).squeeze(-1)


# ----------------------------------------------------------------
# Training helpers
# ----------------------------------------------------------------
def _make_loader(X, y, batch_size, shuffle=True):
    return DataLoader(TensorDataset(X, y), batch_size=batch_size, shuffle=shuffle)


def split_train_validation(train_df, val_fraction=0.2):
    """Use the latest portion of the training split for validation."""
    if len(train_df) < 5:
        raise ValueError("Need at least 5 training rows to create a disjoint validation split.")

    ordered = train_df.copy()
    if "admittime" in ordered.columns:
        ordered["admittime"] = pd.to_datetime(ordered["admittime"], utc=True)
        ordered = ordered.sort_values("admittime").reset_index(drop=True)
    else:
        ordered = ordered.reset_index(drop=True)

    val_size = max(1, int(round(len(ordered) * val_fraction)))
    if val_size >= len(ordered):
        val_size = len(ordered) - 1

    train_part = ordered.iloc[:-val_size].copy()
    val_part = ordered.iloc[-val_size:].copy()
    return train_part, val_part


def train_regression(model, head, X_train, y_train, X_val, y_val,
                     epochs=100, lr=1e-3, batch_size=16,
                     patience=20, verbose=True):
    """Fine-tune for LOS regression (MSE loss, early-stop on val MSE)."""
    loader = _make_loader(X_train, y_train, batch_size)
    params = list(model.parameters()) + list(head.parameters())
    optimizer = optim.Adam(params, lr=lr, weight_decay=1e-4)
    criterion = nn.MSELoss()
    scheduler = optim.lr_scheduler.ReduceLROnPlateau(optimizer, mode='min', patience=10, factor=0.5)

    best_mse, best_state, wait = float('inf'), None, 0

    for epoch in range(epochs):
        model.train(); head.train()
        epoch_loss = 0
        for Xb, yb in loader:
            optimizer.zero_grad()
            loss = criterion(head(model(Xb)), yb)
            loss.backward(); optimizer.step()
            epoch_loss += loss.item()

        model.eval(); head.eval()
        with torch.no_grad():
            val_preds = head(model(X_val)).numpy()
        val_mse = mean_squared_error(y_val.numpy(), val_preds)
        scheduler.step(val_mse)

        if val_mse < best_mse:
            best_mse = val_mse
            best_state = {'m': {k: v.clone() for k, v in model.state_dict().items()},
                          'h': {k: v.clone() for k, v in head.state_dict().items()}}
            wait = 0
        else:
            wait += 1

        if verbose and (epoch + 1) % 10 == 0:
            print(f"  Epoch {epoch+1}/{epochs}: loss={epoch_loss/len(loader):.4f}  val_MSE={val_mse:.4f}")
        if wait >= patience:
            if verbose: print(f"  Early stopping at epoch {epoch+1}")
            break

    if best_state:
        model.load_state_dict(best_state['m']); head.load_state_dict(best_state['h'])
    return model, head


# ----------------------------------------------------------------
# Evaluation
# ----------------------------------------------------------------
def evaluate_regression(model, head, X, y):
    model.eval(); head.eval()
    with torch.no_grad():
        preds = head(model(X)).numpy()
    y_np = y.numpy()
    mse_val = mean_squared_error(y_np, preds)
    return {
        'mse':  mse_val,
        'rmse': np.sqrt(mse_val),
        'mae':  mean_absolute_error(y_np, preds),
        'r2':   r2_score(y_np, preds) if len(y_np) > 1 else float('nan'),
    }, preds


# ----------------------------------------------------------------
# Experiment runners
# ----------------------------------------------------------------
def _build_and_pretrain(X_train_t, layer_dims, noise_std=0.05, dropout=0.1,
                        pretrain_epochs=30, lr=1e-3, batch_size=16, verbose=True):
    """Create a DeepPatient encoder and pre-train it."""
    model = DeepPatient(layer_dims, noise_std=noise_std, dropout=dropout)
    model.pretrain(X_train_t, epochs=pretrain_epochs, lr=lr,
                   batch_size=batch_size, verbose=verbose)
    return model


def run_regression_experiment(tag, X_train_t, y_train_t, X_val_t, y_val_t, X_test_t, y_test_t,
                              layer_dims, verbose=True):
    """Full regression pipeline: pretrain → fine-tune → evaluate."""
    print(f"\n{'='*60}")
    print(f"Experiment {tag}: REGRESSION (predict LOS in days)")
    print(f"  Features: {X_train_t.shape[1]}")
    print(f"{'='*60}")

    model = _build_and_pretrain(X_train_t, layer_dims, verbose=verbose)
    head = RegressionHead(layer_dims[-1], hidden_dim=16, dropout=0.2)

    model, head = train_regression(
        model, head, X_train_t, y_train_t,
        X_val_t, y_val_t,
        epochs=100, lr=5e-4, batch_size=8,
        patience=20, verbose=verbose,
    )

    train_res, _ = evaluate_regression(model, head, X_train_t, y_train_t)
    val_res, _ = evaluate_regression(model, head, X_val_t, y_val_t)
    test_res, preds = evaluate_regression(model, head, X_test_t, y_test_t)

    print(f"\n  Train  →  MSE={train_res['mse']:.4f}  RMSE={train_res['rmse']:.4f}  R²={train_res['r2']:.4f}")
    print(f"  Valid  →  MSE={val_res['mse']:.4f}  RMSE={val_res['rmse']:.4f}  R²={val_res['r2']:.4f}")
    print(f"  Test   →  MSE={test_res['mse']:.4f}  RMSE={test_res['rmse']:.4f}  R²={test_res['r2']:.4f}")

    return {'train': train_res, 'val': val_res, 'test': test_res, 'preds': preds,
            'model': model, 'head': head}


# ----------------------------------------------------------------
# Main – run LOS regression experiments on the LLOS cohort
# ----------------------------------------------------------------
def main():
    print("=" * 60)
    print("Deep Patient – LOS Regression on LLOS Cohort (Synthea)")
    print("=" * 60)

    # ------ load data (temporal split, LLOS cohort only) ---------------
    data_path = load_data_paths().processed_dir

    train_df = pd.read_csv(data_path / "train_set.csv")
    test_df = pd.read_csv(data_path / "test_set.csv")
    with (data_path / "feature_columns.txt").open(encoding="utf-8") as f:
        feature_cols = [l.strip() for l in f if l.strip()]

    train_fit_df, val_df = split_train_validation(train_df)

    print(f"\nTrain: {len(train_df)}  Test: {len(test_df)}  Features: {len(feature_cols)}")
    print(f"Fit rows: {len(train_fit_df)}  Validation rows: {len(val_df)}")
    print(f"Train LOS  mean: {train_df['LOS'].mean():.2f} days")
    print(f"Test  LOS  mean: {test_df['LOS'].mean():.2f} days")

    # ------ identify note vs structured columns -------------------------
    note_cols   = [c for c in feature_cols if c.startswith(('cc_embedding_', 'hpi_embedding_'))]
    struct_cols = [c for c in feature_cols if c not in note_cols]
    print(f"Structured features : {len(struct_cols)}")
    print(f"Clinical-note feats : {len(note_cols)}")

    # ------ helper: scale & tensorise ----------------------------------
    def prepare(cols):
        scaler = StandardScaler()
        Xtr = scaler.fit_transform(train_fit_df[cols].values.astype(np.float32))
        Xval = scaler.transform(val_df[cols].values.astype(np.float32))
        Xte = scaler.transform(test_df[cols].values.astype(np.float32))
        return (torch.tensor(Xtr), torch.tensor(Xval), torch.tensor(Xte), scaler)

    # all features  /  structured only
    Xtr_all, Xval_all, Xte_all, _ = prepare(feature_cols)
    Xtr_str, Xval_str, Xte_str, _ = prepare(struct_cols)

    # regression targets
    y_reg_tr = torch.tensor(train_fit_df['LOS'].values, dtype=torch.float32)
    y_reg_val = torch.tensor(val_df['LOS'].values, dtype=torch.float32)
    y_reg_te = torch.tensor(test_df['LOS'].values,  dtype=torch.float32)

    # ------ layer dims helper -------------------------------------------
    def dims(n_feat):
        return [n_feat, 128, 64, 32] if n_feat > 200 else [n_feat, 64, 32]

    # ===================================================================
    # Regression with notes
    # ===================================================================
    res_C = run_regression_experiment(
        "with_notes",
        Xtr_all, y_reg_tr, Xval_all, y_reg_val, Xte_all, y_reg_te,
        dims(Xtr_all.shape[1]))

    # ===================================================================
    # Regression with structured features only
    # ===================================================================
    res_D = run_regression_experiment(
        "structured_only",
        Xtr_str, y_reg_tr, Xval_str, y_reg_val, Xte_str, y_reg_te,
        dims(Xtr_str.shape[1]))

    # ===================================================================
    # Summary table
    # ===================================================================
    print("\n" + "=" * 70)
    print("SUMMARY  (LLOS cohort, temporal test split)")
    print("=" * 70)

    print("\n  Regression (test set):")
    print(f"  {'Experiment':<20} {'MSE':>8} {'RMSE':>8} {'MAE':>8} {'R²':>8}")
    print(f"  {'-'*20} {'-'*8} {'-'*8} {'-'*8} {'-'*8}")
    for label, r in [("with_notes", res_C), ("structured_only", res_D)]:
        t = r['test']
        print(f"  {label:<20} {t['mse']:8.4f} {t['rmse']:8.4f} {t['mae']:8.4f} {t['r2']:8.4f}")

    print("\n" + "=" * 70)
    print("All experiments complete.")
    print("=" * 70)

    if len(train_df) < 100:
        print("\nWARNING: Dataset is very small. Regenerate Synthea with 1000+ patients")
        print("for meaningful results. Current output is for pipeline validation only.")

    # ------ save artefacts ---------------------------------------------
    torch.save({
        'with_notes':    {'model': res_C['model'].state_dict(),
                          'head':  res_C['head'].state_dict()},
        'structured_only': {'model': res_D['model'].state_dict(),
                            'head':  res_D['head'].state_dict()},
        'feature_cols': feature_cols,
        'struct_cols':  struct_cols,
        'note_cols':    note_cols,
    }, data_path / "llos_model.pt")
    print(f"\nModels saved to {data_path / 'llos_model.pt'}")

    predictions_df = test_df[['subject_id', 'hadm_id', 'LOS']].copy()
    predictions_df['pred_with_notes'] = res_C['preds']
    predictions_df['pred_structured_only'] = res_D['preds']
    predictions_df.to_csv(data_path / "test_predictions.csv", index=False)
    print(f"Test predictions saved to {data_path / 'test_predictions.csv'}")

    metrics_summary = {
        'with_notes': {split: {k: float(v) for k, v in res_C[split].items()} for split in ('train', 'val', 'test')},
        'structured_only': {split: {k: float(v) for k, v in res_D[split].items()} for split in ('train', 'val', 'test')},
    }
    with (data_path / "metrics_summary.json").open("w", encoding="utf-8") as f:
        json.dump(metrics_summary, f, indent=2)
    print(f"Metrics summary saved to {data_path / 'metrics_summary.json'}")

    return {'C': res_C, 'D': res_D}


if __name__ == "__main__":
    results = main()
