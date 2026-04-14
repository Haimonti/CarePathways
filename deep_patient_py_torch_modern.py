# DeepPatient (Modern PyTorch Re-Implementation)
# ------------------------------------------------------------
# This is a clean, end-to-end PyTorch version of Deep Patient
# (Miotto et al.) using stacked denoising autoencoders.
# Works with sparse EHR feature vectors (e.g., ICD codes).

import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader, TensorDataset

# -----------------------------
# Denoising Autoencoder Module
# -----------------------------
class DenoisingAutoencoder(nn.Module):
    def __init__(self, input_dim, hidden_dim, noise_std=0.1):
        super().__init__()
        self.noise_std = noise_std
        self.encoder = nn.Sequential(
            nn.Linear(input_dim, hidden_dim),
            nn.ReLU()
        )
        self.decoder = nn.Sequential(
            nn.Linear(hidden_dim, input_dim),
            nn.Sigmoid()
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
    def __init__(self, layer_dims):
        super().__init__()
        self.autoencoders = nn.ModuleList([
            DenoisingAutoencoder(layer_dims[i], layer_dims[i+1])
            for i in range(len(layer_dims)-1)
        ])

    def forward(self, x):
        for ae in self.autoencoders:
            _, x = ae(x)
        return x

    def pretrain(self, data, epochs=20, lr=1e-3, batch_size=128):
        x = data
        for i, ae in enumerate(self.autoencoders):
            print(f"Pretraining layer {i+1}/{len(self.autoencoders)}")
            dataset = DataLoader(TensorDataset(x), batch_size=batch_size, shuffle=True)
            optimizer = optim.Adam(ae.parameters(), lr=lr)
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
                print(f"Epoch {epoch+1}: loss={epoch_loss/len(dataset):.4f}")

            # Transform data for next layer
            with torch.no_grad():
                _, x = ae(x)
        return x

# -----------------------------
# Example Usage
# -----------------------------
if __name__ == "__main__":
    # Example: 10k patients, 5k binary diagnosis/procedure features
    n_patients = 10000
    input_dim = 5000

    X = torch.rand(n_patients, input_dim)

    # Deep Patient architecture from paper (example)
    model = DeepPatient([
        input_dim,
        1000,
        500,
        100
    ])

    patient_embeddings = model.pretrain(X, epochs=10)

    print("Final patient representation shape:", patient_embeddings.shape)

# ============================================================
# EXTENSIONS: MIMIC-IV, Cox Survival Head, Publishable Baseline
# ============================================================

"""
SECTION A — MIMIC-IV ADAPTATION (Concept-Level Features)
--------------------------------------------------------
Assumes you have preprocessed MIMIC-IV into a patient × concept matrix
(e.g., ICD, procedures, meds, labs) using OMOP-style features.

Typical pipeline:
1. Extract events per patient before an index time
2. Map events → concept IDs
3. Build sparse binary or count matrix
4. Convert to dense torch tensor (or keep sparse)

Example (placeholder):
"""

# X_mimic: torch.Tensor [num_patients, num_concepts]
# y_time: torch.Tensor [num_patients]  (time-to-event)
# y_event: torch.Tensor [num_patients] (1=event, 0=censored)

# Example dimensions
num_patients = 20000
num_concepts = 8000

X_mimic = torch.rand(num_patients, num_concepts)
y_time = torch.rand(num_patients) * 365
y_event = torch.randint(0, 2, (num_patients,))

# Pretrain DeepPatient on MIMIC features
mimic_model = DeepPatient([
    num_concepts,
    2000,
    1000,
    256
])

Z = mimic_model.pretrain(X_mimic, epochs=15)

print("MIMIC patient embeddings:", Z.shape)

# ------------------------------------------------------------
# SECTION B — Cox Proportional Hazards Head
# ------------------------------------------------------------

class CoxHead(nn.Module):
    def __init__(self, input_dim):
        super().__init__()
        self.linear = nn.Linear(input_dim, 1)

    def forward(self, z):
        return self.linear(z).squeeze(-1)


def cox_partial_likelihood(risk, time, event):
    # Sort by descending time
    order = torch.argsort(time, descending=True)
    risk = risk[order]
    event = event[order]

    log_cumsum = torch.logcumsumexp(risk, dim=0)
    loss = -torch.sum((risk - log_cumsum) * event) / event.sum()
    return loss

cox_model = CoxHead(Z.shape[1])
optimizer = optim.Adam(cox_model.parameters(), lr=1e-3)

for epoch in range(50):
    optimizer.zero_grad()
    risk_scores = cox_model(Z)
    loss = cox_partial_likelihood(risk_scores, y_time, y_event)
    loss.backward()
    optimizer.step()

    if epoch % 10 == 0:
        print(f"Epoch {epoch}: Cox loss={loss.item():.4f}")

# ------------------------------------------------------------
# SECTION C — Publishable Baseline Experiment Stack
# ------------------------------------------------------------

"""
Recommended baselines for a paper:

1. Logistic Regression (raw features)
2. XGBoost / LightGBM
3. CoxPH (raw features)
4. DeepPatient + Cox (this model)
5. ClinicalBERT / Med-BERT (text-based)

Ablations:
- Without denoising
- Different embedding sizes
- Binary vs count features

Evaluation:
- C-index
- Time-dependent AUC
- Calibration curves

Statistical tests:
- Bootstrap CI for C-index
- Paired DeLong test
"""

print("DeepPatient + Cox pipeline ready.")

