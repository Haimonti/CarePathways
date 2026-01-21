"""
Deep Patient LLOS Prediction Training Script
=============================================
Trains a Stacked Denoising Autoencoder (Deep Patient) on Synthea data
to predict Long Length of Stay (LLOS).

Target: Binary classification (is_llos = 1 if LOS >= mean + 2σ)
Metrics: C-index, AUROC, F1, Precision, Recall
"""

import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader, TensorDataset
import pandas as pd
import numpy as np
from sklearn.metrics import roc_auc_score, f1_score, precision_score, recall_score, confusion_matrix
from sklearn.preprocessing import StandardScaler
import warnings
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
# LLOS Classifier Head
# -----------------------------
class LLOSClassifier(nn.Module):
    def __init__(self, embedding_dim, hidden_dim=64, dropout=0.3):
        super().__init__()
        self.classifier = nn.Sequential(
            nn.Linear(embedding_dim, hidden_dim),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(hidden_dim, 1)
        )
    
    def forward(self, z):
        return self.classifier(z).squeeze(-1)


# -----------------------------
# C-index Calculation
# -----------------------------
def concordance_index(y_true, y_pred):
    """
    Calculate concordance index (C-index) for ranking accuracy.
    For binary classification: probability of correctly ranking a random positive-negative pair.
    """
    y_true = np.array(y_true)
    y_pred = np.array(y_pred)
    
    n = len(y_true)
    concordant = 0
    discordant = 0
    tied = 0
    
    for i in range(n):
        for j in range(i + 1, n):
            if y_true[i] != y_true[j]:  # Only compare different outcomes
                if (y_true[i] > y_true[j] and y_pred[i] > y_pred[j]) or \
                   (y_true[i] < y_true[j] and y_pred[i] < y_pred[j]):
                    concordant += 1
                elif y_pred[i] == y_pred[j]:
                    tied += 1
                else:
                    discordant += 1
    
    total = concordant + discordant + tied
    if total == 0:
        return 0.5
    
    return (concordant + 0.5 * tied) / total


# -----------------------------
# Training Function
# -----------------------------
def train_model(model, classifier, X_train, y_train, X_val, y_val, 
                epochs=100, lr=1e-3, batch_size=16, pos_weight=None, verbose=True):
    """Fine-tune the full model for LLOS prediction."""
    
    # Create data loaders
    train_dataset = TensorDataset(X_train, y_train)
    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
    
    # Optimizer for both encoder and classifier
    params = list(model.parameters()) + list(classifier.parameters())
    optimizer = optim.Adam(params, lr=lr, weight_decay=1e-4)
    
    # Weighted BCE loss for class imbalance
    if pos_weight is None:
        pos_weight = torch.tensor([1.0])
    criterion = nn.BCEWithLogitsLoss(pos_weight=pos_weight)
    
    # Learning rate scheduler
    scheduler = optim.lr_scheduler.ReduceLROnPlateau(optimizer, mode='max', patience=10, factor=0.5)
    
    best_auc = 0
    best_state = None
    patience_counter = 0
    
    for epoch in range(epochs):
        model.train()
        classifier.train()
        
        epoch_loss = 0
        for X_batch, y_batch in train_loader:
            optimizer.zero_grad()
            
            # Forward pass
            embeddings = model(X_batch)
            logits = classifier(embeddings)
            
            loss = criterion(logits, y_batch)
            loss.backward()
            optimizer.step()
            
            epoch_loss += loss.item()
        
        # Validation
        model.eval()
        classifier.eval()
        
        with torch.no_grad():
            val_embeddings = model(X_val)
            val_logits = classifier(val_embeddings)
            val_probs = torch.sigmoid(val_logits).numpy()
            val_preds = (val_probs >= 0.5).astype(int)
            y_val_np = y_val.numpy()
        
        # Calculate metrics
        try:
            val_auc = roc_auc_score(y_val_np, val_probs)
        except:
            val_auc = 0.5
        
        scheduler.step(val_auc)
        
        # Early stopping
        if val_auc > best_auc:
            best_auc = val_auc
            best_state = {
                'model': model.state_dict(),
                'classifier': classifier.state_dict()
            }
            patience_counter = 0
        else:
            patience_counter += 1
        
        if verbose and (epoch + 1) % 10 == 0:
            print(f"Epoch {epoch+1}/{epochs}: loss={epoch_loss/len(train_loader):.4f}, val_AUC={val_auc:.4f}")
        
        if patience_counter >= 20:
            if verbose:
                print(f"Early stopping at epoch {epoch+1}")
            break
    
    # Load best model
    if best_state:
        model.load_state_dict(best_state['model'])
        classifier.load_state_dict(best_state['classifier'])
    
    return model, classifier


# -----------------------------
# Evaluation Function
# -----------------------------
def evaluate(model, classifier, X_test, y_test, threshold=0.5):
    """Comprehensive evaluation with all metrics."""
    model.eval()
    classifier.eval()
    
    with torch.no_grad():
        embeddings = model(X_test)
        logits = classifier(embeddings)
        probs = torch.sigmoid(logits).numpy()
        preds = (probs >= threshold).astype(int)
        y_true = y_test.numpy()
    
    results = {}
    
    # C-index
    results['c_index'] = concordance_index(y_true, probs)
    
    # AUROC
    try:
        results['auroc'] = roc_auc_score(y_true, probs)
    except:
        results['auroc'] = 0.5
    
    # Classification metrics
    results['f1'] = f1_score(y_true, preds, zero_division=0)
    results['precision'] = precision_score(y_true, preds, zero_division=0)
    results['recall'] = recall_score(y_true, preds, zero_division=0)
    
    # Confusion matrix
    results['confusion_matrix'] = confusion_matrix(y_true, preds)
    
    return results, probs, preds


# -----------------------------
# Main Training Pipeline
# -----------------------------
def main():
    print("=" * 60)
    print("Deep Patient LLOS Prediction - Synthea Data")
    print("=" * 60)
    
    # Load data
    data_path = "./reconstructions/datasets/synthea_processed"
    
    print("\n[1] Loading data...")
    train_df = pd.read_csv(f"{data_path}/train_set.csv")
    test_df = pd.read_csv(f"{data_path}/test_set.csv")
    
    with open(f"{data_path}/feature_columns.txt", 'r') as f:
        feature_cols = [line.strip() for line in f.readlines()]
    
    print(f"    Training samples: {len(train_df)}")
    print(f"    Test samples: {len(test_df)}")
    print(f"    Features: {len(feature_cols)}")
    
    # Prepare features
    X_train = train_df[feature_cols].values.astype(np.float32)
    y_train = train_df['is_llos'].values.astype(np.float32)
    X_test = test_df[feature_cols].values.astype(np.float32)
    y_test = test_df['is_llos'].values.astype(np.float32)
    
    # Normalize features (important for neural networks)
    scaler = StandardScaler()
    X_train = scaler.fit_transform(X_train)
    X_test = scaler.transform(X_test)
    
    # Convert to tensors
    X_train_t = torch.tensor(X_train, dtype=torch.float32)
    y_train_t = torch.tensor(y_train, dtype=torch.float32)
    X_test_t = torch.tensor(X_test, dtype=torch.float32)
    y_test_t = torch.tensor(y_test, dtype=torch.float32)
    
    # Class imbalance weight
    pos_count = y_train.sum()
    neg_count = len(y_train) - pos_count
    if pos_count > 0:
        pos_weight = torch.tensor([neg_count / pos_count])
    else:
        pos_weight = torch.tensor([1.0])
    
    print(f"    LLOS in train: {int(pos_count)} ({pos_count/len(y_train)*100:.1f}%)")
    print(f"    LLOS in test: {int(y_test.sum())} ({y_test.sum()/len(y_test)*100:.1f}%)")
    print(f"    Positive weight: {pos_weight.item():.2f}")
    
    # Define model architecture
    input_dim = len(feature_cols)
    
    # Adjust architecture for small dataset
    if input_dim > 200:
        layer_dims = [input_dim, 128, 64, 32]
    else:
        layer_dims = [input_dim, 64, 32]
    
    embedding_dim = layer_dims[-1]
    
    print(f"\n[2] Model Architecture:")
    print(f"    DeepPatient layers: {layer_dims}")
    print(f"    Embedding dimension: {embedding_dim}")
    
    # Create model
    model = DeepPatient(layer_dims, noise_std=0.05, dropout=0.1)
    classifier = LLOSClassifier(embedding_dim, hidden_dim=16, dropout=0.2)
    
    # Pretrain autoencoders
    print("\n[3] Pretraining autoencoders...")
    pretrained_embeddings = model.pretrain(
        X_train_t, 
        epochs=30, 
        lr=1e-3, 
        batch_size=16,
        verbose=True
    )
    
    # Fine-tune for LLOS prediction
    print("\n[4] Fine-tuning for LLOS prediction...")
    
    # Use train data for both training and validation in this small dataset
    # In practice, you'd want a proper validation split
    model, classifier = train_model(
        model, classifier,
        X_train_t, y_train_t,
        X_train_t, y_train_t,  # Using train as val due to small dataset
        epochs=100,
        lr=5e-4,
        batch_size=8,
        pos_weight=pos_weight,
        verbose=True
    )
    
    # Evaluate on test set
    print("\n[5] Evaluation Results:")
    print("-" * 40)
    
    # Test evaluation
    test_results, test_probs, test_preds = evaluate(model, classifier, X_test_t, y_test_t)
    
    print(f"Test Set Metrics:")
    print(f"  C-index:   {test_results['c_index']:.4f}")
    print(f"  AUROC:     {test_results['auroc']:.4f}")
    print(f"  F1-score:  {test_results['f1']:.4f}")
    print(f"  Precision: {test_results['precision']:.4f}")
    print(f"  Recall:    {test_results['recall']:.4f}")
    print(f"\nConfusion Matrix:")
    print(test_results['confusion_matrix'])
    
    # Train evaluation (for comparison)
    train_results, _, _ = evaluate(model, classifier, X_train_t, y_train_t)
    print(f"\nTraining Set Metrics (for reference):")
    print(f"  C-index:   {train_results['c_index']:.4f}")
    print(f"  AUROC:     {train_results['auroc']:.4f}")
    print(f"  F1-score:  {train_results['f1']:.4f}")
    
    # Save model
    print("\n[6] Saving model...")
    torch.save({
        'model_state_dict': model.state_dict(),
        'classifier_state_dict': classifier.state_dict(),
        'layer_dims': layer_dims,
        'feature_cols': feature_cols,
        'scaler_mean': scaler.mean_,
        'scaler_scale': scaler.scale_
    }, f"{data_path}/llos_model.pt")
    print(f"    Model saved to {data_path}/llos_model.pt")
    
    # Save predictions
    test_df['predicted_prob'] = test_probs
    test_df['predicted_llos'] = test_preds
    test_df.to_csv(f"{data_path}/test_predictions.csv", index=False)
    print(f"    Predictions saved to {data_path}/test_predictions.csv")
    
    print("\n" + "=" * 60)
    print("Training complete!")
    print("=" * 60)
    
    # Warning about small dataset
    if len(train_df) < 100:
        print("\n⚠️  WARNING: Dataset is very small!")
        print("    For meaningful results, regenerate Synthea with 1000+ patients.")
        print("    Current results are for pipeline testing only.")
    
    return model, classifier, test_results


if __name__ == "__main__":
    model, classifier, results = main()
