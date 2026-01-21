# Deep Patient LLOS Prediction with Synthea Data

Predict **Long Length of Stay (LLOS)** using a Stacked Denoising Autoencoder based on the Deep Patient paper trained on synthetic EHR data from Synthea.

## Overview

This pipeline adapts the [Deep Patient](https://www.nature.com/articles/srep26094) model architecture for LLOS prediction using Synthea-generated synthetic patient data. LLOS is defined as hospital stays ≥ mean + 2 standard deviations.

### Features Used
- **Demographics**: Age, gender, race, marital status
- **Diagnoses**: SNOMED-CT codes (one-hot encoded)
- **Procedures**: SNOMED-CT codes (one-hot encoded)
- **Medications**: RxNorm codes with frequency
- **Clinical Notes**: Chief Complaint and History of Present Illness (Word2Vec embeddings)

### Model Architecture
```
Input Features → 128 → 64 → 32 (embedding) → Classifier → LLOS Prediction
```

---

## Requirements

### Python Environment
- Python 3.11+ (tested on 3.13)
- Virtual environment recommended

### Dependencies
```bash
pip install pandas numpy torch scikit-learn nltk gensim
```

### Synthea (for data generation)
- Java 11+
- [Synthea](https://github.com/synthetichealth/synthea) - Synthetic Patient Generator

---

## Quick Start

### 1. Generate Synthea Data

Download the Synthea JAR file from the [Synthea Releases page](https://github.com/synthetichealth/synthea/releases) (look for `synthea-with-dependencies.jar`), then generate patients:

```bash
# Generate 1000 patients (recommended minimum for meaningful results)
java -jar synthea-with-dependencies.jar \
  -p 1000 \
  -s 42 \
  -c synthea.properties
```

Copy the output to the project:
```bash
cp -r output/csv /path/to/CarePathways/output_synthea/
cp -r output/notes /path/to/CarePathways/output_synthea/
```

### 2. Run Preprocessing

Open and run the Jupyter notebook:
```bash
jupyter notebook synthea_preprocessing.ipynb
```

Or run all cells programmatically:
```bash
jupyter nbconvert --to notebook --execute synthea_preprocessing.ipynb
```

**Output files** (saved to `datasets/synthea_processed/`):
- `final_dataset.csv` - Complete preprocessed dataset
- `train_set.csv` - Training split (80%)
- `test_set.csv` - Test split (20%)
- `feature_columns.txt` - Feature names

### 3. Train the Model

```bash
python train_llos_synthea.py
```

**Output**:
- `llos_model.pt` - Trained PyTorch model
- `test_predictions.csv` - Predictions on test set

---

## Project Structure

```
CarePathways/
├── README.md                     # This file
├── synthea_preprocessing.ipynb   # Main preprocessing notebook
├── train_llos_synthea.py         # Model training script
├── synthea.properties            # Synthea configuration
├── PhysicalandAdmission.csv      # Sample output
├── output_synthea/               # Synthea data (create this folder)
│   ├── csv/                      # Synthea CSV exports
│   │   ├── patients.csv
│   │   ├── encounters.csv
│   │   ├── conditions.csv
│   │   ├── procedures.csv
│   │   └── medications.csv
│   └── notes/                    # Synthea clinical notes
│       └── *.txt
└── datasets/
    └── synthea_processed/        # Preprocessed outputs
        ├── final_dataset.csv
        ├── train_set.csv
        ├── test_set.csv
        └── llos_model.pt
```

---

## Preprocessing Pipeline

The notebook performs these steps:

| Step | Description | Output |
|------|-------------|--------|
| 1 | Filter inpatient encounters, calculate LOS | `phy_ad.csv` |
| 2 | Create LLOS label (LOS ≥ mean + 2σ) | Binary target |
| 3 | One-hot encode diagnosis SNOMED codes | `phyad_dicd.csv` |
| 4 | One-hot encode procedure SNOMED codes | `phyad_dicd_picd.csv` |
| 5 | Build medication frequency matrix | `phyad_dicd_picd_medfreq.csv` |
| 6 | Extract & clean Chief Complaint | `chief_complaint_cleaned.csv` |
| 7 | Extract & clean HPI | `hpi_cleaned.csv` |
| 8 | Train Word2Vec, compute embeddings | `cc_hpi_embed.txt` |
| 9 | Merge all features | `final_dataset.csv` |
| 10 | Stratified train/test split | `train_set.csv`, `test_set.csv` |

---

## Model Training

The training script:

1. **Pretrains** stacked denoising autoencoders layer-by-layer
2. **Fine-tunes** the full model with a classification head
3. **Handles class imbalance** with weighted BCE loss
4. **Reports metrics**: C-index, AUROC, F1, Precision, Recall

### Hyperparameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `noise_std` | 0.05 | Denoising noise level |
| `dropout` | 0.1-0.3 | Regularization |
| `pretrain_epochs` | 30 | Autoencoder pretraining |
| `finetune_epochs` | 100 | Classification fine-tuning |
| `batch_size` | 8-16 | Adjusted for small datasets |
| `learning_rate` | 5e-4 | Adam optimizer |

---

## Evaluation Metrics

| Metric | Description |
|--------|-------------|
| **C-index** | Concordance index - ranking accuracy for pairs |
| **AUROC** | Area under ROC curve - discrimination ability |
| **F1-score** | Harmonic mean of precision and recall |
| **Precision** | True positives / Predicted positives |
| **Recall** | True positives / Actual positives |

---

## Recommended Dataset Size

For meaningful results:

| Patients | Inpatient Encounters | Expected LLOS (~2.5%) | Recommendation |
|----------|---------------------|----------------------|----------------|
| 100 | ~50 | ~1-2 | ❌ Too small |
| 1,000 | ~500 | ~12-15 | ⚠️ Minimum viable |
| 5,000 | ~2,500 | ~60-80 | ✅ Recommended |
| 10,000+ | ~5,000+ | ~125+ | ✅ Ideal |

---

## Troubleshooting

### "No module named 'torch'"
```bash
pip install torch
```

### "No module named 'gensim'"
```bash
pip install gensim
```

### Timezone errors in preprocessing
The notebook handles timezone-aware timestamps from Synthea. If you see timezone errors, ensure pandas is up to date:
```bash
pip install --upgrade pandas
```

### Low AUROC / C-index
- Increase dataset size (more Synthea patients)
- Check LLOS distribution in train/test sets
- Adjust positive class weight

### Out of memory
- Reduce batch size in training script
- Use fewer Word2Vec embedding dimensions

---
