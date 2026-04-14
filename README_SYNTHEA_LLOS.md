# Deep Patient LLOS Prediction with Synthea Data

Predict **Long Length of Stay (LLOS)** using a Stacked Denoising Autoencoder (Deep Patient) trained on synthetic EHR data from Synthea.

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
Input Features → 128 → 64 → 32 (embedding) → Regressor → LOS Prediction
```

---

## Requirements

### Python Environment
- Python 3.11+ (tested on 3.13)
- Virtual environment recommended

### Dependencies
```bash
pip install pandas numpy torch scikit-learn nltk gensim PyYAML
```

### Synthea (for data generation)
- Java 11+
- [Synthea](https://github.com/synthetichealth/synthea) - Synthetic Patient Generator

---

## Quick Start

### 1. Generate Synthea Data

Download Synthea and generate patients:

```bash
# Download Synthea
git clone https://github.com/synthetichealth/synthea.git
cd synthea

# Build (requires Gradle)
./gradlew build check

# Generate 1000 patients (recommended minimum for meaningful results)
java -jar build/libs/synthea-with-dependencies.jar \
  -p 1000 \
  -s 42 \
  --exporter.csv.export=true \
  Massachusetts
```

Copy the output to the project:
```bash
cp -r output/csv /path/to/deep_patient_v2/output_synthea/
cp -r output/notes /path/to/deep_patient_v2/output_synthea/
```

### 2. Run Preprocessing

Open and run the Jupyter notebook:
```bash
jupyter notebook reconstructions/Preprocessing_of_Synthea.ipynb
```

Or run all cells programmatically:
```bash
jupyter nbconvert --to notebook --execute reconstructions/Preprocessing_of_Synthea.ipynb
```

**Output files** (saved to `reconstructions/datasets/synthea_processed/`):
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
deep_patient_v2/
├── output_synthea/
│   ├── csv/                      # Synthea CSV exports
│   │   ├── patients.csv
│   │   ├── encounters.csv
│   │   ├── conditions.csv
│   │   ├── procedures.csv
│   │   └── medications.csv
│   └── notes/                    # Synthea clinical notes
│       └── *.txt
├── reconstructions/
│   ├── Preprocessing_of_Synthea.ipynb   # Main preprocessing notebook
│   └── datasets/
│       └── synthea_processed/    # Preprocessed outputs
├── train_llos_synthea.py         # Model training script
└── deep_patient_py_torch_modern.py  # Original Deep Patient implementation
```

---

## Preprocessing Pipeline

The notebook performs these steps:

| Step | Description | Output |
|------|-------------|--------|
| 1 | Filter inpatient encounters, calculate LOS | `phy_ad.csv` |
| 2 | Filter to the LLOS cohort (LOS ≥ mean + 2σ) | Regression cohort |
| 3 | One-hot encode diagnosis SNOMED codes | `phyad_dicd.csv` |
| 4 | One-hot encode procedure SNOMED codes | `phyad_dicd_picd.csv` |
| 5 | Build medication frequency matrix | `phyad_dicd_picd_medfreq.csv` |
| 6 | Extract & clean Chief Complaint | `chief_complaint_cleaned.csv` |
| 7 | Extract & clean HPI | `hpi_cleaned.csv` |
| 8 | Train Word2Vec, compute embeddings | `cc_hpi_embed.txt` |
| 9 | Merge all features | `final_dataset.csv` |
| 10 | Temporal train/test split | `train_set.csv`, `test_set.csv` |

---

## Model Training

The training script:

1. **Pretrains** stacked denoising autoencoders layer-by-layer
2. **Splits** the notebook's training output into fit/validation subsets for early stopping
3. **Fine-tunes** the full model with a regression head
4. **Reports metrics**: MSE, RMSE, MAE, R²

### Hyperparameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `noise_std` | 0.05 | Denoising noise level |
| `dropout` | 0.1-0.3 | Regularization |
| `pretrain_epochs` | 30 | Autoencoder pretraining |
| `finetune_epochs` | 100 | Regression fine-tuning |
| `batch_size` | 8-16 | Adjusted for small datasets |
| `learning_rate` | 5e-4 | Adam optimizer |

---

## Evaluation Metrics

| Metric | Description |
|--------|-------------|
| **MSE** | Mean Squared Error - primary regression loss |
| **RMSE** | Root MSE - same units as LOS (days) |
| **MAE** | Mean Absolute Error - average absolute residual |
| **R²** | Coefficient of determination - explained variance |

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

### Weak regression metrics
- Increase dataset size (more Synthea patients)
- Check LLOS distribution in train/test sets
- Tune the validation split and layer widths

### Out of memory
- Reduce batch size in training script
- Use fewer Word2Vec embedding dimensions

---

## Adapting for Other Datasets

### Using Real EHR Data (MIMIC-IV)

1. Replace Synthea CSV paths with MIMIC-IV paths
2. Map ICD-9/10 codes instead of SNOMED
3. Update note extraction regex for MIMIC discharge summaries
4. Adjust demographic mappings

### Changing the Target Variable

Edit the LLOS threshold in the preprocessing notebook:
```python
# Current: mean + 2σ
los_threshold = los_mean + 2 * los_std

# Alternative: Fixed threshold (e.g., 7 days)
los_threshold = 7.0

# Alternative: Top 10% longest stays
los_threshold = df_phy_ad['LOS'].quantile(0.90)
```

---

## Citation

If you use this code, please cite the original Deep Patient paper:

```bibtex
@article{miotto2016deep,
  title={Deep patient: An unsupervised representation to predict 
         the future of patients from the electronic health records},
  author={Miotto, Riccardo and Li, Li and Kidd, Brian A and Dudley, Joel T},
  journal={Scientific reports},
  volume={6},
  number={1},
  pages={26094},
  year={2016},
  publisher={Nature Publishing Group}
}
```

---

## License

This project is for research and educational purposes. Synthea data is synthetic and does not contain real patient information.
