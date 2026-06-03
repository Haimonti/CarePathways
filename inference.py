import json
import os
import lightgbm as lgb
import torch
from los_predictor.config import (
    EXTRACTOR_CONFIG,
    HIDDEN_DIM,
    LGBM_MODEL_PATH,
    MODEL_WEIGHTS_PATH,
    SECTION_LIST,
    T5_PROMPT,
    device,
)
from los_predictor.models import LOSModel, T5Extractor
from los_predictor.utils import build_structured_input, parse_sections


def _validate_weight_files():
    missing = [
        f
        for f in [MODEL_WEIGHTS_PATH, LGBM_MODEL_PATH, EXTRACTOR_CONFIG]
        if not os.path.isfile(f)
    ]
    if missing:
        raise FileNotFoundError(
            "Missing model file(s) in working directory:\n  "
            + "\n  ".join(missing)
        )


def load_models():
    _validate_weight_files()
    checkpoint = torch.load(MODEL_WEIGHTS_PATH, map_location=device)
    bilstm_model = LOSModel()
    bilstm_model.load_state_dict(checkpoint["model_state_dict"])
    bilstm_model.to(device).eval()
    print(f"Loaded BiLSTM      : {MODEL_WEIGHTS_PATH}")
    lgbm_model = lgb.Booster(model_file=LGBM_MODEL_PATH)
    print(f"Loaded LightGBM    : {LGBM_MODEL_PATH}")
    with open(EXTRACTOR_CONFIG) as f:
        extractor_config = json.load(f)
    t5_extractor = T5Extractor(
        model_name=extractor_config["model_name"], device=device
    )
    print(f"Loaded T5Extractor : {extractor_config['model_name']}")
    return bilstm_model, lgbm_model, t5_extractor


_bilstm_model, _lgbm_model, _t5_extractor = load_models()
print(f"\nReady for inference.")
print(f"Task prompt: '{T5_PROMPT}'")


@torch.no_grad()
def predict_los(form_data: dict, prompt: str = T5_PROMPT):
    structured = build_structured_input(form_data)
    if not structured:
        raise ValueError(
            f"No valid section fields found. Expected keys: {SECTION_LIST}"
        )
    global T5_PROMPT
    saved_prompt = T5_PROMPT
    T5_PROMPT = prompt
    sections = parse_sections(structured)
    pooled = torch.zeros(len(SECTION_LIST), HIDDEN_DIM)
    for i, section in enumerate(SECTION_LIST):
        text = sections.get(section, "").strip()
        if text:
            embedding = _t5_extractor.encode(text)
            pooled[i] = torch.tensor(embedding, dtype=torch.float32)
    T5_PROMPT = saved_prompt
    feature_vector = (
        _bilstm_model.encode_document([pooled]).cpu().numpy().reshape(1, -1)
    )
    prediction = _lgbm_model.predict(feature_vector)[0]
    return max(0.0, round(float(prediction), 2))