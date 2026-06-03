import os

import numpy as np
import torch
import torch.nn as nn
from tqdm import tqdm
from transformers import AutoTokenizer, T5EncoderModel

from los_predictor.config import (
    CACHE_DIR,
    HIDDEN_DIM,
    LSTM_HIDDEN,
    LSTM_OUT_DIM,
    MAX_NOTES,
    MAX_TOKENS,
    NUM_HEADS,
    DROPOUT,
    SECTION_LIST,
    SHORT_SECTIONS,
    T5_BATCH_SIZE,
    T5_PROMPT,
)
from los_predictor.utils import (
    build_chunk_text,
    chunk_long_section,
    count_tokens,
    load_cached,
    make_cache_key,
    parse_sections,
    save_cached,
)


class T5Extractor:
    def __init__(self, model_name: str = "t5-small", device=None):
        self.model_name = model_name
        self.device = device or torch.device("cpu")
        self.tokenizer = AutoTokenizer.from_pretrained(
            model_name, use_fast=True
        )
        self.encoder = T5EncoderModel.from_pretrained(model_name).to(
            self.device
        )
        self.encoder.eval()

        for param in self.encoder.parameters():
            param.requires_grad = False

        self.prompt_tokens = count_tokens(T5_PROMPT, self.tokenizer)
        print(
            f"Loaded {model_name} — encoder frozen, "
            f"prompt overhead: {self.prompt_tokens} tokens"
        )

    @torch.no_grad()
    def _run_encoder(self, text: str):
        enc = self.tokenizer(
            T5_PROMPT + text,
            add_special_tokens=True,
            truncation=True,
            max_length=MAX_TOKENS,
            padding=False,
            return_tensors="pt",
        )
        out = self.encoder(**enc.to(self.device))
        n_real = enc["attention_mask"][0].sum().item()
        return out.last_hidden_state[0, :n_real].cpu().numpy()

    def encode_single(self, text: str):
        path = os.path.join(
            CACHE_DIR, f"{make_cache_key(text, self.model_name)}.npz"
        )
        if os.path.exists(path):
            hit = load_cached(path)
            if hit is not None:
                return hit

        feats = self._run_encoder(text)
        os.makedirs(CACHE_DIR, exist_ok=True)
        save_cached(path, feats)
        return feats

    def encode_with_chunking(self, text: str):
        sections = parse_sections(text)
        budget = MAX_TOKENS - self.prompt_tokens - 20
        chunks = []
        current = []
        n_current = 0

        for sec in SECTION_LIST:
            txt = sections.get(sec, "")
            if not txt.strip():
                continue

            n = count_tokens(txt, self.tokenizer)

            if sec in SHORT_SECTIONS:
                if n_current + n > budget and current:
                    chunks.append(build_chunk_text(current))
                    current = [(sec, txt)]
                    n_current = n
                else:
                    current.append((sec, txt))
                    n_current += n

            elif n > budget:
                if current:
                    chunks.append(build_chunk_text(current))
                    current = []
                    n_current = 0
                for sub in chunk_long_section(txt, sec, self.tokenizer, budget):
                    chunks.append(sub)

            elif n_current + n > budget and current:
                chunks.append(build_chunk_text(current))
                current = [(sec, txt)]
                n_current = n

            else:
                current.append((sec, txt))
                n_current += n

        if current:
            chunks.append(build_chunk_text(current))

        vecs = [self.encode_single(c) for c in chunks]
        return np.mean([v.mean(axis=0) for v in vecs], axis=0)

    def encode(self, text: str):
        sections = parse_sections(text)
        budget = MAX_TOKENS - self.prompt_tokens - 20
        total = sum(
            count_tokens(sections[s], self.tokenizer)
            for s in SECTION_LIST
            if sections.get(s, "").strip()
        )
        if total > budget:
            return self.encode_with_chunking(text)
        return self.encode_single(text).mean(axis=0)

    def encode_batch(self, texts: list):
        results = [None] * len(texts)
        for i, text in enumerate(texts):
            path = os.path.join(
                CACHE_DIR, f"{make_cache_key(text, self.model_name)}.npz"
            )
            if os.path.exists(path):
                hit = load_cached(path)
                if hit is not None:
                    results[i] = hit
                    continue
            results[i] = self._run_encoder(text)
        return results

    def extract(self, df):
        rows = df[["input", "target"]].values
        all_docs = []
        unique_map = {}

        for input_text, _ in rows:
            secs = parse_sections(input_text)
            all_docs.append(secs)
            for txt in secs.values():
                if txt.strip():
                    unique_map[txt] = None

        unique_texts = list(unique_map.keys())
        print(f"Pre-encoding {len(unique_texts)} unique section texts")

        for i in tqdm(
            range(0, len(unique_texts), T5_BATCH_SIZE), desc="T5 encode"
        ):
            self.encode_batch(unique_texts[i: i + T5_BATCH_SIZE])

        docs = [[s] for s in all_docs]
        targets = np.array([t for _, t in rows], dtype=np.float32)
        return docs, targets


class LOSModel(nn.Module):
    def __init__(self):
        super().__init__()
        self.section_emb = nn.Embedding(len(SECTION_LIST), HIDDEN_DIM)
        self.note_bilstm = nn.LSTM(
            HIDDEN_DIM, LSTM_HIDDEN, batch_first=True, bidirectional=True
        )
        self.bilstm = nn.LSTM(
            LSTM_OUT_DIM, LSTM_HIDDEN, batch_first=True, bidirectional=True
        )
        self.attention = nn.MultiheadAttention(
            LSTM_OUT_DIM, num_heads=NUM_HEADS, dropout=DROPOUT, batch_first=True
        )
        self.norm = nn.LayerNorm(LSTM_OUT_DIM)
        self.regressor = nn.Sequential(
            nn.Linear(LSTM_OUT_DIM, 128),
            nn.ReLU(),
            nn.Dropout(DROPOUT),
            nn.Linear(128, 1),
        )
        self.register_buffer("section_ids", torch.arange(len(SECTION_LIST)))

    def encode_note(self, pooled: torch.Tensor):
        combined = pooled + self.section_emb(self.section_ids)
        out, _ = self.note_bilstm(combined.unsqueeze(0))
        return out.mean(dim=1).squeeze(0)

    def encode_document(self, doc_pooled: list):
        notes = [self.encode_note(p) for p in doc_pooled[-MAX_NOTES:]]
        x = torch.stack(notes).unsqueeze(0)
        x, _ = self.bilstm(x)
        attn, _ = self.attention(x, x, x)
        x = self.norm(x + attn)
        return x.mean(dim=1).squeeze(0)

    def forward(self, doc_pooled: list):
        doc_vec = self.encode_document(doc_pooled)
        return self.regressor(doc_vec).squeeze(-1)


def precompute_pooled(docs: list, extractor: T5Extractor, desc: str = "Pre-computing embeddings"):
    all_pooled = []
    for doc in tqdm(docs, desc=desc):
        doc_pooled = []
        for note in doc:
            pooled = torch.zeros(len(SECTION_LIST), HIDDEN_DIM)
            for i, sec in enumerate(SECTION_LIST):
                txt = note.get(sec, "")
                if txt.strip():
                    vec = extractor.encode(txt)
                    pooled[i] = torch.tensor(vec, dtype=torch.float32)
            doc_pooled.append(pooled)
        all_pooled.append(doc_pooled)
    return all_pooled


def extract_features(model: LOSModel, pooled_docs: list, desc: str = "Extracting"):
    model.eval()
    features = []
    with torch.no_grad():
        for doc_pooled in tqdm(pooled_docs, desc=desc):
            features.append(model.encode_document(doc_pooled).numpy())
    return np.stack(features)