"""
T5Extractor and LOSModel (BiLSTM + attention) for the manual entry pipeline.
Imports are all relative — no dependency on the old los_predictor package.
"""
from __future__ import annotations

import os

import numpy as np
import torch
import torch.nn as nn
from tqdm import tqdm
from transformers import AutoTokenizer, T5EncoderModel

from .config import (
    DROPOUT,
    HIDDEN_DIM,
    LSTM_HIDDEN,
    LSTM_OUT_DIM,
    MAX_NOTES,
    MAX_TOKENS,
    NUM_HEADS,
    SECTION_LIST,
    SHORT_SECTIONS,
    T5_BATCH_SIZE,
    DEFAULT_T5_PROMPT,
)
from .utils import (
    build_chunk_text,
    chunk_long_section,
    count_tokens,
    load_cached,
    make_cache_key,
    parse_sections,
    save_cached,
)


class T5Extractor:
    def __init__(
        self,
        model_name: str = "t5-small",
        device: torch.device | None = None,
        cache_dir: str | None = None,
        prompt: str = DEFAULT_T5_PROMPT,
    ):
        self.model_name = model_name
        self.device = device or torch.device("cpu")
        self.cache_dir = cache_dir
        self.prompt = prompt

        self.tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)
        self.encoder = T5EncoderModel.from_pretrained(model_name).to(self.device)
        self.encoder.eval()

        for param in self.encoder.parameters():
            param.requires_grad = False

        self.prompt_tokens = count_tokens(self.prompt, self.tokenizer)
        print(
            f"Loaded {model_name} — encoder frozen, "
            f"prompt overhead: {self.prompt_tokens} tokens"
        )

    @torch.no_grad()
    def _run_encoder(self, text: str) -> np.ndarray:
        enc = self.tokenizer(
            self.prompt + text,
            add_special_tokens=True,
            truncation=True,
            max_length=MAX_TOKENS,
            padding=False,
            return_tensors="pt",
        )
        out = self.encoder(**enc.to(self.device))
        n_real = enc["attention_mask"][0].sum().item()
        return out.last_hidden_state[0, :n_real].cpu().numpy()

    def _cache_path(self, text: str) -> str | None:
        if self.cache_dir is None:
            return None
        return os.path.join(self.cache_dir, f"{make_cache_key(text, self.model_name)}.npz")

    def encode_single(self, text: str) -> np.ndarray:
        path = self._cache_path(text)
        if path and os.path.exists(path):
            hit = load_cached(path)
            if hit is not None:
                return hit

        feats = self._run_encoder(text)
        if path:
            os.makedirs(os.path.dirname(path), exist_ok=True)
            save_cached(path, feats)
        return feats

    def encode_with_chunking(self, text: str) -> np.ndarray:
        sections = parse_sections(text)
        budget = MAX_TOKENS - self.prompt_tokens - 20
        chunks: list[str] = []
        current: list[tuple[str, str]] = []
        n_current = 0

        for sec in SECTION_LIST:
            txt = sections.get(sec, "")
            if not txt.strip():
                continue
            n = count_tokens(txt, self.tokenizer)

            if sec in SHORT_SECTIONS:
                if n_current + n > budget and current:
                    chunks.append(build_chunk_text(current, self.prompt))
                    current = [(sec, txt)]
                    n_current = n
                else:
                    current.append((sec, txt))
                    n_current += n
            elif n > budget:
                if current:
                    chunks.append(build_chunk_text(current, self.prompt))
                    current = []
                    n_current = 0
                for sub in chunk_long_section(txt, sec, self.tokenizer, budget):
                    chunks.append(sub)
            elif n_current + n > budget and current:
                chunks.append(build_chunk_text(current, self.prompt))
                current = [(sec, txt)]
                n_current = n
            else:
                current.append((sec, txt))
                n_current += n

        if current:
            chunks.append(build_chunk_text(current, self.prompt))

        vecs = [self.encode_single(c) for c in chunks]
        return np.mean([v.mean(axis=0) for v in vecs], axis=0)

    def encode(self, text: str) -> np.ndarray:
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

    def encode_batch(self, texts: list) -> list:
        results = [None] * len(texts)
        for i, text in enumerate(texts):
            path = self._cache_path(text)
            if path and os.path.exists(path):
                hit = load_cached(path)
                if hit is not None:
                    results[i] = hit
                    continue
            results[i] = self._run_encoder(text)
        return results


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

    def encode_note(self, pooled: torch.Tensor) -> torch.Tensor:
        combined = pooled + self.section_emb(self.section_ids)
        out, _ = self.note_bilstm(combined.unsqueeze(0))
        return out.mean(dim=1).squeeze(0)

    def encode_document(self, doc_pooled: list) -> torch.Tensor:
        notes = [self.encode_note(p) for p in doc_pooled[-MAX_NOTES:]]
        x = torch.stack(notes).unsqueeze(0)
        x, _ = self.bilstm(x)
        attn, _ = self.attention(x, x, x)
        x = self.norm(x + attn)
        return x.mean(dim=1).squeeze(0)

    def forward(self, doc_pooled: list) -> torch.Tensor:
        doc_vec = self.encode_document(doc_pooled)
        return self.regressor(doc_vec).squeeze(-1)
