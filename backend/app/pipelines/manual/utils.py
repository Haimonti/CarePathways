"""Utility functions for text parsing and embedding caching."""
from __future__ import annotations

import hashlib
import os
import re

from .config import (
    CLOSE_TAG_PATTERN,
    DEFAULT_T5_PROMPT,
    SECTION_LIST,
    SECTION_PATTERN,
    SECTION_SET,
)


def parse_sections(text: str) -> dict:
    sections = {s: "" for s in SECTION_LIST}
    for sec, content, _ in SECTION_PATTERN.findall(text):
        sec = sec.strip()
        if sec in SECTION_SET:
            sections[sec] = CLOSE_TAG_PATTERN.sub(" ", content).strip()
    return sections


def build_structured_input(form_data: dict) -> str:
    parts = []
    for section in SECTION_LIST:
        value = form_data.get(section, "").strip()
        if value:
            parts.append(f"<{section}> {value} </s>")
    return " ".join(parts)


def count_tokens(text: str, tokenizer) -> int:
    return len(tokenizer.encode(text, add_special_tokens=False))


def chunk_long_section(text: str, name: str, tokenizer, budget: int) -> list:
    sentences = re.split(r"(?<=[.!?])\s+", text)
    chunks = []
    current: list[str] = []
    n_current = 0

    for sent in sentences:
        n = count_tokens(sent, tokenizer)
        if n_current + n > budget and current:
            chunks.append(f"<{name}> {' '.join(current)}")
            current = [sent]
            n_current = n
        else:
            current.append(sent)
            n_current += n

    if current:
        chunks.append(f"<{name}> {' '.join(current)}")

    return chunks


def build_chunk_text(chunk_sections: list, prompt: str = DEFAULT_T5_PROMPT) -> str:
    parts = [f"<{name}> {text}" for name, text in chunk_sections]
    return prompt + " </s> ".join(parts)


def make_cache_key(text: str, model_name: str) -> str:
    return hashlib.md5(f"{model_name}::{text}".encode()).hexdigest()


def load_cached(path: str):
    import numpy as np

    try:
        return np.load(path, allow_pickle=False)["features"]
    except Exception:
        if os.path.exists(path):
            os.remove(path)
        return None


def save_cached(path: str, features) -> None:
    import numpy as np

    np.savez_compressed(path, features=features)
