import argparse, os, sys, re, unicodedata
import numpy as np
import pandas as pd
import torch
from transformers import AutoTokenizer, AutoModel


def set_threading(threads: int):
    threads = max(1, int(threads))
    os.environ.setdefault("OMP_NUM_THREADS", str(threads))
    os.environ.setdefault("MKL_NUM_THREADS", str(threads))
    os.environ.setdefault("OPENBLAS_NUM_THREADS", str(threads))
    os.environ.setdefault("VECLIB_MAXIMUM_THREADS", str(threads))
    os.environ.setdefault("NUMEXPR_NUM_THREADS", str(threads))
    os.environ.setdefault("TOKENIZERS_PARALLELISM", "true")
    try:
        torch.set_num_threads(threads)
        torch.set_num_interop_threads(min(threads, 4))
    except Exception:
        pass


def preprocess_text(text: str) -> str:

    if not isinstance(text, str):
        return ""

    text = unicodedata.normalize("NFKC", text)
    text = re.sub(r"[\r\n\t]+", " ", text)
    text = re.sub(r"\s{2,}", " ", text)

    text = re.sub(r"[^\x00-\x7F]+", " ", text)
    text = text.replace("–", "-").replace("—", "-")
    text = re.sub(r"([.,;:!?\-])\1{2,}", r"\1", text)
    return text.strip()


def mean_pool(last_hidden_state: torch.Tensor, attention_mask: torch.Tensor) -> torch.Tensor:
    mask = attention_mask.unsqueeze(-1).expand(last_hidden_state.size()).float()
    summed = torch.sum(last_hidden_state * mask, dim=1)
    counts = torch.clamp(mask.sum(dim=1), min=1e-9)
    return summed / counts

def cls_pool(last_hidden_state: torch.Tensor, attention_mask: torch.Tensor) -> torch.Tensor:
    return last_hidden_state[:, 0, :]

#embed func
@torch.inference_mode()
def embed_texts_cpu(
    texts,
    model_name="medicalai/ClinicalBERT",
    batch_size=16,
    max_len=256,
    pooling="mean",
):
    tok = AutoTokenizer.from_pretrained(model_name, use_fast=True)
    mdl = AutoModel.from_pretrained(model_name)
    mdl.to("cpu").eval()

    pool_fn = mean_pool if pooling.lower() == "mean" else cls_pool
    outs = []

    for s in range(0, len(texts), batch_size):
        batch = texts[s:s + batch_size]
        enc = tok(
            batch,
            padding=True,
            truncation=True,
            max_length=max_len,
            return_tensors="pt"
        )
        out = mdl(**enc)
        pooled = pool_fn(out.last_hidden_state, enc["attention_mask"])
        outs.append(pooled.detach().float().numpy())

    return np.vstack(outs)

def main():
    ap = argparse.ArgumentParser("ClinicalBERT embeddings → clinicalbert_* with multi-core threading.")
    ap.add_argument("--csv", required=True, help="Path to input CSV")
    ap.add_argument("--text_col", default="", help="Text column to embed (auto-detect if not provided)")
    ap.add_argument("--id_cols", default="subject_id,hadm_id", help="Comma-separated ID columns to carry over")
    ap.add_argument("--out_csv", required=True, help="Path to output CSV")
    ap.add_argument("--model", default="medicalai/ClinicalBERT", help="Hugging Face model name")
    ap.add_argument("--batch_size", type=int, default=16, help="Batch size for CPU")
    ap.add_argument("--max_len", type=int, default=256, help="Token limit (256–512 for long notes)")
    ap.add_argument("--pooling", choices=["mean","cls"], default="mean", help="Pooling strategy")
    ap.add_argument("--threads", type=int, default=os.cpu_count() or 8, help="CPU threads to use")
    ap.add_argument("--prefix", default="clinicalbert", help="Output column prefix (default: clinicalbert)")
    args = ap.parse_args()

    set_threading(args.threads)

    if not os.path.exists(args.csv):
        print(f"ERROR: CSV not found: {args.csv}", file=sys.stderr)
        sys.exit(1)

    df = pd.read_csv(args.csv)

    #detect text column or confirm it
    if args.text_col and args.text_col in df.columns:
        text_col = args.text_col
    else:
        candidates = [c for c in df.columns if any(k in c.lower() for k in ["brief", "course", "note", "summary", "text", "complaint"])]
        if len(candidates) == 1:
            text_col = candidates[0]
            print(f"Detected text column automatically: {text_col}")
        elif len(candidates) > 1:
            print(f"Multiple possible text columns found: {candidates}")
            print("Please specify one with --text_col")
            sys.exit(1)
        else:
            print(f"ERROR: No suitable text column found in CSV. Columns: {list(df.columns)}")
            sys.exit(1)

    #columns (id)
    id_cols = [c.strip() for c in args.id_cols.split(",") if c.strip() and c in df.columns]
    out_df = df[id_cols].copy() if id_cols else pd.DataFrame(index=df.index)

    #preprocess ttext 
    texts = df[text_col].fillna("").astype(str).apply(preprocess_text).tolist()

    print(
        f"Model: {args.model} | Device: CPU | Threads: {args.threads} | "
        f"Pooling: {args.pooling} | Batch={args.batch_size} | Max_len={args.max_len} | Text_col={text_col}"
    )

    #embedding_computation
    E = embed_texts_cpu(
        texts=texts,
        model_name=args.model,
        batch_size=args.batch_size,
        max_len=args.max_len,
        pooling=args.pooling
    )



    for i in range(E.shape[1]):
        out_df[f"{args.prefix}_{i}"] = E[:, i]

    tmp = args.out_csv + ".tmp"
    out_df.to_csv(tmp, index=False)
    os.replace(tmp, args.out_csv)
    print(f"Saved: {args.out_csv}  shape={out_df.shape}  (N={E.shape[0]}, D={E.shape[1]})")


if __name__ == "__main__":
    main()
