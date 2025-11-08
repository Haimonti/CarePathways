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

def mean_pool(last_hidden_state, attention_mask):
    mask = attention_mask.unsqueeze(-1).expand(last_hidden_state.size()).float()
    summed = torch.sum(last_hidden_state * mask, dim=1)
    counts = torch.clamp(mask.sum(dim=1), min=1e-9)
    return summed / counts

def cls_pool(last_hidden_state, attention_mask):
    return last_hidden_state[:, 0, :]

@torch.inference_mode()
def embed_texts(texts, model_name, batch_size=16, max_len=256, pooling="mean", device=None):
    tok = AutoTokenizer.from_pretrained(model_name, use_fast=True)
    mdl = AutoModel.from_pretrained(model_name)
    mdl.to(device).eval()

    pool_fn = mean_pool if pooling.lower() == "mean" else cls_pool
    outs = []

    for s in range(0, len(texts), batch_size):
        batch = texts[s:s + batch_size]
        enc = tok(batch, padding=True, truncation=True, max_length=max_len, return_tensors="pt").to(device)
        out = mdl(**enc)
        pooled = pool_fn(out.last_hidden_state, enc["attention_mask"])
        outs.append(pooled.detach().cpu().float().numpy())

    return np.vstack(outs)




def main():
    ap = argparse.ArgumentParser("DischargeBERT embeddings → dischargebert_*")
    ap.add_argument("--csv", required=True)
    ap.add_argument("--text_col", default="")
    ap.add_argument("--id_cols", default="subject_id,hadm_id")
    ap.add_argument("--out_csv", required=True)
    ap.add_argument("--model", default="emilyalsentzer/Bio_Discharge_Summary_BERT")
    ap.add_argument("--batch_size", type=int, default=16)
    ap.add_argument("--max_len", type=int, default=256)
    ap.add_argument("--pooling", choices=["mean","cls"], default="mean")
    ap.add_argument("--threads", type=int, default=os.cpu_count() or 8)
    ap.add_argument("--prefix", default="dischargebert")
    args = ap.parse_args()

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print(f"using device: {device}")

    set_threading(args.threads)

    df = pd.read_csv(args.csv)

    if args.text_col and args.text_col in df.columns:
        text_col = args.text_col
    else:
        candidates = [c for c in df.columns if any(k in c.lower() for k in ["brief","course","note","summary","text","complaint"])]
        if len(candidates) == 1:
            text_col = candidates[0]
            print(f"detected text column automatically: {text_col}")
        else:
            print(f"ERROR: specify text column. columns: {list(df.columns)}")
            sys.exit(1)



    id_cols = [c.strip() for c in args.id_cols.split(",") if c.strip() and c in df.columns]
    out_df = df[id_cols].copy() if id_cols else pd.DataFrame(index=df.index)

    texts = df[text_col].fillna("").astype(str).apply(preprocess_text).tolist()
    print(f"model: {args.model} , pooling: {args.pooling} , text_col={text_col}")

    E = embed_texts(
        texts=texts,
        model_name=args.model,
        batch_size=args.batch_size,
        max_len=args.max_len,
        pooling=args.pooling,
        device=device
    )

    for i in range(E.shape[1]):
        out_df[f"{args.prefix}_{i}"] = E[:, i]

    tmp = args.out_csv + ".tmp"
    out_df.to_csv(tmp, index=False)
    os.replace(tmp, args.out_csv)
    print(f"saved: {args.out_csv} shape={out_df.shape}")

if __name__ == "__main__":
    main()
