import sys
from transformers import AutoModelForCausalLM, AutoTokenizer
from datasets import load_dataset
import torch
from tqdm import tqdm
import json
import pathlib
import os
import yaml


def filter_arxiv(x):
    meta = json.loads(x["meta"])
    if "config" in meta:
        return meta["config"] == "arxiv"
    else:
        return False


def main():
    with open(sys.argv[1]) as f:
        cfg = yaml.safe_load(f)

    device = cfg["device"]
    model_id = cfg["model"]
    subset = cfg["subset"]

    print("loading model")
    model = AutoModelForCausalLM.from_pretrained(model_id).to(device)
    tokenizer = AutoTokenizer.from_pretrained(model_id)

    print("loading dataset...")
    print("LOADING DATASET...")
    test = load_dataset("hoskinson-center/proof-pile", split="test")

    print(test)

    print("filtering dataset (if necessary)...")
    if subset == "arxiv":
        test = test.filter(filter_arxiv)
    
    print("tokenizing dataset...")
    test = test.map(
        lambda batch: {"input_ids": tokenizer(batch["text"])["input_ids"]},
        batched=True,
    )
    
    double_newline = tokenizer("\n\n")["input_ids"]
    
    print("making long sequence")
    tokens = [token for seq in tqdm(test["input_ids"]) for token in seq + double_newline]

    max_length = model.config.n_positions
    seq_len = encodings.input_ids.size(1)
    stride = seq_len

    nlls = []
    arxiv_nlls = []
    prev_end_loc = 0
    for begin_loc in tqdm(range(0, seq_len, stride)):
        end_loc = min(begin_loc + max_length, seq_len)
        trg_len = end_loc - prev_end_loc  # may be different from stride on last loop
        current_tokens = tokens[begin_loc:end_loc]
        input_ids = torch.tensor([current_tokens]).to("device")
        print(input_ids.shape)
        sys.exit()

        target_ids = input_ids.clone()
        target_ids[:, :-trg_len] = -100

        with torch.no_grad():
            outputs = model(input_ids, labels=target_ids)

            # loss is calculated using CrossEntropyLoss which averages over input tokens.
            # Multiply it with trg_len to get the summation instead of average.
            # We will take average over all the tokens to get the true average
            # in the last step of this example.
            neg_log_likelihood = outputs.loss * trg_len

        nlls.append(neg_log_likelihood)

        prev_end_loc = end_loc
        if end_loc == seq_len:
            break

    ppl = torch.exp(torch.stack(nlls).sum() / end_loc)
    print("ppl: ", ppl)


if __name__ == "__main__":
    main()
