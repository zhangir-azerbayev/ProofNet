import sys
from transformers import AutoModelForCausalLM, AutoTokenizer
from datasets import load_dataset
import torch
from tqdm import tqdm
import json
import pathlib
import yaml

def main():
    with open(sys.argv[1]) as f: 
        cfg = yaml.safe_load(f)

    device = cfg["device"]
    model_id = cfg["model"]
    stride = cfg["stride"]
    subset = cfg["subset"]

    model = AutoModelForCausalLM.from_pretrained(model_id).to(device)
    tokenizer = AutoTokenizer.from_pretrained(model_id)

    test = load_dataset("hoskinson-center/proof-pile", split="test")

    for x in test: 
        print(x)
        sys.exit()

    if subset=="all": 
        texts = test["text"]
    elif subset=="arxiv": 
        texts = test.filter(lambda x: x["meta"])
    encodings = tokenizer("\n\n".join(texts), return_tensors="pt")

    max_length = model.config.n_positions
    seq_len = encodings.input_ids.size(1)

    nlls = []
    arxiv_nlls = []
    prev_end_loc = 0
    for begin_loc in tqdm(range(0, seq_len, stride)):
        end_loc = min(begin_loc + max_length, seq_len)
        trg_len = end_loc - prev_end_loc  # may be different from stride on last loop
        input_ids = encodings.input_ids[:, begin_loc:end_loc].to(device)
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

if __name__=="__main__": 
    main()
