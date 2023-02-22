import sys
from transformers import AutoModelForCausalLM, AutoTokenizer
from datasets import load_dataset
import torch
from tqdm import tqdm
import json
import pathlib
import os
import yaml
from itertools import islice


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
    dataset = cfg["dataset"]
    subset = cfg["subset"]
    batch_size = cfg["batch_size"]
    out_file = cfg["out_file"]

    if os.path.isfile(out_file): 
        raise AssertionError("outfile already a file")

    print("loading model")
    model = AutoModelForCausalLM.from_pretrained(model_id).to(device)
    tokenizer = AutoTokenizer.from_pretrained(model_id)

    print("loading dataset...")
    print("LOADING DATASET...")
    
    if dataset=="wikitext": 
        test = load_dataset("wikitext", "wikitext-2-raw-v1", split="test")
    else:  
        test = load_dataset(dataset, split="test")

    print("filtering dataset (if necessary)...")
    if subset == "arxiv":
        test = test.filter(filter_arxiv)
    
    if dataset == "hoskinson-center/proof-pile":
        test = test.shuffle(seed=42).select(list(range(1000))) # compute resources :(
 
    print("tokenizing dataset...")
    test = test.map(
        lambda batch: {"input_ids": tokenizer(batch["text"])["input_ids"]},
        batched=True,
    )

    print(test)

    pbar = tqdm(total=len(test))
    dataloader = iter(test)
    
    double_newline = tokenizer("\n\n")["input_ids"]
    
    seq_len = model.config.max_position_embeddings

    nlls = []
    buff = []
    i = 0 
    do_more = True
    while do_more: 
        while len(buff) < batch_size * seq_len: 
            try: 
                buff += double_newline + next(dataloader)["input_ids"]
                pbar.update(1)
            except StopIteration:
                break

        if len(buff) >= batch_size*seq_len: 
            input_ids = torch.tensor(buff[:batch_size*seq_len]).view(batch_size, seq_len)
            buff = buff[batch_size*seq_len:]
        else: 
            do_more = False
            reduced_batch_size = len(buff)//seq_len
            if reduced_batch_size==0: 
                continue
            else:
                input_ids = torch.tensor(buff[:reduced_batch_size*seq_len]).view(
                        reduced_batch_size, seq_len
                        )
        
        input_ids = input_ids.to(device)
        target_ids = input_ids.clone()

        with torch.no_grad():
            outputs = model(input_ids, labels=target_ids)

            nll = outputs.loss

        print(f"batch {i} nll: {nll}")
        i += 1
        nlls.append(nll)

    pbar.close()

    ppl = torch.exp(torch.tensor(nlls).mean())
    print("ppl: ", ppl)

    results_string = f"ppl: {ppl}\nnlls:" + "\n".join([str(x) for x in nlls])
    with open(out_file, "w") as f: 
        f.write(out_file)


if __name__ == "__main__":
    main()
