import sys 
import os 
from tqdm import tqdm
import json
import yaml
import ndjson
import pathlib

import torch

from transformers import AutoTokenizer, AutoModelForCausalLM
from datasets import load_dataset
from utils import *

device = "cuda"

def call_gpt(prompts, model, tokenizer, max_tokens, stop):
    """
    Return a list of strings
    """
    encoded_texts = tokenizer(prompts, 
            return_tensors="pt", 
            max_length = len(tokenizer(prompts[0])['input_ids']),
            truncation=True, 
            ).to(device)


    outputs = model.generate(**encoded_texts, 
            max_new_tokens=max_tokens, 
            ).cpu()
 
    untrunced_bodies = [tokenizer.decode(x, skip_special_tokens=True)
            for x in outputs]

    trunced_bodies = [y[len(x):y.index(stop)] for x, y in zip(prompts, untrunced_bodies)]

    return trunced_bodies


def main():
    with open(sys.argv[1]) as f: 
        cfg = yaml.safe_load(f)

    BATCH_SIZE = cfg["batch_size"]
    IN_KEY = cfg["in_key"]
    OUT_KEY = cfg["out_key"]
    save_dir = cfg["save_dir"]
    save_file = cfg["save_file"]
    max_tokens = cfg["max_tokens"]
    model_path = cfg["model"]
    hfmodel = cfg["hfmodel"]
    ref_key = cfg["ref_key"]
    STOP = cfg["stop"]
    split = cfg["split"]

    if os.path.isdir(save_dir): 
        raise AssertionError("directory already exists")
    pathlib.Path(save_dir).mkdir(parents=True) 
    

    # set up language model
    print("loading tokenizer...")
    tokenizer = AutoTokenizer.from_pretrained(hfmodel)
    tokenizer.pad_token_id=tokenizer.eos_token_id

    torch.cuda.empty_cache()
    torch.cuda.set_per_process_memory_fraction(1.0)

    print("loading model...")
    model = AutoModelForCausalLM.from_pretrained(
            model_path 
            ).to(device)
    print("done loading model")

    data = load_dataset("hoskinson-center/proofnet")["test"]
    data = [x for x in data]

    dataloader = batch_loader(data, BATCH_SIZE)

    for batch in tqdm(dataloader): 
        prompts = [x[IN_KEY].strip() + '<SEP>theorem' for x in batch]

        outs = call_gpt(prompts, model, tokenizer, max_tokens, STOP)

        text_outs = outs

        for text_out, step, prompt in zip(text_outs, batch, prompts):
            print(prompt + text_out)
            step[OUT_KEY] = text_out
            step["prompt"] = prompt

            with open(os.path.join(save_dir, save_file), "a+") as f: 
                record = json.dumps(step)
                f.write(record+"\n")

    # calculates bleu score and saves to metrics.json
    with open(os.path.join(save_dir, save_file)) as f: 
        data = ndjson.load(f)

    bleu = calc_bleu(data, OUT_KEY, ref_key)

    with open(os.path.join(save_dir, "metrics.json"), "w") as f: 
        json.dump({"bleu": bleu}, f)

    make_readable(save_dir, save_file, ref_key)

if __name__=="__main__": 
    main()
