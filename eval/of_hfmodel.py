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


def batch_loader(seq, size):
    """
    Iterator that takes in a list `seq` and returns
    chunks of size `size`
    """
    return [seq[pos : pos + size] for pos in range(0, len(seq), size)]


def call_gpt(prompts, model, tokenizer, stop):
    """
    Return a list of strings
    """
    encoded_texts = tokenizer(prompts, 
            return_tensors="pt", 
            max_length = len(tokenizer(prompts[0])['input_ids']),
            truncation=True, 
            ).to(device)


    outputs = model.generate(**encoded_texts, 
            max_new_tokens=400, 
            pad_token_id = tokenizer.eos_token_id,
            ).cpu()
 
    untrunced_bodies = [tokenizer.decode(x, skip_special_tokens=True)
            for x in outputs]

    trunced_bodies = [y[len(x):] for x, y in zip(prompts, untrunced_bodies)]
    trunced_bodies = [y[:y.index(stop)] if stop in y else y for y in trunced_bodies]

    print("SOLUTION:")

    return trunced_bodies


def main():
    with open(sys.argv[1]) as f: 
        cfg = yaml.safe_load(f)
    
    hfmodel = cfg["hfmodel"]
    BATCH_SIZE = cfg["batch_size"]
    BEFORE_EXAMPLE = cfg["before_example"]
    AFTER_EXAMPLE = cfg["after_example"]
    IN_KEY = cfg["in_key"]
    OUT_KEY = cfg["out_key"]
    ref_key = cfg["ref_key"]
    save_dir = cfg["save_dir"]
    save_file = cfg["save_file"]
    few_shot_prompt_path = cfg["few_shot_prompt_path"]
    STOP = cfg["stop"]
    max_tokens = cfg["max_tokens"]
    split = cfg["split"]

    with open(few_shot_prompt_path) as f: 
        FEW_SHOT_PROMPT = f.read()

    if os.path.isdir(save_dir): 
        raise AssertionError("save directory already exist")
    pathlib.Path(save_dir).mkdir(parents=True) 

    # set up language model
    print("loading tokenizer...")
    tokenizer = AutoTokenizer.from_pretrained(hfmodel)
    tokenizer.truncation_side='left'

    prompt_len = len(tokenizer(FEW_SHOT_PROMPT)['input_ids'])
    print("PROMPT LEN: ", prompt_len)

    torch.cuda.empty_cache()
    torch.cuda.set_per_process_memory_fraction(1.0)

    print("loading model...")
    model = AutoModelForCausalLM.from_pretrained(
            hfmodel, 
            ).to(device)
    print("done loading model")

    data = load_dataset("hoskinson-center/proofnet")[split]
    data = [x for x in data]

    dataloader = batch_loader(data, BATCH_SIZE)

    # generation loop
    for batch in tqdm(dataloader[:10]): 
        prompts = [FEW_SHOT_PROMPT + BEFORE_EXAMPLE + x[IN_KEY] + AFTER_EXAMPLE for x in batch]

        outs = call_gpt(prompts, model, tokenizer, stop=STOP)

        text_outs = outs

        for text_out, step, prompt in zip(text_outs, batch, prompts):
            print("TEXT" + "#"*40)
            print(prompt + text_out)

            step[OUT_KEY] = text_out
            step['prompt'] = prompt

            with open(os.path.join(save_dir, save_file), "a+") as f: 
                record = json.dumps(step)
                f.write(record+"\n")

    # calculates bleu
    with open(os.path.join(save_dir, save_file)) as f: 
        data = ndjson.load(f)

    bleu = calc_bleu(data, OUT_KEY, ref_key)

    with open(os.path.join(save_dir, "metrics.json"), "w") as f: 
        json.dump({"bleu": bleu}, f)

    make_readable(save_dir, save_file, ref_key)

if __name__=="__main__": 
    main()
