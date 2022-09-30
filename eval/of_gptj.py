import sys 
import os
from tqdm import tqdm
import json
import yaml
import ndjson
import pathlib

import torch

from transformers import AutoTokenizer, GPTJForCausalLM, AutoModelForCausalLM

device = "cuda:0"


def batch_loader(seq, size):
    """
    Iterator that takes in a list `seq` and returns
    chunks of size `size`
    """
    return [seq[pos : pos + size] for pos in range(0, len(seq), size)]


def call_gptj(prompts, model, tokenizer, stop):
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
            pad_token_id = 50270,
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

    BATCH_SIZE = cfg["batch_size"]
    BEFORE_EXAMPLE = cfg["before_example"]
    AFTER_EXAMPLE = cfg["after_example"]
    IN_KEY = cfg["in_key"]
    OUT_KEY = cfg["out_key"]
    save_dir = cfg["save_dir"]
    save_file = cfg["save_file"]
    pathlib.Path(save_dir).mkdir(parents=True, exist_ok=True) 
    few_shot_prompt_path = cfg["few_shot_prompt_path"]
    data_path = cfg["data_path"]
    STOP = cfg["stop"]
    max_tokens = cfg["max_tokens"]

    with open(few_shot_prompt_path) as f: 
        FEW_SHOT_PROMPT = f.read()

    # set up language model
    print("loading tokenizer...")
    tokenizer = AutoTokenizer.from_pretrained("EleutherAI/gpt-j-6B")
    tokenizer.truncation_side='left'

    prompt_len = len(tokenizer(FEW_SHOT_PROMPT)['input_ids'])
    print("PROMPT LEN: ", prompt_len)

    torch.cuda.empty_cache()
    torch.cuda.set_per_process_memory_fraction(1.0)

    print("loading model...")
    model = GPTJForCausalLM.from_pretrained(
            "EleutherAI/gpt-j-6B", revision="main", 
            ).to("cuda:0")
    print("done loading model")

    with open(data_path) as f:
        data = ndjson.load(f)

    if os.path.isfile(os.path.join(save_dir, save_file)): 
        print("WARNING: AUGMENTING EXISTING FILE WITH NEW IDs")
        with open(os.path.join(save_dir, save_file)) as f: 
            old_data = ndjson.load(f)

        old_data_ids = set([x["id"] for x in old_data])
        data = [x for x in data if x["id"] not in old_data_ids]

    dataloader = batch_loader(data, BATCH_SIZE)

    for batch in tqdm(dataloader): 
        prompts = [FEW_SHOT_PROMPT + BEFORE_EXAMPLE + x[IN_KEY] + AFTER_EXAMPLE for x in batch]

        outs = call_gptj(prompts, model, tokenizer, stop=STOP)

        text_outs = outs

        for text_out, step in zip(text_outs, batch):
            step[OUT_KEY] = text_out

            with open(os.path.join(save_dir, save_file), "a+") as f: 
                record = json.dumps(step)
                f.write(record+"\n")

if __name__=="__main__": 
    main()
