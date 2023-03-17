import sys 
import os
from tqdm import tqdm
import json
import yaml
import ndjson
import pathlib

from utils import *

from datasets import load_dataset


def main():
    with open(sys.argv[1]) as f: 
        cfg = yaml.safe_load(f)

    BATCH_SIZE = cfg["batch_size"]
    BEFORE_EXAMPLE = cfg["before_example"]
    AFTER_EXAMPLE = cfg["after_example"]
    IN_KEY = cfg["in_key"]
    OUT_KEY = cfg["out_key"]
    if "endpoint" in cfg: 
        endpoint = cfg["endpoint"]
        chat = cfg["chat"]
    else: 
        endpoint="code-davinci-002"
    ref_key = cfg["ref_key"]
    save_dir = cfg["save_dir"]
    save_file = cfg["save_file"]
    few_shot_prompt_path = cfg["few_shot_prompt_path"]
    STOP = cfg["stop"]
    max_tokens = cfg["max_tokens"]
    split = cfg["split"]

    if os.path.isdir(save_dir):
        raise AssertionError("Save file already exists")
    pathlib.Path(save_dir).mkdir(parents=True) 
    
    with open(few_shot_prompt_path) as f: 
        if ".txt" in few_shot_prompt_path: 
            FEW_SHOT_PROMPT = f.read()
        elif ".json" in few_shot_prompt_path:
            FEW_SHOT_PROMPT = json.load(f)
            print("FEW SHOT PROMPT:")
            for line in FEW_SHOT_PROMPT: 
                print(line["content"])
        else: 
            raise ValueError("few_shot_prompt_path invalid")

    data = load_dataset("hoskinson-center/proofnet")[split]
    data = [x for x in data]

    dataloader = batch_loader(data, BATCH_SIZE)

    print(f"ENDPOINT: {endpoint}")
    
    # generation loop
    for batch in tqdm(dataloader): 
        prompts = [FEW_SHOT_PROMPT + BEFORE_EXAMPLE + x[IN_KEY] + AFTER_EXAMPLE for x in batch]

        print("calling api...")
        outs = call_api(prompts, stop=STOP, max_tokens=max_tokens, endpoint=endpoint, chat=chat)

        finish_reasons = [x["finish_reason"] 
                for x in outs["choices"]]
        if "length" in finish_reasons: 
            print("HIT LENGTH LIMIT, RETRYING WITH MORE TOKENS")
            outs = call_api(prompts, stop=STOP, max_tokens=400, endpoint=endpoint, chat=chat)

        text_outs = [x["text"] for x in outs["choices"]]

        for text_out, step, prompt in zip(text_outs, batch, prompts):
            print("TEXT" + "#"*20)
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
