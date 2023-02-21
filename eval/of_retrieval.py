import sys 
import os
from tqdm import tqdm
import json
import yaml
import ndjson
import pathlib
import numpy as np
import faiss

from .utils import *

BEFORE = \
"""\n\nYou are an expert Lean user. I am going to ask you to translate a natural language theorem statement into a Lean mathlib theorem statement. But first, I am going to show you four Lean formal statements from the same area of mathematics in order to refresh your memory of the mathlib API and make sure you are using it correctly.

Here are the four formal statements:\n\n"""

MIDDLE = \
"""\n\nThe following is the natural language theorem statement: \""""

AFTER = \
"""\" Translate the natural language version to a Lean mathlib version:"""

Here are the four formal statements: 

def create_vector_db(docs_path, vecs_path): 
    print(f"loading docs from {docs_path}")
    with open(docs_path) as f:
        docs = ndjson.load(f)

    print(f"loading embeddings from {vecs_path}")
    embeddings = np.load(vecs_path).astype("float32")

    # sanity checks
    assert D == embeddings.shape[1]
    assert embeddings.shape[0] == len(self.docs)

    print(f"Found {len(self.docs)} mathlib declarations")

    print("creating fast kNN database...")
    database = faiss.IndexFlatL2(D)
    database.add(embeddings)  
    return docs, database


def main():
    """
    This script *must* be run after `of_codex.py`, it uses its outputs. 
    """
    with open(sys.argv[1]) as f: 
        cfg = yaml.safe_load(f)

    docs_path = cfg["docs_path"]
    vecs_path = cfg["vecs_path"]
    codex_out_path = cfg["codex_out_path"]
    BATCH_SIZE = cfg["batch_size"]
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
        FEW_SHOT_PROMPT = f.read()

    data = ndjson.load(open(codex_out_path))

    dataloader = batch_loader(data, BATCH_SIZE)
    docs, database = create_vector_db(docs_path, vecs_path)
    
    # generation loop
    for batch in tqdm(dataloader): 
        y_hats = [x["gpt_formal_statement"] for x in batch]

        responses = openai.Embedding.create(
            input=y_hats, model="text-embedding-ada-002"
        )
        
        queries = np.stack(
            [np.array(x["embedding"]).astype("float32") for x in responses["data"]]
        )

        _, idxs_np = database.search(queries, K)  

        demonstrations = ["\n\n".join([y.item() for y in x]) for x in idxs_np]

        prompts = [FEW_SHOT_PROMPT + BEFORE + d + MIDDLE + y["nl_statement"] + AFTER 
                for d, y in zip(demonstrations, batch)]

        outs = call_api(prompts, stop=STOP, max_tokens=max_tokens)

        finish_reasons = [x["finish_reason"] 
                for x in outs["choices"]]
        if "length" in finish_reasons: 
            print("HIT LENGTH LIMIT, RETRYING WITH MORE TOKENS")
            outs = call_api(prompts, stop=STOP, max_tokens=400)

        text_outs = [x["text"] for x in outs["choices"]]

        for text_out, step, prompt in zip(text_outs, batch, prompts):
            step["gpt_original_formal_statement"] = step["gpt_formal_statement"]
            step["gpt_formal_statement"] = text_out

            step["original_prompt"] = step["prompt"]
            step["prompt"] = prompt

            with open(os.path.join(save_dir, save_file), "a+") as f: 
                record = json.dumps(step)
                f.write(record+"\n")

    # calculates bleu score and saves to metrics.json
    with open(os.path.join(save_dir, save_file)) as f: 
        data = ndjson.load(f)

    bleu = calc_bleu(data, "gpt_formal_statement", "formal_statement")

    with open(os.path.join(save_dir, "metrics.json"), "w") as f: 
        json.dump(f, {"bleu": bleu})

    make_readable(save_dir, save_file, ref_key)


if __name__=="__main__": 
    main()
