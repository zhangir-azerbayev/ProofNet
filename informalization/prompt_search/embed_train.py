import ndjson 
import json 
import sys 
import os

from tqdm import tqdm

import numpy as np 

import openai 

def batch_loader(seq, size):
    """
    Iterator that takes in a list `seq` and returns
    chunks of size `size`
    """
    return [seq[pos:pos + size] for pos in range(0, len(seq), size)]


with open("../nl_code_pairs/v2.jsonl") as f: 
    data = ndjson.load(f)

log_dir = "mathlib_train_nl_embeddings/"

if os.path.isfile(os.path.join(log_dir, "shard_0.jsonl")):
    raise AssertionError("log_dir already contains shards")

for shard, batch in tqdm(enumerate(batch_loader(data, 100))): 
    texts = [x["nl_statement_of_codex"] for x in batch]
    ids = [x["task_id"] for x in batch]

    responses = openai.Embedding.create(
        input=texts, 
        model="text-similarity-curie-001", 
        )
    
    log = []
    for text, i, response in zip(texts, ids, responses["data"]): 
        assert data[i]["task_id"] == i 

        to_log = {
            "task_id": i,
            "embedding": response["embedding"]
            }
        log.append(to_log)

    with open(os.path.join(log_dir, f"shard_{shard}.jsonl"), "w") as f:
        ndjson.dump(log, f)
