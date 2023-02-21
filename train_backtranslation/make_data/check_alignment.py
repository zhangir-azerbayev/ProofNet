import ndjson
import json 

from tqdm import tqdm

with open("docgen_export_full/docgen_export_full.json") as f: 
    left = json.load(f)["decls"]


with open("docgen_export_with_nl/docgen_export_with_nl.jsonl") as f: 
    right = ndjson.load(f)

hashmap = {}
for x in right:
    if x["name"] in hashmap: 
        raise AssertionError("Duplicate name: {}".format(x["name"]))
    hashmap[x["name"]] = x["nl_statement_of_codex"]

i = 0 
for x in tqdm(left): 
    if x["kind"]=="theorem":
        if x["name"] not in hashmap: 
            i += 1
        else: 
            x["nl_statement_of_codex"] = hashmap[x["name"]]

print("DONE: dropped", i)
