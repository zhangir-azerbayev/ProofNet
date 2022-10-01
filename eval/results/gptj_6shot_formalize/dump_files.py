import ndjson
import re 
import os
from tqdm import tqdm

with open("out.jsonl") as f: 
    data = ndjson.load(f)

data = sorted(data, key= lambda x: x["id"])

for x in tqdm(data): 
    eyed = x["id"]
    filename = eyed[:eyed.index("|")]
    exname = eyed[eyed.index("|")+1:]
    
    filepath = filename+".lean"
    if os.path.isfile(filepath): 
        with open(filepath) as f: 
            existing = f.read()
    else: 
        existing = ""

    if not re.search("theorem " + exname, existing): 
        formal = x["gptj_formal_statement"][1:]
        if " " in formal: 
            formal = formal[formal.index(" "):]
        to_add = f"\n\ntheorem {exname}" + formal + ":=\nsorry"
        existing += to_add


    with open(filepath, "w") as f: 
        f.write(existing)




