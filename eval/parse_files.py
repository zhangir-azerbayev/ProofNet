import os
import re 
import json
import ndjson 

formal_dict = {}
informal_dict = {}
formal_exp = re.compile(r"theorem.*?:=", re.DOTALL)

formal_dir = "../benchmark/formal"
for name in os.listdir(formal_dir): 
    if name.endswith(".lean"): 
        with open(os.path.join(formal_dir, name)) as f: 
            formal_str = f.read()

        formal_matches = re.findall(formal_exp, formal_str)
        for formal in formal_matches: 
            eyed = name.replace(".lean", "") + "|" + re.search(r"exercise\S+", formal).group(0)
            formal_dict[eyed] = formal 

informal_dir = "../benchmark/informal"
for name in os.listdir(informal_dir): 
    if name.endswith(".tex"): 
        with open(os.path.join(informal_dir, name)) as f: 
            informal_str = f.read()


        informal_matches = re.findall(r"\\paragraph\{.*?\} .*", informal_str)

        for informal in informal_matches: 
            eyed = name.replace(".tex", "") + "|" + informal[informal.index("{")+1:informal.index("}")].lower().replace(" ", "_").replace(".", "_")
            nl = informal[informal.index("} ")+2:]
            informal_dict[eyed] = nl


out_file = "model_input.jsonl"
if os.path.isfile(out_file): 
    with open(out_file) as f: 
        old_data = ndjson.load(f)

    old_data_ids = set([x["id"] for x in old_data])
    for eyed in formal_dict.keys(): 
        if eyed in formal_dict.keys() and eyed not in old_data_ids: 
            old_data.append({"id": eyed, "formal_statement": formal_dict[eyed], 
                "nl_statement": informal_dict[eyed]})
    
    with open(out_file, "w") as f: 
        ndjson.dump(old_data, f)

else: 
    data = []
    for eyed in formal_dict.keys(): 
        if eyed in informal_dict.keys():
            data.append({"id": eyed, "formal_statement": formal_dict[eyed], 
                "nl_statement": informal_dict[eyed]})
        with open(out_file, "w") as f: 
            ndjson.dump(data, f)

