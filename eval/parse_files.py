import os
import re 
import json
import ndjson 
from functools import lru_cache

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
            informal_list = list(f.readlines())

        i = 0  
        while i < len(informal_list): 
            line = informal_list[i]
            if re.match(r"\\paragraph\{.*?\} .*", line): 
                informal = line
                eyed = name.replace(".tex", "") + "|" + informal[informal.index("{")+1:informal.index("}")].lower().replace(" ", "_").replace(".", "_")
                nl = informal[informal.index("} ")+2:]
            
                i += 1 

                if re.match(r"\\begin\{proof\}", informal_list[i]):
                    rexp = re.compile(r"\\begin\{proof\}.*?\\end\{proof\}", re.DOTALL)
                    nl_proof = re.match(rexp, "\n".join(informal_list[i:])).group(0)
                else: 
                    nl_proof = ""

                informal_dict[eyed] = {"nl_statement": nl, "nl_proof": nl_proof}
            i += 1


@lru_cache(maxsize=None)
def header_of_author(source_name): 
    with open("../benchmark/formal/" + source_name + ".lean") as f: 
        text = f.read()

    rexp = re.compile("^.*?theorem", re.DOTALL)
    header = re.match(rexp, text).group(0)[:-7]

    return header

def header_of_id(eyed): 
    return header_of_author(eyed[:eyed.index("|")])

out_file = "model_input_new1.jsonl"
if False: #os.path.isfile(out_file): 
    with open(out_file) as f: 
        old_data = ndjson.load(f)

    old_data_ids = set([x["id"] for x in old_data])
    for eyed in formal_dict.keys(): 
        if eyed in informal_dict.keys() and eyed not in old_data_ids: 
            old_data.append({"id": eyed, "formal_statement": formal_dict[eyed], 
                "src_header": header_of_id(eyed), **informal_dict[eyed]})
    
    with open(out_file, "w") as f: 
        ndjson.dump(old_data, f)

else: 
    data = []
    for eyed in formal_dict.keys(): 
        if eyed in informal_dict.keys():
            data.append({"id": eyed, "formal_statement": formal_dict[eyed], 
                "src_header": header_of_id(eyed), **informal_dict[eyed]})
        with open(out_file, "w") as f: 
            f.write(json.dumps(data, indent=4))

