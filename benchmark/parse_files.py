import os
import re
import json
import ndjson
from functools import cache

FORMAL_DIR = "benchmark_to_publish/formal"
INFORMAL_DIR = "benchmark_to_publish/informal"
TEST_SAVE_PATH = "test.jsonl"
VALID_SAVE_PATH = "valid.jsonl"


def get_formal_dict(formal_dir=FORMAL_DIR):
    formal_dict = {}
    formal_exp = re.compile(r"theorem.*?:=", re.DOTALL)

    for name in os.listdir(formal_dir):
        if name.endswith(".lean"):
            with open(os.path.join(formal_dir, name)) as f:
                formal_str = f.read()

            formal_matches = re.findall(formal_exp, formal_str)
            for formal in formal_matches:
                eyed = (
                    name.replace(".lean", "")
                    + "|"
                    + re.search(r"exercise\S+", formal).group(0)
                )
                formal_dict[eyed] = formal

    return formal_dict

def get_informal_dict(informal_dir=INFORMAL_DIR):
    informal_dict = {}
    for name in os.listdir(informal_dir):
        if name.endswith(".tex"):
            with open(os.path.join(informal_dir, name)) as f:
                informal_list = list(f.readlines())

            i = 0
            while i < len(informal_list):
                line = informal_list[i]
                if re.match(r"\\paragraph\{.*?\} .*", line):
                    informal = line
                    eyed = (
                        name.replace(".tex", "")
                        + "|"
                        + informal[informal.index("{") + 1 : informal.index("}")]
                        .lower()
                        .replace(" ", "_")
                        .replace(".", "_")
                    )
                    nl = informal[informal.index("} ") + 2 :]

                    i += 1

                    if re.match(r"\\begin\{proof\}", informal_list[i]):
                        rexp = re.compile(r"\\begin\{proof\}.*?\\end\{proof\}", re.DOTALL)
                        nl_proof = re.match(rexp, "\n".join(informal_list[i:])).group(0)
                    else:
                        nl_proof = ""

                    informal_dict[eyed] = {"nl_statement": nl.strip(), "nl_proof": nl_proof.strip()}
                i += 1

    return informal_dict


@cache
def header_of_author(source_name):
    with open(os.path.join(FORMAL_DIR, source_name + ".lean")) as f:
        text = f.read()

    rexp = re.compile("^.*?theorem", re.DOTALL)
    header = re.match(rexp, text).group(0)[:-7]

    return header


def header_of_id(eyed):
    return header_of_author(eyed[: eyed.index("|")])


def main(): 
    formal_dict = get_formal_dict()
    informal_dict = get_informal_dict()

    data = []
    for eyed in formal_dict.keys():
        if eyed in informal_dict.keys():
            data.append(
                {
                    "id": eyed,
                    "formal_statement": formal_dict[eyed],
                    "src_header": header_of_id(eyed),
                    **informal_dict[eyed],
                }
            )

    with open("test_ids") as f: 
        test_ids = [x.strip() for x in f.readlines()]
    with open("valid_ids") as f: 
        valid_ids = [x.strip() for x in f.readlines()]
    
    test = [x for x in data if x["id"] in test_ids]
    valid = [x for x in data if x["id"] in valid_ids]

    with open(TEST_SAVE_PATH, "w") as f: 
        ndjson.dump(test, f)

    with open(VALID_SAVE_PATH, "w") as f: 
        ndjson.dump(valid, f)

if __name__=="__main__": 
    main()
