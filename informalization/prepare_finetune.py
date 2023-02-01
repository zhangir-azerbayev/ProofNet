# Don't use this until OpenAI releases codex finetuning api
import ndjson 

with open("nl_code_pairs/v2.jsonl", "r") as f:
    data = ndjson.load(f)

lines = [{
    "prompt": x["nl_statement_of_codex"] + "[SEP]", 
    "completion": x["formal_statement"] + " :=", 
    } for x in data]


with open("finetune_data/v2_finetune.jsonl", "w") as f: 
    ndjson.dump(lines, f)


[print(x["completion"]) for x in lines[0:20]]
