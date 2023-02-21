import ndjson 
import re
from transformers import GPT2Tokenizer 

tokenizer = GPT2Tokenizer.from_pretrained("gpt2")

with open("nl_code_pairs/v2.jsonl") as f: 
    data = ndjson.load(f)


a = [x["nl_statement_of_codex"] for x in data]

b = [x["formal_statement"] for x in data]


a_tokens = tokenizer(a, padding='do_not_pad')

b_tokens = tokenizer(b, padding='do_not_pad')


a_lens = sum(len(x) for x in a_tokens['input_ids'])
b_lens = sum(len(x) for x in b_tokens['input_ids'])

print(a_lens + b_lens)
