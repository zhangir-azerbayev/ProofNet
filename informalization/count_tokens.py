import ndjson 
from transformers import GPT2Tokenizer 

tokenizer = GPT2Tokenizer.from_pretrained("gpt2")

with open("raw_mathlib_data.json") as f: 
    data = ndjson.load(f)


a = [x["nl_statement_of_codex"] for x in data][0:3]

b = [x["formal_statement"] for x in data][0:3]

print(a)
print(b)

a_tokens = tokenizer(a)

b_tokens = tokenizer(b)

a_lens = sum(len(x["input_ids"]) for x in a_tokens)
b_lens = sum(len(x["input_ids"]) for x in b_tokens)

print(a_tokens + b_tokens)
