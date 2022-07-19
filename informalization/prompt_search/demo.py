import sys 
import json
import ndjson 
import numpy as np 
import faiss 
import openai

def make_prompt(nl, entries): 
    prompt = ""
    for x in entries: 
        prompt += "Natural language version: \"" + x["nl_statement_of_codex"] + "\". Translate the natural language version to a Lean mathlib version:\n" + x["formal_statement"] + ":=\n\n"

    prompt += "Natural language version: \"" + nl + "\". Translate the natural language version to a Lean mathlib version:\ntheorem"

    return prompt

D = 4096
K = 4

database = np.load("mathlib_train_nl_embeddings/embeddings_full.npy").astype('float32')

with open("../nl_code_pairs/v2.jsonl") as f: 
    full_data = ndjson.load(f)  

print("database loaded")

print(database.shape)

index = faiss.IndexFlatL2(D)
index.add(database)                  
print("index prepared")

while True: 
    nl = input("Enter a natural language theorem statement: ")
    nl = nl.strip()

    vec = openai.Embedding.create(
            input=nl, 
            model = "text-similarity-curie-001", 
            )["data"][0]["embedding"]

    np_vec = np.expand_dims(np.array(vec).astype('float32'), axis=0)

    _, I = index.search(np_vec, K)

    idxs = np.squeeze(I).tolist()

    entries = [full_data[i] for i in idxs]
    
    print("RETRIEVED PROMPT" + "#"*40)
    for x in entries: 
        print(x["nl_statement_of_codex"])
        print(x["formal_statement"])
        print("\n\n")

    print("GENERATED FORMAL STATEMENT" + "#"*40)
    completion = openai.Completion.create(
            engine="code-davinci-002", 
            prompt=make_prompt(nl, entries),
            max_tokens=150, 
            stop=":=", 
            temperature=0, 
            )

    statement = "theorem " + completion["choices"][0]["text"]
    print("\n" + nl + "\n")
    print(statement)
    print("\n\n")
