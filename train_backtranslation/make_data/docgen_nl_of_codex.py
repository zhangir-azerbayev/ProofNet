import sys 
import os
from tqdm import tqdm
import json
import ndjson
import openai

from ratelimit import limits, sleep_and_retry

def batch_loader(seq, size):
    """
    Iterator that takes in a list `seq` and returns
    chunks of size `size`
    """
    return [seq[pos : pos + size] for pos in range(0, len(seq), size)]

# calls=3, period=60 is max for reliability with batch_size=20
# might want to throttle it to keep lean chat up
@sleep_and_retry
@limits(calls=1, period=60)
def call_api(prompt):
    return openai.Completion.create(
        engine="code-davinci-002",
        prompt=prompt,
        max_tokens=150,
        n=1,
        temperature=0,
        stop="\"",
    )


def main():
    BATCH_SIZE = 20
    BEFORE_THEOREM = "\nLean mathlib version:\n"
    AFTER_THEOREM = "\nTranslate the Lean mathlib version to a natural language version:\n\""
    
    # Run with save_path = "docgen_export_with_nl/docgen_export_with_nl.jsonl"
    save_path = sys.argv[1]

    with open("few_shot_prompt.txt") as f: 
        FEW_SHOT_PROMPT = f.read()

    with open("docgen_export_parsed/docgen_export_full_parsed.jsonl") as f:
        data = ndjson.load(f)

    dataloader = batch_loader(data, BATCH_SIZE)

    for batch in tqdm(dataloader): 
        prompts = [FEW_SHOT_PROMPT + BEFORE_THEOREM + x["formal_statement"] + AFTER_THEOREM for x in batch]

        outs = call_api(prompts)

        finish_reasons = [x["finish_reason"] 
                for x in outs["choices"]]
        if "length" in finish_reasons: 
            outs = call_api(prompts, max_tokens=400)

        text_outs = [x["text"] for x in outs["choices"]]

        for text_out, step in zip(text_outs, batch):
            step["nl_statement_of_codex"] = text_out

            with open(save_path, "a+") as f: 
                record = json.dumps(step)
                f.write(record+"\n")

if __name__=="__main__": 
    main()
