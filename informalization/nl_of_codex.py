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

@sleep_and_retry
@limits(calls=2, period=60)
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
    BATCH_SIZE = 10
    BEFORE_THEOREM = "\nLean mathlib version:\n"
    AFTER_THEOREM = " :=\nTranslate the Lean mathlib version to a natural language version:\n\""

    save_path = sys.argv[1]
    if os.path.isfile(save_path): 
        raise OSError("save_path already exists")

    with open("few_shot_prompt.txt") as f: 
        FEW_SHOT_PROMPT = f.read()

    with open("raw_mathlib_data.json") as f:
        data = ndjson.load(f)

    dataloader = batch_loader(data, BATCH_SIZE)

    for batch in tqdm(dataloader): 
        prompts = [FEW_SHOT_PROMPT + BEFORE_THEOREM + x["formal_statement"] + AFTER_THEOREM for x in batch]

        outs = call_api(prompts)

        text_outs = [x["text"] for x in outs["choices"]]

        for text_out, step in zip(text_outs, batch):
            print("text out: ", text_out)
            print("step: ", step)
            step["nl_statement_of_codex"] = text_out

            with open(save_path, "a+") as f: 
                record = json.dumps(step)
                f.write(record+"\n")



if __name__=="__main__": 
    main()
