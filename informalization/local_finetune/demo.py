import sys
import os

import transformers 
from transformers import GPTNeoForCausalLM, GPT2Tokenizer

MODEL_PATH = "runs/gptneo1B-train/checkpoint-5000"
MODEL_NAME = "gpt-neo-1.3B"

def main(): 
    tokenizer = GPT2Tokenizer.from_pretrained(MODEL_NAME)
    tokenizer.add_special_tokens({'pad_token': '<|pad|>', 
        'sep_token': '[SEP]'})

    model = GPTNeoForCausalLM.from_pretrained(MODEL_PATH).to("cuda")


    while True: 
        text = input("Enter a theorem statement: ").strip()

        text += "[SEP]"

        tokens = tokenizer(text, return_tensors="pt").to("cuda")

        output = model.generate(**tokens, do_sample=False, max_new_tokens=150)

        decoded_texts = tokenizer.batch_decode(output, skip_special_tokens=False)

        print(decoded_texts)


