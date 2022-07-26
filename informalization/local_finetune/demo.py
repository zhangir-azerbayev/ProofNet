import sys
import os

import transformers 
from transformers import GPTNeoForCausalLM, GPT2Tokenizer

MODEL_PATH = "runs/gptneo1B_train/checkpoint-5000"
MODEL_NAME = "EleutherAI/gpt-neo-1.3B"

def main(): 
    tokenizer = GPT2Tokenizer.from_pretrained(MODEL_NAME)
    tokenizer.add_special_tokens({'pad_token': '<|pad|>', 
        'sep_token': '[SEP]'})

    model = GPTNeoForCausalLM.from_pretrained(MODEL_PATH).to("cuda")
    model.eval()


    while True: 
        text = input("\nEnter a theorem statement:\n").strip()

        text += "[SEP]"

        tokens = tokenizer(text, return_tensors="pt").to("cuda")

        output = model.generate(**tokens, do_sample=False, max_new_tokens=150, pad_token_id=tokenizer.pad_token_id)

        decoded_texts = tokenizer.batch_decode(output, skip_special_tokens=False)[0]

        decoded_texts = decoded_texts[len(text)+2:]

        decoded_texts = decoded_texts.replace("<|endoftext|>", "")

        print("\n\n" + decoded_texts)

if __name__=="__main__": 
    main()
