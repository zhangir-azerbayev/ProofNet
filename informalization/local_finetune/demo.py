import sys
import os

import transformers 
from transformers import AutoModelForCausalLM, AutoTokenizer

MODEL_PATH = "runs/proofGPT-1.3B/checkpoint-15000"
MODEL_NAME = "hoskinson-center/proofGPT-v0.1"

def main(): 
    tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME)
    tokenizer.pad_token=tokenizer.eos_token

    model = AutoModelForCausalLM.from_pretrained(MODEL_PATH)
    model.eval()

    device = input("enter device index: ")
    device = f"cuda:{device}"
    model.to(device)

    while True: 
        text = input("\nEnter a theorem statement:\n").strip()

        text += "<SEP>"

        tokens = tokenizer(text, return_tensors="pt").to(device)

        output = model.generate(input_ids=tokens["input_ids"], attention_mask=tokens["attention_mask"], do_sample=False, max_new_tokens=150)

        decoded_texts = tokenizer.batch_decode(output, skip_special_tokens=False)[0]

        decoded_texts = decoded_texts[len(text)+2:]

        decoded_texts = decoded_texts.replace("<|endoftext|>", "")

        print("\n\n" + decoded_texts)

if __name__=="__main__": 
    main()
