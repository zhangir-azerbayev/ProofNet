from typing import List, Dict

import torch 
from torch.utils.data import Dataset
from transformers import PreTrainedTokenizer

def data_collator(data): 
    return {'input_ids': torch.stack([f[0] for f in data]), 
            'attention_mask': torch.stack([f[1] for f in data]),
            'labels': torch.stack([f[0] for f in data])}

class NlFormalDataset(Dataset): 
    def __init__(self, data: List[Dict[str, str]], tokenizer: PreTrainedTokenizer, max_len: int): 
        self.data = data 
        self.tokenizer = tokenizer
        self.max_len = max_len

        if tokenizer.pad_token is None: 
            raise ValueError("Please use a tokenizer with a pad token")
        if tokenizer.eos_token is None: 
            raise ValueError("Please use a tokenizer with an eos token")
        if tokenizer.sep_token is None: 
            raise ValueError("Please use a tokenizer with a sep token")

    def __getitem__(self, index):
        datum = self.data[index]
        text = datum['prompt'] + datum['completion'] + tokenizer.eos_token
        toks = self.tokenizer(text, truncation=True,
                padding="max_length", max_length=self.max_len)

        return toks["input_ids"], toks["attention_mask"]

    def __len__(self): 
        return len(self.data)
