import sys
import os
import yaml
import ndjson 
from tqdm import tqdm 

from dataset import NlFormalDataset, data_collator

import torch 
import torch.nn
from torch.optim import AdamW 

import transformers 
from transformers import GPTNeoForCausalLM, GPT2Tokenizer
from transformers import TrainingArguments, Trainer
from transformers import get_cosine_schedule_with_warmup
from transformers.trainer_pt_utils import get_parameter_names 

def main(): 
    config_path = sys.argv[1] 

    with open(config_path) as f: 
        cfg = yaml.safe_load(f)

    train_data_path = '../finetune_data/v2_finetune.jsonl'
    valid_data_path = '../finetune_data/v2_finetune_valid.jsonl'

    os.environ["CUDA_VISIBLE_DEVICES"] = cfg['device']
    experiment_name = cfg['experiment_name']
    train_path = cfg['train_path']
    eval_path = cfg['eval_path']
    lr = cfg['lr']
    lr_decay = cfg['lr_decay']
    warmup_steps = cfg['warmup_steps']
    weight_decay = cfg['weight_decay']
    gradient_clipping = cfg['gradient_clipping']
    train_steps = cfg['train_steps']
    logging_steps = cfg['logging_steps']
    save_steps = cfg['save_steps']
    batch_size = cfg['batch_size']
    model_name = cfg['model_name']
    max_length = cfg['max_length']

    save_dir = os.join("runs/", experiment_name)

    os.mkdir(save_dir)

    with open(train_path) as f: 
        train_data = ndjson.load(f)

    with open(eval_path) as f:
        eval_data = ndjson.load(f)

    # Tokenizers and data 
    tokenizer = GPT2Tokenizer.from_pretrained(model_name)
    tokenizer.add_special_tokens({'pad_token': '<|pad|>', 
        'sep_token': '[SEP]']})

    train_set = NlFormalDataset(train_data, tokenizer, max_length)
    eval_set = NlFormalDataset(eval_data, tokenizer, max_length)

    # Models 
    model = GPTNeoForCausalLM.from_pretrained(model_name)

    # Initialize optimizer and scheduler

    optimizer = AdamW(optimizer_grouped_parameters, 
            lr=lr)

    scheduler = get_cosine_schedule_with_warmup(
            optimizer, 
            num_warmup_steps=warmup_steps,
            num_training_steps=train_steps)

    training_args = TrainingArguments(
            output_dir = save_dir,
            max_steps = train_steps,
            per_device_train_batch_size = batch_size,
            logging_strategy= "steps", 
            logging_steps = logging_steps, 
            save_strategy = "steps",
            save_steps = save_steps,
            evaluation_strategy = "steps",
            eval_steps = eval_steps,
            )

    with open(os.join(save_dir, "config.yaml"), "w") as f: 
        yaml.dump(cfg, f)

    Trainer(model=model, args=training_args, 
            train_dataset=train_set, eval_dataset=eval_set,
            data_collator=data_collator, 
            optimizers=(optimizer, scheduler),).train()

if __name__=="__main__": 
    main()
