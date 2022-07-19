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

    experiment_name = cfg['experiment_name']
    train_path = cfg['train_path']
    eval_path = cfg['eval_path']
    lr = cfg['lr']
    warmup_steps = cfg['warmup_steps']
    weight_decay = cfg['weight_decay']
    gradient_clipping = cfg['gradient_clipping']
    train_steps = cfg['train_steps']
    logging_steps = cfg['logging_steps']
    save_steps = cfg['save_steps']
    eval_steps = cfg['eval_steps']
    batch_size = cfg['batch_size']
    model_name = cfg['model_name']
    max_length = cfg['max_length']
    accum_steps = cfg['accum_steps']

    save_dir = os.path.join("runs/", experiment_name)

    os.mkdir(save_dir)

    with open(train_path) as f: 
        train_data = ndjson.load(f)

    with open(eval_path) as f:
        eval_data = ndjson.load(f)

    # Tokenizers and data 
    tokenizer = GPT2Tokenizer.from_pretrained(model_name)
    tokenizer.add_special_tokens({'pad_token': '<|pad|>', 
        'sep_token': '[SEP]'})

    train_set = NlFormalDataset(train_data, tokenizer, max_length)
    eval_set = NlFormalDataset(eval_data, tokenizer, max_length)

    # Models 
    model = GPTNeoForCausalLM.from_pretrained(model_name)
    model.resize_token_embeddings(len(tokenizer))

    # Initialize optimizer and scheduler
    decay_parameters = get_parameter_names(model, [torch.nn.LayerNorm])
    decay_parameters = [name for name in decay_parameters if "bias" not in name]
    optimizer_grouped_parameters = [
        {
            "params": [p for n, p in model.named_parameters() if n in decay_parameters],
            "weight_decay": weight_decay,
        },
        {
            "params": [p for n, p in model.named_parameters() if n not in decay_parameters],
            "weight_decay": 0.0,
        },
    ]


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
            max_grad_norm=gradient_clipping, 
            gradient_accumulation_steps = accum_steps,
            )

    devices = os.environ["CUDA_VISIBLE_DEVICES"]
    cfg["devices"] = devices

    with open(os.path.join(save_dir, "config.yaml"), "w") as f: 
        yaml.dump(cfg, f)

    Trainer(model=model, args=training_args, 
            train_dataset=train_set, eval_dataset=eval_set,
            data_collator=data_collator, 
            optimizers=(optimizer, scheduler),).train()

if __name__=="__main__": 
    main()
