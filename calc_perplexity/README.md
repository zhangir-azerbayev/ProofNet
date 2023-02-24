The script `calc.py` calculates the word-level perplexity of a Huggingface `AutoModelForCausalLM` on a Huggingface
dataset. 

To calculate perplexity, run `calc.py $PATH_TO_CONFIG`. Examples of valid config files can be found `./configs`. We
validated the correctness of this script by `calc.py configs/gpt2.py`, which replicates the result of`gpt2-large`
perplexity o Wikitext from the original GPT2 paper. 
