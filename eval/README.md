The `configs` directory contains config files for reproducing the experiments in table 3 of the paper. The path to the desired config file should be given as the first argument to one of `of_finetuned.py`, `of_hfmodel.py`, `of_openai.py`, `of_retrieval.py`: based on the name of the config file it should be apparent which of the four scripts should be used. 

Example usage: `python of_hfmodel.py configs/pythia-1.4b_6shot_informalize.yaml`. 

Nb. that we do not distribute weights for backtranslation finetuned models and these must be obtained by running the training scripts in `train_backtranslation/`. 
