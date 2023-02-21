# Benchmark
Lean and TeX files are stored in `benchmark_to_publish/`. These files are then merged and dumped to a json by `parse_files.py`. The files `test.jsonl` and `valid.jsonl` are exactly the same as what is loaded by the [huggingface dataset](https://huggingface.co/datasets/hoskinson-center/proofnet). 

Evaluation scripts use the huggingface dataset in lieu of the files here. 
