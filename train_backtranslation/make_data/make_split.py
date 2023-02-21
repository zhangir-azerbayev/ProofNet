import ndjson 
import random 

with open("docgen_export_with_nl/docgen_export_with_nl.jsonl") as f: 
    data = ndjson.load(f)

random.shuffle(data)

with open("finetune_splits/docgen_export_train.jsonl", "w") as f: 
    ndjson.dump(data[:-3000], f)

with open("finetune_splits/docgen_export_valid.jsonl", "w") as f: 
    ndjson.dump(data[-3000:-1500], f)

with open("finetune_splits/docgen_export_test.jsonl", "w") as f: 
    ndjson.dump(data[-1500:], f)


