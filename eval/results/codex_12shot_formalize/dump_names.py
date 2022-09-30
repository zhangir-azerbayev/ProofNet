import ndjson

with open("out.jsonl") as f: 
    data = ndjson.load(f)

data_ids = [x["id"] for x in data]

data_ids = sorted(data_ids)

for x in data_ids: 
    print(x)
