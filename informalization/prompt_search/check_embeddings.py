import sys
import ndjson
import numpy as np 

num_shards = int(sys.argv[1])

embeddings_dict = []
for i in range(num_shards):
    with open(f"mathlib_train_nl_embeddings/shard_{i}.jsonl") as f: 
        embeddings_dict += ndjson.load(f)

for i in range(len(embeddings_dict)):
    assert embeddings_dict[i]["task_id"] == i

embeddings_list = [x["embedding"] for x in embeddings_dict]
arr = np.array(embeddings_list)

print(arr)
print(arr.shape)
