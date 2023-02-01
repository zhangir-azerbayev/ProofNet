import sys
import ndjson 

path = sys.argv[1]
first_index = int(sys.argv[2])

with open(path) as f: 
    data = ndjson.load(f)

for i in range(len(data)): 
    last_index = first_index + i
    data[i]['task_id'] = last_index

print(last_index)

with open(path, "w") as f: 
    ndjson.dump(data, f)

