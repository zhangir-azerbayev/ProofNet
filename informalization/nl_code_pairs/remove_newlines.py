import sys
import ndjson 

path = sys.argv[1]

with open(path) as f: 
    data = ndjson.load(f)

for i in range(len(data)): 
    data[i]['nl_statement_of_codex'] = data[i]['nl_statement_of_codex'].replace('\n', ' ')


with open(path, "w") as f: 
    ndjson.dump(data, f)

