import ndjson 
import sys 

path = sys.argv[1]
num = int(sys.argv[2])


with open(path) as f: 
    data = ndjson.load(f)

for step in data[:num]: 
    formal = step["formal_statement"]
    nl = step["nl_statement_of_codex"]
    print("#"*40 +"\n" + formal + "\n\n" + nl)

