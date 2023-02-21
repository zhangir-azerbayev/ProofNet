import json 
import sys 

with open("lookup_docgen_nl.json") as f: 
    data = json.load(f)

key = sys.argv[1]

print(data[key])
