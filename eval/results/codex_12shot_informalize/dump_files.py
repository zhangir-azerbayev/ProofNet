import ndjson

with open("out.jsonl") as f: 
    data = ndjson.load(f)

data = sorted(data, key = lambda x: x["id"])


lines_lst = ["\\paragraph{" + x["id"].replace("|", ".").replace("_", ".") + "} "+\
        x["codex_informal_statement"].replace("\\\\", "\\") for x in data]

text = "\n\n".join(lines_lst)

with open("dump.tex", "w") as f: 
    f.write(text)

