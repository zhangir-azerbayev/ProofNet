import os 

num_proofs = 0

for name in os.listdir("./"): 
    if name.endswith(".tex"): 
        with open(name) as f: 
            text = f.read()

        num_proofs += text.count("begin{proof}")

print(num_proofs)
