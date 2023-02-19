#!/bin/env python3

import sys

lean = sys.argv[1]
tex = sys.argv[2]

with open(lean, 'r') as f:
    lean = f.read().splitlines()

with open(tex, 'r') as f:
    tex = f.read().replace("\\end{document}", "").split("\\paragraph{")

formal = [l.split('exercise_')[1].split(' ')[0]
                for l in lean if 'theorem exercise' in l]

formal = ['Exercise ' + f.replace('_', '.') for f in formal]

for l in tex:
    if "\\documentclass{" in l: 
        print(l)
    elif any([x in l for x in formal]):
        print("\\paragraph{" + l)

print("\\end{document}")
