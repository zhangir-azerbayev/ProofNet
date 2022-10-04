#!/bin/env python3

import sys

lean = sys.argv[1]
tex = sys.argv[2]

with open(lean, 'r') as f:
    lean = f.read().splitlines()

with open(tex, 'r') as f:
    tex = f.read().splitlines()

formal = [l.split('exercise_')[1].split(' ')[0]
                for l in lean if 'theorem exercise' in l]

formal = ['Exercise ' + f.replace('_', '.') for f in formal]

for l in tex:
    if 'paragraph{Exercise ' in l:
        e = l.split('{')[1].split('}')[0]
        if e not in formal:
            print(e)
