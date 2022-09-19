#!/bin/env python3
import sys
import ndjson
import re

codex_jsonl = sys.argv[1]
dict_jsonl = sys.argv[2]

with open(codex_jsonl) as f:
    codex = ndjson.load(f)

with open(dict_jsonl) as f:
    id_to_exercise_name = ndjson.load(f)

id_to_exercise_name = dict([(e['id'], e['name']) for e in id_to_exercise_name])

books_exercices = {}
for exercise in codex:
    statement = exercise['formal_statement']
    statement = statement.strip(' ')
    statement = statement.replace('\n', ' ')
    statement = re.sub(' +', ' ', statement)
    try:
        statement = statement.split(' ', maxsplit=1)[1] # remove theorem name
    except:
        pass
    statement = statement + ' :=\nsorry'
    name = id_to_exercise_name[exercise['id']]
    book   = name.split('.', maxsplit=1)[0]
    number = name.split('.', maxsplit=1)[1]
    number = 'theorem exercise_' + number
    try:
        books_exercices[book].append(number + '\n' + statement)
    except:
        books_exercices[book] = [number + '\n' + statement]


for book in books_exercices:
    file = 'formal/' + book + '_codex.lean'
    with open(file, 'w') as f:
        f.write('\n\n'.join(books_exercices[book]))

