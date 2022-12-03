import argparse
from tqdm import tqdm
import ndjson
from nltk.translate.bleu_score import sentence_bleu


parser = argparse.ArgumentParser()
parser.add_argument("--path", type=str)
parser.add_argument("--candidate_key", type=str)
parser.add_argument("--reference_key", type=str)
args = parser.parse_args()

path = args.path
candidate_key = args.candidate_key
reference_key = args.reference_key

with open(path) as f: 
    data = ndjson.load(f)

scores = []
for example in tqdm(data): 
    cand = example[candidate_key]
    ref = example[reference_key]

    bleu = sentence_bleu([ref.split()], cand.split())

    scores.append(bleu)

print(sorted(scores))

avg = sum(scores)/len(scores)

print(f"BLEU SCORE: {avg}")

