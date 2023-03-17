import os
import ndjson
import openai 
import re
from ratelimit import limits, sleep_and_retry
from nltk.translate.bleu_score import sentence_bleu, SmoothingFunction
import backoff

TEX_PREAMBLE = """\documentclass{article}

\\title{\\textbf{
Exercises from \\\\
\\textit{Everything} \\\\
by All Authors
}}

\\date{}

\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{fullpage}

\\begin{document}
\\maketitle\n
"""
TEX_POSTAMBLE = "\n\\end{document}"

def natural_sorted(l): 
    """ 
    Sort the given iterable in the way that humans expect.
    https://stackoverflow.com/questions/2669059/how-to-sort-alpha-numeric-set-in-python
    """ 
    convert = lambda text: int(text) if text.isdigit() else text 
    alphanum_key = lambda key: [ convert(c) for c in re.split('([0-9]+)', key) ] 
    return sorted(l, key = lambda x: alphanum_key(x["id"]))

def batch_loader(seq, size):
    """
    Iterator that takes in a list `seq` and returns
    chunks of size `size`
    """
    return [seq[pos : pos + size] for pos in range(0, len(seq), size)]


# calls=3, period=60 is max for reliability with batch_size=20
# might want to throttle it to keep lean chat up
@backoff.on_exception(backoff.expo, openai.error.RateLimitError)
def call_api(prompt, stop, max_tokens=150, endpoint="code-davinci-002"):
    response = openai.Completion.create(
        engine=endpoint,
        prompt=prompt,
        max_tokens=max_tokens,
        n=1,
        temperature=0,
        stop=stop,
    )

    print(f"RECEIVED RESPONSE FROM {endpoint}")
    return response

def calc_bleu(data, candidate_key, reference_key): 
    bleus = [sentence_bleu([x[reference_key].split()], x[candidate_key].split(),
        smoothing_function=SmoothingFunction().method4) for x in data]
    return sum(bleus)/len(bleus)

def make_readable(save_dir, save_file, ref_key): 
    if ref_key=="nl_statement": 
        _dump_tex(save_dir, save_file)
    elif ref_key=="formal_statement": 
        _dump_lean(save_dir, save_file)
    else: 
        raise AssertionError("Unrecognized ref key")


def _dump_tex(save_dir, save_file): 
    with open(os.path.join(save_dir, save_file)) as f: 
        data = ndjson.load(f)

    data = natural_sorted(data)


    lines_lst = ["\\paragraph{" + x["id"].replace("|", ".").replace("_", ".") + "} "+\
            x["gpt_nl_statement"].replace("\\\\", "\\") for x in data]

    text = TEX_PREAMBLE + "\n\n".join(lines_lst) + TEX_POSTAMBLE

    with open(os.path.join(save_dir, "gpt_nl_statement.tex"), "w") as f: 
        f.write(text)

def _dump_lean(save_dir, save_file): 
    with open(os.path.join(save_dir, save_file)) as f: 
        data = ndjson.load(f)

    data = natural_sorted(data)
    
    author = None
    for x in data: 
        eyed = x["id"]
        new_author = eyed[:eyed.index("|")]
        exname = eyed[eyed.index("|")+1:]

        if author!=new_author:
            author = new_author

            src = x["src_header"] + "\n\n"

            with open(os.path.join(save_dir, author + ".lean"), "w") as f: 
                f.write(src)

        formal = x["gpt_formal_statement"][1:]

        with open(os.path.join(save_dir, author+".lean"), "a") as f: 
            try: 
                f.write(f"\n\ntheorem {exname}" + formal[formal.index(" "):]  + ":=\nsorry")
            except ValueError: 
                f.write(f"\n\ntheorem {exname}" + formal  + ":=\nsorry")

