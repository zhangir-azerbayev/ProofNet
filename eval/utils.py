import os
import ndjson
import openai 
from ratelimit import limits, sleep_and_retry
from nltk.translate.bleu_score import sentence_bleu

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
    return sorted(l, key = alphanum_key)

def batch_loader(seq, size):
    """
    Iterator that takes in a list `seq` and returns
    chunks of size `size`
    """
    return [seq[pos : pos + size] for pos in range(0, len(seq), size)]


# calls=3, period=60 is max for reliability with batch_size=20
# might want to throttle it to keep lean chat up
@sleep_and_retry
@limits(calls=2, period=60)
def call_api(prompt, stop, max_tokens=150,):
    return openai.Completion.create(
        engine="code-davinci-002",
        prompt=prompt,
        max_tokens=max_tokens,
        n=1,
        temperature=0,
        stop=stop,
    )

def calc_bleu(data, candidate_key, reference_key): 
    bleus = [sentence_bleu([x[candidate_key]].split(), x[reference_key].split()) for x in data]
    return sum(bleus)/len(bleus)

def make_readable(save_dir, save_file, ref_key): 
    if ref_key=="nl_statement": 
        _dump_tex(save_dir, ref_key)
    elif ref_key=="formal_statement": 
        _dump_lean(save_dir, ref_key)
    else: 
        raise AssertionError("Unrecognized ref key")


def _dump_tex(save_dir, save_file): 
    with open(os.path.join(save_dir, save_file)) as f: 
        data = ndjson.load(f)

    data = natural_sorted(data, key = lambda x: x["id"])


    lines_lst = ["\\paragraph{" + x["id"].replace("|", ".").replace("_", ".") + "} "+\
            x["codex_informal_statement"].replace("\\\\", "\\") for x in data]

    text = TEX_PREAMBLE + "\n\n".join(lines_lst) + TEX_POSTAMBLE

    with open(os.path.join(save_dir, "gpt_nl_statement.tex"), "w") as f: 
        f.write(text)

def _dump_lean(save_dir, save_file): 
    with open(os.path.join(save_dir, save_file)) as f: 
        data = ndjson.load(f)

    data = natural_sorted(data, key = lambda x: x["id"])
    
    author = None
    for x in data: 
        eyed = x["id"]
        new_author = x[:x.index("|")]
        exname = x[x.index("|")+1:]

        if author!=new_author:
            author = new_author

            src = "import .common\n" + x["src_header"] + "\n\n"

            with open(os.path.join(save_dir, author + ".lean"), "w") as f: 
                f.write(src)

        with open(os.path.join(save_dir, author+".lean"), "a") as f: 
            f.write(f"\n\ntheorem {exname} " + x["gpt_formal_statement"])
