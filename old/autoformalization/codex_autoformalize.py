from tqdm import tqdm
import re
import random
import json
import sys
import openai
from ratelimit import limits, sleep_and_retry
from itertools import zip_longest
import time


@sleep_and_retry
@limits(calls=1, period=60)
def call_api(engine, prompt, max_tokens, n, temperature):
    return openai.Completion.create(
        engine=engine,
        prompt=prompt,
        max_tokens=max_tokens,
        n=n,
        temperature=temperature,
    )


def batch_loader(seq, size):
    """
    Iterator that takes in a list `seq` and returns
    chunks of size `size`
    """
    return [seq[pos : pos + size] for pos in range(0, len(seq), size)]


def delete_non_numeric_chars(string):
    return re.sub("[^0-9]", "", string)


def parse_tex_file(path):
    tex_lines = open(path).readlines()

    problems = []

    first_idx = [i for i, l in enumerate(tex_lines) if "begin{document}" in l][0] + 1

    book = ""
    book_mark = "\\section*{"
    chapter = ""
    chapter_mark = "\\subsection*{"
    section = ""
    section_mark = "\\subsubsection*{"
    for i, line in enumerate(tex_lines[first_idx:]):
        if book_mark in line:
            book = line[len(book_mark) : -len("}\n")]
            chapter = ""
            section = ""
        elif chapter_mark in line:
            chapter = delete_non_numeric_chars(line)
            section = ""
        elif section_mark in line:
            section = delete_non_numeric_chars(line)
        elif ":" in line:
            problem_num = line[: line.index(":")]
            nl_statement = line[line.index(":") + 1 :].strip()

            task_id = i

            source_lst = [book, chapter, section, problem_num]
            while "" in source_lst:
                source_lst.remove("")
            source_id = "_".join(source_lst)

            problems.append(
                {"task_id": i, "source_id": source_id, "nl_statement": nl_statement}
            )
    return problems

def main():
    prefix = "\nNatural language version: \""
    postfix = "\" Translate the natural language version to a Lean mathlib version:"
    batch_size = 10
    n = 1
    temp = 0

    prompt = open("few_shot_prompt.txt", "r").read()

    problems = parse_tex_file("nl_statements_v1/nl_statements.tex")

    problem_loader = batch_loader(problems, batch_size)

    log = []
    for probs in tqdm(problem_loader): 
        
        prompts = [prompt + prefix + x["nl_statement"] + postfix
                for x in probs]

        outs = call_api(engine="code-davinci-002", 
                        prompt=prompts, 
                        max_tokens=400, 
                        n=1, 
                        temperature=0
                        )

        text_outs = [x["text"] for x in outs["choices"]]

        [print("#"*20, "\n", x) for x in text_outs]

        trimmed_outs = [x[:x.index(":=")].strip() if ":=" in x else "--failed to generate statement" for x in text_outs]
        
        for prob_dict, formal_statements, raw_outs in zip(probs, batch_loader(trimmed_outs, n), batch_loader(text_outs, n)):  
            # temp while n=1
            prob_dict["codex_formal_statement"] = formal_statements[0] 
            prob_dict["codex_raw_output"] =raw_outs[0]

            log.append(prob_dict)

    with open("nl_statements_v1/codex_log.json", "w") as f: 
        json.dump(log, f)

    with open("nl_statements_v1/codex_dump.lean", "w") as f: 
        for prob in log: 
            lean_dump = "\n".join(["/-", prob["source_id"], prob["nl_statement"], "-/", prob["codex_formal_statement"], ":= sorry\n\n"])
            f.write(lean_dump)

if __name__=="__main__": 
    main()
