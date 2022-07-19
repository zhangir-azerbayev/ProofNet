import json 
import sys
import re 

def merge_typestars_of_binders(binders): 
    binders = [re.sub(r"Type u_[0-9]", "Type*", x) for x in binders]

    i = 0 
    for i in range(len(binders)-1):
        if re.search(r"Type*", binders[i]) and re.search(r"Type*", binders[i+1]): 
            binders[i] = binders[i][binders[i].index(":")]
            




def assemble_statement(kind, nm, binders, tp): 
    statement = kind + " " + nm 
    for binder in binders: 
        sc = statement + " " + binder
        if len(sc[sc.rfind("\n")+1:]) > 80:
            statement += "\n\t" + binder
        else: 
            statement += " " + binder

    statement += " :\n\t" + tp

    return statement

def process_ue_string(arg: str): 
    #print(arg)
    arg = arg.replace("\n", " ")
    #print("ARG BEFORE PROCESSING: ", repr(arg))
    arg = re.sub(r"\ue000(.*?)\ue001", "", arg)
    arg = re.sub(r"\ue002", "", arg)
    #print("ARG: ", repr(arg))
    return arg

def parse_single_arg(arg): 
    if arg == "c": 
        return ""
    elif isinstance(arg, str): 
        return process_ue_string(arg)
    else: 
        assert isinstance(arg, list) 
        if arg[0] == "n":
            return parse_single_arg(arg[1:])
        else: 
            return "".join([parse_single_arg(x) for x in arg])


with open("docgen_export_full.json") as f:
    db = json.load(f)

for x in db["decls"][:10]: 
    #print(json.dumps(x, indent=4))

    list_of_args = [y["arg"] for y in x["args"]]
    binders = [parse_single_arg(y) for y in list_of_args]
    processed_tp = parse_single_arg(x["type"])

    print(assemble_statement(x["kind"], x["name"], binders, processed_tp))
    print("\n")
