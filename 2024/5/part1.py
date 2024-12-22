import os

def read_file(path):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(script_dir, path)
    with open(file_path, "r") as f:
        text = f.read()
    return text

def is_correct(pages):
    global rule_dict
    for i,p in enumerate(pages):
        if p in rule_dict and any([n in rule_dict[p] for n in pages[:i+1]]):
            return False
    return True

def order(pages):
    global rule_dict
    while not is_correct(pages):
        for i,p in enumerate(pages):
            if p in rule_dict:
                errors = [n for n in pages[:i+1] if n in rule_dict[p]]
                if errors:
                    idx = min([pages.index(error) for error in errors])
                    pages[idx], pages[i] = pages[i], pages[idx]
    return pages

text = read_file("input.txt")

rules, updates = text.split("\n\n")
rules = rules.split("\n")

# Make dict of rules
rule_dict = {}
for rule in rules:
    f,l = rule.split("|")
    if f in rule_dict:
        rule_dict[f].add(l)
    else:
        rule_dict[f] = {l}

# Part one or part two 
part_one = False

updates = [update.split(",") for update in updates.split("\n")]
middle_sum = 0
for update in updates:
    if part_one:
        if is_correct(update):
            ordered = order(update)
            middle_sum += int(ordered[len(ordered)//2])
    else:
        if not is_correct(update):
            ordered = order(update)
            middle_sum += int(ordered[len(ordered)//2])
print(middle_sum)