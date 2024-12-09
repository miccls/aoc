import re 
import os
from part1 import part1

def read_file(path):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    with open(script_dir + "\\" + path, "r") as f:
        text = f.read()
    return text

text = read_file("input.txt")
text = "do()"+text+"don't()"
pattern =  r"don't\(\).*?(?=do\(\)|$)"
text = re.sub(pattern, "", text, flags=re.DOTALL)
# Find all matches and their indices
total = part1(text)
print(total)