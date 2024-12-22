import os
import re
import numpy as np

def read_file(path):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(script_dir, path)
    with open(file_path, "r") as f:
        text = f.read()
    return text

def is_x_mas(array):
    rev = np.array([a[::-1] for a in array])
    diag = "".join(np.diag(array)) 
    rev = "".join(np.diag(rev))
    if (diag == "MAS" or diag == "SAM") and (rev == "MAS" or rev == "SAM"):
        return 1
    return 0

# Check diags
def part1(path):
    text = read_file(path)
    # Extract all diagonals, rows, and columns
    text_matrix_rev = np.array([list(line)[::-1] for line in text.split("\n")])
    text_matrix = np.array([list(line) for line in text.split("\n")])
    tot = 0
    pattern = r'(?=(XMAS|SAMX))'
    for k in range(4-len(text), len(text) - 3):
        diag1 = "".join(np.diag(text_matrix, k = k))
        diag2 = "".join(np.diag(text_matrix_rev, k = k))
        tot+=len(re.findall(pattern, diag1))
        tot+=len(re.findall(pattern, diag2))

    # Rows
    for l in text_matrix:
        l = "".join(l)
        tot+=len(re.findall(pattern, l))

    # Cols
    for c in range(len(text_matrix[0])):
        col = "".join(text_matrix[:,c])
        tot+=len(re.findall(pattern, col))
    
    return tot

def part2(path):
    text = read_file(path)
    text_matrix = np.array([list(line) for line in text.split("\n")])
    size = (3,3)
    tot = 0
    for i in range(len(text_matrix)-size[0]+1):
        for j in range(len(text_matrix[0])-size[1]+1):
            tot += is_x_mas(text_matrix[i:i+size[0],j: j+size[1]])
    return tot

print("Part 1: ", part1("input.txt"))
print("Part 2: ", part2("input.txt"))