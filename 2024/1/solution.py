import numpy as np
import timeit

# Execution time = 0.04s. Longer than doing manual insertion sort.

def get_arrays(path):
    with open(path, "r+") as f:
        lines = f.readlines()

    l = np.zeros((len(lines),))
    r = np.zeros((len(lines),))

    for i, line in enumerate(lines):
        l_value, r_value = line.split()
        l[i] = int(l_value)
        r[i] = int(r_value)

    return l,r 

def part_one():
    path = "Day1/input.txt"

    l,r = get_arrays(path)
        
    l = np.sort(l)
    r = np.sort(r)

    return l, r

def part_two():
    path = "Day1/input.txt"
    counts = {}
    l,r = get_arrays(path)
    sim = 0
    for lv in l:
        if lv not in counts:
            counts[lv] = np.count_nonzero(r == lv)
        sim += counts[lv] * lv
    return sim

l,r =part_one()
print("Answer to part one: ", end="")
print(np.sum(np.abs(l-r)))
print("Answer to part two: ", end="")
print(part_two())