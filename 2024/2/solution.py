import numpy as np

def sign(x):
    if x:
        return x/abs(x)
    return 0

def get_report(path):
    with open(path, "r+") as f:
        lines = f.readlines()
    reps = [list(map(int, line.split())) for line in lines]
    return reps

def check_sub_rep(lst, i): 
    tmp = np.concatenate((lst[:i], lst[i+1:])) if i != -1 else lst
    diffs = [tmp[j+1] - tmp[j] for j in range(len(tmp)-1)]
    majority_sign = -1 if sum([sign(a) for a in diffs]) < 0 else 1
    if all([sign(a) == majority_sign and abs(a) < 4 for a in diffs]):
        return True
    return False

def check_safe(lst, part_two):
    # Loop over list and see how many is unsafe
    for i in range(len(lst)):
       if check_sub_rep(lst, i) and part_two:
           return True        
    # Check whole array
    if check_sub_rep(lst, -1):
        return True
    
    return False

reps = get_report("2024/Day2/report.txt")

part_two = False

# Split into two parts
total_safe = 0
for rep in reps:
    total_safe += check_safe(rep,part_two)
print(total_safe)