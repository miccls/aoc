import numpy as np

def get_report(path):
    with open(path, "r+") as f:
        lines = f.readlines()
    reps = {}
    for line in lines:
        line = [int(s) for s in line.split()]
        if len(line) in reps:
            reps[len(line)] = np.concatenate((reps[len(line)], np.array([line])))
        else:
            reps[len(line)] = np.array([line])

    return reps

def check_sub_rep(lst, i): 
    sgn = lambda a: 0 if a == 0 else a/abs(a)
    tmp = np.concatenate((lst[:i], lst[i+1:]))
    diffs = [tmp[j+1] - tmp[j] for j in range(len(tmp)-1)]
    over_all_sign = -1 if sum([sgn(a) for a in diffs]) < 0 else 1
    if all([sgn(a) == over_all_sign and abs(a) < 4 for a in diffs]):
        return True
    return False

def check_safe(lst):
    # Loop over list and see how many is unsafe
   
    for i in range(len(lst)):
       if check_sub_rep(lst, i):
           return True        
    # Check whole array
    if check_sub_rep(lst, -1):
        return True
    return False



reps = get_report("Day2/report.txt")

# Split into two parts
total_safe = 0
for k, rep in reps.items():
    safe = sum([check_safe(d) for d in rep])
    total_safe += safe
print(total_safe)