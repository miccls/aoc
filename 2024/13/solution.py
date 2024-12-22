import os
import numpy as np
import re
from math import isclose

def read_file(path):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(script_dir, path)
    with open(file_path, "r") as f:
        text = f.read()
    return text

def read_lps(path):
    text = read_file(path)
    text = text.split("\n\n")
    text = [p.split("\n") for p in text]

    lps = []
    sols = []
    for i in range(len(text)):
        lp = text[i]
        A_eq = list(map(int,re.findall(r'\d+', lp[0])))
        B_eq = list(map(int,re.findall(r'\d+', lp[1])))
        rhs_eq = list(map(lambda s: int(s) + 10000000000000,re.findall(r'\d+', lp[2])))
        lps.append({"obj": [3,1],
            "lhs_eq" : [[A_eq[0], B_eq[0]], [A_eq[1], B_eq[1]]],
            "rhs_eq" : rhs_eq})
        A = np.array([[A_eq[0], B_eq[0]], [A_eq[1], B_eq[1]]])
        sol = np.linalg.solve(A, rhs_eq)
        if any([s < 0 or not isclose(round(s), s, rel_tol=0, abs_tol=1e-4) for s in sol]):
            continue
        sol = [round(s) for s in sol]
        tokens = np.dot([3,1], sol)
        sols.append(tokens)
    return lps, sols
   
lps, sols = read_lps("input.txt")
print(sum(sols))
quit()
print(lps[0])
bnd = [(0, float("inf")),
       (0, float("inf"))]

from scipy.optimize import linprog
tot=0
for lp in lps:
    opt = linprog(c=lp["obj"],A_eq=lp["lhs_eq"], b_eq=lp["rhs_eq"],integrality=[1,1], bounds=bnd, method="highs-ipm")
    if opt.success:
        tot += int(opt.fun)
print(tot)