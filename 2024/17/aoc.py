import os

def read_file(path):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(script_dir, path)
    with open(file_path, "r") as f:
        text = f.read()
    return text


def combo(operand, state):
    if operand in [0,1,2,3]:
        return operand
    elif operand == 4:
        return state["A"]
    elif operand == 5:
        return state["B"]
    elif operand == 6:
        return state["C"]
    else:
        raise TypeError(f"Bad input, {operand} not a valid operand")
    

def combo_inv(operation, operand, state):
    if operand in [0,1,2,3]:
        return state # Nothing can be said
    elif operand == 4:
        return state["A"]
    elif operand == 5:
        return state["B"]
    elif operand == 6:
        return state["C"]
    else:
        raise TypeError(f"Bad input, {operand} not a valid operand")

def adv(operand, state):
    state["A"] = int(state["A"] / (2**combo(operand, state)))

def bxl(operand, state):
    state["B"] = operand ^ state["B"]

def bst(operand, state):
    state["B"] = (combo(operand, state) % 8)
    return state["B"]

def jnz(operand, state):
    if state["A"]:
        state["ip"] = operand
        # Remember to not jump two

def bxc(operand, state):
    state["B"] = state["B"] ^ state["C"]

def out(operand, state):
    return f"{combo(operand, state) % 8},"

def bdv(operand, state):
    state["B"] = int(state["A"] / (2**combo(operand, state)))

def cdv(operand, state):
    state["C"] = int(state["A"] / (2**combo(operand, state)))


def state_given_print(operation, operand):
    combo_inv(operation, operand)



ops = [
    adv,
    bxl,
    bst,
    jnz,
    bxc,
    out,
    bdv,
    cdv
]

def numberToBase(n, b):
    if n == 0:
        return [0]
    digits = []
    while n:
        digits.append(int(n % b))
        n //= b
    return digits[::-1]

def solve1(text):

    state = {
        "A" : 0,
        "B" : 0,
        "C" : 0,
        "ip": 0
    }

    init, program = text.split("\n\n")

    for line in init.split("\n"):
        if "Register A" in line:
            state["A"] = int(line.split("Register A:")[1])
        elif "Register B" in line:
            state["B"] = int(line.split("Register B:")[1])
        elif "Register C" in line:
            state["C"] = int(line.split("Register C:")[1])

    program = list(map(int,program.replace("Program: ","").rstrip().split(",")))
    s = ""
    while True:
        try:
            ip = state["ip"]
            #print(ip)
            v = ops[program[ip]](program[ip+1], state)
            if program[ip]==5:
                s += str(v)
            # If jump, don't change
            if ip == state["ip"]:
                state["ip"] += 2

        except IndexError:
            break
    return state, s

"""
bst(4) // b <- combo(4) % 8 = A%8
bxl(6) // b <- b ^ 6 = (A % 8) ^ 6
cdv(5) // c <- A/2**combo(5) = A/2**((A % 8) ^ 6) 
bxc(6) // b <- A/2**((A % 8) ^ 6) ^ ((A % 8) ^ 6)
bxl(4) // b <- b ^ 4 = (A/2**((A % 8) ^ 6) ^ ((A % 8) ^ 6)) ^ 4
out(5)
adv(3)
jnz(0)
"""

def A_to_out(A:int):

    B = int((A % 8) ^ 6)
    C = int(A / (2**B))
    B = int(B ^ C)
    B = int(B ^ 4)

    return B % 8

def experiment(text, r):

    state = {
        "A" : 0,
        "B" : 0,
        "C" : 0,
        "ip": 0
    }

    init, program = text.split("\n\n")
    program = list(map(int,program.replace("Program: ","").rstrip().split(",")))
    for k in range(100000):
        
        state = {
        "A" : k,
        "B" : 0,
        "C" : 0,
        "ip": 0
        }   
    
        
        s = ""
        while True:
            try:
                ip = state["ip"]
                #print(ip)
                if ip == 0:
                    print(state)
                v = ops[program[ip]](program[ip+1], state)
                if program[ip]==5:
                    s += str(v)
                # If jump, don't change
                if ip == state["ip"]:
                    state["ip"] += 2

            except IndexError:
                break
        print(oct(k), ":",s)
    
import time

text = read_file("input.txt")
print(solve1(text))
"""
n = 727510707590656
n = 0
s = ""
while "2,4,1,6,7,5,4,6,1,4,5,5,0,3,3,0" not in s:
    s = ""
    nn = n
    while nn > 0:
        op = A_to_out(nn)
        s = s + f"{op},"
        nn = nn // 8
    print(n,":",s)
    time.sleep(0.5)
    n = n+1
        
"""
"""
bst(4) // b <- combo(4) % 8 = A%8
bxl(6) // b <- b ^ 6 = (A % 8) ^ 6
cdv(5) // c <- A/2**combo(5) = A/2**((A % 8) ^ 6) 
bxc(6) // b <- A/2**((A % 8) ^ 6) ^ ((A % 8) ^ 6)
bxl(4) // b <- b ^ 4 = (A/2**((A % 8) ^ 6) ^ ((A % 8) ^ 6)) ^ 4
out(5)
adv(3)
jnz(0)
"""

# We know A decreases by 8 each iteration, so we can just back track:
# Target print: 2,4,1,6,7,5,4,6,1,4,5,5,0,3,3,0
# 0
# Results from all possible inputs:
#[print(i,":",A_to_out(i)%8) for i in range(20)]

"""

{'A': 66171486, 'B': 0, 'C': 0, 'ip': 0}
{'A': 8271435, 'B': 66171482, 'C': 66171486, 'ip': 0}
{'A': 1033929, 'B': 258483, 'C': 258482, 'ip': 0}
{'A': 129241, 'B': 8078, 'C': 8077, 'ip': 0}
{'A': 16155, 'B': 1010, 'C': 1009, 'ip': 0}
{'A': 2019, 'B': 505, 'C': 504, 'ip': 0}
{'A': 252, 'B': 62, 'C': 63, 'ip': 0}
{'A': 31, 'B': 57, 'C': 63, 'ip': 0}
{'A': 3, 'B': 10, 'C': 15, 'ip': 0}

"""

"""
t = "2,4,1,6,7,5,4,6,1,4,5,5,0,3,3,0"
n = 1
for i in range(16):
    while True:
        s = ""
        nn = n
        while nn > 0:
            op = A_to_out(nn)
            s = s + f"{op},"
            nn = nn // 8
        s = s[:-1]
        print("s: ", s)
        if t == s:
            print(n)
            break
        if t.endswith(s):
            n = n*8
            break
        n+=1

s = ""
nn = n
while nn > 0:
    op = A_to_out(nn)
    s = s + f"{op},"
    nn = nn // 8
s = s[:-1]
print("\ns: ", s)
"""