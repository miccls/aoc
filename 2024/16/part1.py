import os
from heapq import *
from collections import *

def read_file(path):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(script_dir, path)
    with open(file_path, "r") as f:
        text = f.read()
    return text

dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

visited = set()
def solvep1(G):
    R = len(G)
    C = len(G[0])
    S, E = None, None
    for r in range(R):
        for c in range(C):
            if G[r][c] == "S":
                S = r, c
            elif G[r][c] == "E":
                E = r, c
    costs = {(S, 0): 0}
    pq = [(0, (S, 0))]
    while pq:
        cost, (s, d) = heappop(pq)
        if cost != costs[(s, d)]:
            continue
        if s == E:
            return cost
        sr, sc = s

        # move forward
        dr, dc = dirs[d]
        nbr, nbc = sr + dr, sc + dc
        if 0 <= nbr < R and 0 <= nbc < C and G[nbr][nbc] != "#":
            nb = ((nbr, nbc), d)
            if nb not in costs or costs[nb] > cost + 1:
                costs[nb] = cost + 1
                heappush(pq, (cost + 1, nb))

        # rotate
        for dn in [(d + 1) % 4, (d - 1) % 4]:
            nb = ((sr, sc), dn)
            if nb not in costs or costs[nb] > cost + 1000:
                costs[nb] = cost + 1000
                heappush(pq, (cost + 1000, nb))


def solvep2(G):
    R = len(G)
    C = len(G[0])
    S, E = None, None
    for r in range(R):
        for c in range(C):
            if G[r][c] == "S":
                S = r, c
            elif G[r][c] == "E":
                E = r, c

    cost = solvep1(G)
    costs = {(S, 0): 0}
    allp = set()

    def bt(c):
        s, d = P[-1]

        if c == cost and s == E:
            allp.update(cur)
        elif c > cost:
            return

        sr, sc = s
        for dn, dcost in [(d, 0), ((d + 1) % 4, 1000), ((d - 1) % 4, 1000)]:
            dr, dc = dirs[dn]
            nbr, nbc = sr + dr, sc + dc
            if (
                0 <= nbr < R
                and 0 <= nbc < C
                and G[nbr][nbc] != "#"
                and (nbr, nbc) not in cur
            ):
                nb = ((nbr, nbc), dn)
                newcost = c + 1 + dcost
                if nb not in costs or newcost <= costs[nb]:
                    costs[nb] = newcost

                    cur.add((nbr, nbc))
                    P.append(nb)
                    bt(newcost)
                    cur.remove((nbr, nbc))
                    P.pop()
    P = [(S, 0)]
    cur = {S}


    bt(0)
    return len(allp)


maze = read_file("input.txt")
maze = maze.split("\n")
print(solvep2(maze))