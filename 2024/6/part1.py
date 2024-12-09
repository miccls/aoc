grid = open('day06.txt').read().split('\n')
grid = [[line[i] for i in range(len(line))] for line in grid]

h, w = len(grid),len(grid[0])

delta, rotate = -1j, 1j

for j in range(h):
    for i in range(w):
        if grid[j][i] == '^':
            loc = complex(i,j)
            start_loc = loc * 1
            grid[j][i] = '.'

grid = {complex(i,j): grid[j][i] for j in range(h) for i in range(w)}


def check_loop(grid, l, d):
    seen = set((l,d))
    noloop = True
    while noloop:
        if l + d in grid:
            if grid[l + d] == '.': # walk forward
                l = l + d
            else:
                d *= rotate # turn
            if (l,d) in seen: # this looks familiar
                noloop = False
            else:
                seen.add((l,d)) # make notes
        else: # left the grid
            break
    return(not noloop)

visited = set((loc,))
while True:
    if loc + delta in grid: # on the board
        if grid[loc + delta] == '.':# free space directly ahead
            loc += delta
        else: # block straight ahead, so just turn
            delta *= rotate
        visited.add((loc,))
    else: # off the board
        break
            
print(len(visited))