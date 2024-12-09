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

blocks = set()
visited = set((loc,))
while True:
    if loc + delta in  grid: # on the board
        if grid[loc + delta] == '.':# free space directly ahead
            if loc + delta != start_loc and (loc + delta,) not in visited:
                # make sure we haven't hit the starting location, and that we haven't already
                # been here before, because we shouldn't put a wall up the second time we reach here
                # only the first time
                block = loc + delta # suppose we put a block directly ahead
                tmp_grid = grid.copy()
                tmp_grid[block] = '#'
                tmp_delta = delta * rotate # we would rotate right here
                tmp_loc = loc # direction with test-block in place
                if check_loop(tmp_grid, tmp_loc, tmp_delta) == True: # does it loop?
                    blocks.add(block) # it loops!
            loc += delta
        else: # block straight ahead, so just turn
            delta *= rotate
        visited.add((loc,))
    else: # off the board
        break
            
print(len(blocks))