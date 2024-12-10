import os

def read_file(path):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(script_dir, path)
    with open(file_path, "r") as f:
        text = f.read()
    return text

directions = [1,-1,1j,-1j]

def no_further_possibility(trailhead):
    global grid
    return all([False if (trailhead+d) not in grid else (int(grid[trailhead+d]) != (int(grid[trailhead])+1)) for d in directions])

def get_score(trailhead, part_two):
    """Recursive function to compute score of trailheads"""
    global grid
    global visited

    if grid[trailhead] == "9" and (trailhead not in visited or part_two):
        visited.append(trailhead)
        return 1
    elif no_further_possibility(trailhead):
        return 0
    else:
        score = 0
        for d in directions:
            if (trailhead+d) in grid and int(grid[trailhead+d]) == int(grid[trailhead]) + 1:
                # Then we can go there
                score += get_score(trailhead + d, part_two)
        return score


path = "input.txt"
text = read_file(path).split()

y = len(text)
x = len(text[0])

# Use complex coordinates for the map
grid = {}
# Visited trailheads
visited = []
trailheads = []
for xi in range(x):
    for yi in range(y):
        grid[complex(xi,yi)] = text[yi][xi]
        if text[yi][xi] == "0":
            trailheads.append(complex(xi,yi))

# Approach: 
# Loop over the trail heads and compute the score recursively.
# Fun!

score = 0
part_two = True
for trailhead in trailheads:
    visited = []
    score += get_score(trailhead, part_two)
print(score)