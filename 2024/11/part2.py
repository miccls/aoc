import functools

@functools.cache
def find_nodes(value: str, turns: int) -> int:
    if turns == 0:
        return 1
    
    if not int(value):
        return find_nodes('1', turns - 1)

    if len(value) % 2:
        return find_nodes(str(int(value) * 2024), turns - 1)

    midpoint = len(value) // 2
    part0 = find_nodes(str(int(value[midpoint:])), turns - 1)
    part1 = find_nodes(str(int(value[:midpoint])), turns - 1)
    
    return part0 + part1

import os
if __name__ == '__main__':

    total = 0
    script_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(script_dir, "input.txt")
    with open(file_path, 'r') as f:
        for line in f:
            values = [x for x in line.strip().split(' ')]
    def get_tot(values):
        total = 0
        for v, value in enumerate(values):
            total += find_nodes(value, 25)
        return total

    import timeit
    execution_time = timeit.timeit('get_tot(values)', globals=globals(), number=1000)

    print(f"Execution time: {execution_time} seconds")