
def parse_file():
    path = "input.txt"
    with open(path) as f:
        lines = [line.strip() for line in f.readlines()]
    separating_index = lines.index("")
    return [list(map(int, s_range.split("-"))) for s_range in lines[:separating_index]], [int(l) for l in lines[separating_index+1:]]

def part1():
    fresh_ranges, ingredients = parse_file()
    print(len([l for l in ingredients if any([(b[0] <= l and l <= b[1]) for b in fresh_ranges])]))
    
def adjust_ranges(ranges, removed_range):
    _, removed_ub = removed_range
    return [[max(removed_ub+1, lb), ub] for (lb, ub) in ranges if ub > removed_ub]
    
def part2():
    fresh_ranges, _ = parse_file()
    fresh_ranges = sorted(fresh_ranges, key = lambda x : x[0])
    def length_of_interval_with_smallest_lower_bound(ranges):
        if ranges == []:
            return 0
        lb, ub = ranges.pop(0)
        return len(range(lb, ub+1)) + length_of_interval_with_smallest_lower_bound(adjust_ranges(ranges, [lb, ub]))
    print(length_of_interval_with_smallest_lower_bound(fresh_ranges))   
    
part1()
part2()