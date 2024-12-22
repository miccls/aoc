
import os

def read_file(path):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(script_dir, path)
    with open(file_path, "r") as f:
        text = f.read()
    return text

def evolve(strnum):
    if (strnum == "0"):
        return "1"
    elif (len(strnum) % 2 == 0):
        right = str(int(strnum[int(len(strnum)/2):]))
        left = str(int(strnum[:int(len(strnum)/2)]))
        return [left, right]
    else:
        return str(2024 * int(strnum))
    
text = read_file("input.txt")
stones = text.split()
#stones = "125 17".split()


stone_list = []
def countstones(stones,its):
    
    for _ in range(its):
        print(_)
        nums = []
        for i in stones:
            new = evolve(i)
            if type(new) is list:
                for n in new:
                    nums.append(n)
            else:
                nums.append(new)
        stones = nums.copy()
        #print(stones)
    return len(stones)

def evolvestones(stones):
    nums = []
    for i in stones:
        new = evolve(i)
        if type(new) is list:
            for n in new:
                nums.append(n)
        else:
            nums.append(new)
            
    return nums


def part2(stones, its):
    stone_list = [[s] for s in stones]
    for _ in range(its):
        print(_)
        for i in range(len(stone_list)):
            pass

    return stone_list



#print(countstones(["0"],20))
print(countstones(stones, 25))