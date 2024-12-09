import re 
import os

def read_file(path):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    with open(script_dir + "\\" + path, "r") as f:
        text = f.read()
    return text


def part1(text):
    pattern = r"mul\((\d{1,3}),(\d{1,3})\)"
    matches = re.findall(pattern, text)
    mult = lambda pair: int(pair[0]) * int(pair[1])
    return sum(map(mult, matches))

if __name__ == "__main__":
    text = read_file("input.txt")
    print(part1(text))