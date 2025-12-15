#include <algorithm>
#include <cctype>
#include <chrono>
#include <cstdint>
#include <fstream>
#include <generator>
#include <iostream>
#include <ranges>
#include <string>
#include <vector>

enum class Operation { ADD, MULTIPLY };

Operation stringToOperation(char c) {
    return (c == '+') ? Operation::ADD : Operation::MULTIPLY;
}

uint64_t startValue(Operation op) {
    return (op == Operation::MULTIPLY) ? 1ULL : 0ULL;
}

std::generator<std::string> streamLines(const std::string& filename) {
    std::ifstream file(filename);
    std::string line;
    while (std::getline(file, line)) {
        co_yield line;
    }
}

using Grid = std::vector<std::string>;

Grid buildGrid(const std::vector<std::string>& lines) {
    size_t width = 0;
    for (const auto& l : lines) width = std::max(width, l.size());

    Grid grid = lines;
    for (auto& row : grid) row.resize(width, ' ');
    return grid;
}

std::vector<std::pair<int, int>> findProblems(const Grid& grid) {
    int H = grid.size();
    int W = grid[0].size();

    std::vector<std::pair<int, int>> problems;
    bool active = false;
    int start = 0;

    for (int c = 0; c < W; ++c) {
        bool all_space = true;
        for (int r = 0; r < H; ++r) {
            if (grid[r][c] != ' ') {
                all_space = false;
                break;
            }
        }

        if (!all_space && !active) {
            active = true;
            start = c;
        } else if (all_space && active) {
            problems.emplace_back(start, c - 1);
            active = false;
        }
    }

    if (active) {
        problems.emplace_back(start, W - 1);
    }

    return problems;
}

Operation extractOperator(const Grid& grid, int l, int r) {
    int bottom = grid.size() - 1;
    for (int c = l; c <= r; ++c) {
        char ch = grid[bottom][c];
        if (ch == '+' || ch == '*') return stringToOperation(ch);
    }
    throw std::runtime_error("Operator not found");
}

std::vector<uint64_t> numbersPart1(const Grid& grid, int l, int r) {
    int H = grid.size();
    std::vector<uint64_t> nums;

    for (int row = 0; row < H - 1; ++row) {
        uint64_t val = 0;
        bool has_digit = false;

        for (int c = l; c <= r; ++c) {
            char ch = grid[row][c];
            if (std::isdigit(ch)) {
                val = val * 10 + (ch - '0');
                has_digit = true;
            }
        }

        if (has_digit) nums.push_back(val);
    }
    return nums;
}

std::vector<uint64_t> numbersPart2(const Grid& grid, int l, int r) {
    int H = grid.size();
    std::vector<uint64_t> nums;

    for (int c = r; c >= l; --c) {
        uint64_t val = 0;
        bool has_digit = false;

        for (int row = 0; row < H - 1; ++row) {
            char ch = grid[row][c];
            if (std::isdigit(ch)) {
                val = val * 10 + (ch - '0');
                has_digit = true;
            }
        }

        if (has_digit) {
            nums.push_back(val);
        }
    }
    return nums;
}

template <typename ExtractNumbers>
uint64_t solve(const Grid& grid, ExtractNumbers&& extract) {
    auto problems = findProblems(grid);
    uint64_t total = 0;

    for (auto [l, r] : problems) {
        Operation op = extractOperator(grid, l, r);
        auto nums = extract(grid, l, r);

        uint64_t acc = startValue(op);
        for (auto v : nums) {
            acc = (op == Operation::MULTIPLY) ? acc * v : acc + v;
        }
        total += acc;
    }
    return total;
}

int main() {
    auto lines = streamLines("input.txt") | std::ranges::to<std::vector>();
    auto start = std::chrono::steady_clock::now();
    Grid grid = buildGrid(lines);

    uint64_t part1 = solve(grid, numbersPart1);
    uint64_t part2 = solve(grid, numbersPart2);

    std::cout << "Part 1: " << part1 << "\n";
    std::cout << "Part 2: " << part2 << "\n";
}
