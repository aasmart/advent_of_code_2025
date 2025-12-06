#include <algorithm>
#include <cassert>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <print>
#include <ranges>
#include <sstream>
#include <string>
#include <vector>

namespace {
auto part1(std::vector<std::string> const& lines) -> uint64_t {
    std::vector<std::vector<uint64_t>> numbers;
    std::vector<char> operators;
    for (auto [i, line] :
         std::views::enumerate(std::ranges::subrange(lines.begin(), lines.begin() + lines.size() - 1))) {
        std::istringstream iss { line };
        uint64_t number {};
        while (iss >> number) {
            if (i >= numbers.size()) {
                numbers.emplace_back();
            }

            numbers[i].push_back(number);
        }
    }

    std::istringstream iss { lines.back() };
    char op {};
    while (iss >> op) {
        operators.push_back(op);
    }


    uint64_t total { 0 };
    for (auto [i, op] : std::views::enumerate(operators)) {
        bool const is_multiply = op == '*';

        std::vector<uint32_t> args;
        for (auto& op_numbers : numbers) {
            args.push_back(op_numbers[i]);
        }

        total += std::ranges::fold_left(args, is_multiply ? 1 : 0, [is_multiply](uint64_t acc, uint64_t value) {
            return is_multiply ? acc * value : acc + value;
        });
    }

    return total;
}

[[nodiscard]] auto read_lines(std::string filename) -> std::vector<std::string> {
    std::ifstream ifs { filename };

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(ifs, line)) {
        lines.push_back(line);
    }

    ifs.close();
    return lines;
}
}   // namespace

auto main(int argc, char* argv[]) -> int {
    std::println("{}", part1(read_lines(argv[1])));
    return 0;
}
