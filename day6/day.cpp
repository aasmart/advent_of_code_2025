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
// NOTE: to self; spent way to long on trying to overcomplicate this with ranges
// and regex; should have done simpler solution

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

// NOTE: to self; got tunnel visioned on a false idea that numbers were "atomic units",
// and that I didn't want to break them up. This caused me not to consider literally
// just reading column-by-column and handling that from there.
//
// Although, this approach is pretty fast
//
// When I came up with the offset solution, I didn't pause to question if there could be
// a better way. Basically I didn't spend enough time with the actual problem, and instead
// spent more time trying to find the solution
auto part2(std::vector<std::string> const& lines) -> uint64_t {
    std::vector<std::vector<std::pair<size_t, std::string>>> numbers(lines.size() - 1);
    std::vector<char> operators;
    for (auto [i, line] :
         std::views::enumerate(std::ranges::subrange(lines.begin(), lines.begin() + lines.size() - 1))) {
        std::string number;
        std::optional<size_t> first_digit_pos;
        for (size_t j = 0; j <= line.size(); ++j) {
            if (j < line.size() && line[j] != ' ') {
                if (!first_digit_pos) {
                    first_digit_pos = j;
                }
                number.push_back(line[j]);
            } else if (first_digit_pos) {
                numbers[i].emplace_back(first_digit_pos.value(), number);
                number.clear();
                first_digit_pos.reset();
            }
        }
    }

    for (size_t i = 0; i < numbers[0].size(); ++i) {
        size_t min_start_pos = std::numeric_limits<size_t>::max();
        for (auto& row : numbers) {
            min_start_pos = std::min(row[i].first, min_start_pos);
        }

        for (auto& row : numbers) {
            row[i].first -= min_start_pos;
        }
    }

    std::istringstream iss { lines.back() };
    char op {};
    while (iss >> op) {
        operators.push_back(op);
    }

    uint64_t total { 0 };
    for (auto [column, op] : std::views::enumerate(operators)) {
        bool const is_multiply = op == '*';

        std::vector<uint64_t> args;
        uint32_t num_pos { 0 };
        while (true) {
            std::optional<uint64_t> num;
            for (auto& op_numbers : numbers) {
                auto const [col_start_pos, col_number_str] = op_numbers[column];
                auto const col_number_str_index = num_pos - col_start_pos;
                num = col_start_pos <= num_pos && col_number_str_index < col_number_str.size()
                      ? ((num.value_or(0) * 10) + col_number_str[col_number_str_index] - '0')
                      : num;
            }

            if (!num) {
                break;
            }

            args.push_back(num.value());
            num_pos += 1;
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
    std::println("{}", part2(read_lines(argv[1])));
    return 0;
}
