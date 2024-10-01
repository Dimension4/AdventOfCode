package aoc

import "core:fmt"
import "core:math"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"

board_set :: bit_set[0 ..< 25]

WIN_CONDITIONS := [?]board_set {
    {0, 1, 2, 3, 4},
    {5, 6, 7, 8, 9},
    {10, 11, 12, 13, 14},
    {15, 16, 17, 18, 19},
    {20, 21, 22, 23, 24},
    {0, 5, 10, 15, 20},
    {1, 6, 11, 16, 21},
    {2, 7, 12, 17, 22},
    {3, 8, 13, 18, 23},
    {4, 9, 14, 19, 24},
}

main :: proc() {
    bytes, ok := os.read_entire_file(os.args[1])
    file := transmute(string)bytes
    defer delete(bytes)
    assert(ok, "couldn't open input file")

    boards: [dynamic]u8
    defer delete(boards)
    numbers: [dynamic]u8
    defer delete(numbers)
    rest := file
    numbers_line, _ := strings.split_lines_iterator(&rest)
    for num in strings.split_iterator(&numbers_line, ",") {
        append(&numbers, u8(strconv.atoi(num)))
    }

    for num in strings.split_multi_iterate(&rest, {" ", "\r\n", "\n"}) {
        x := strconv.parse_int(num) or_continue
        append(&boards, u8(x))
    }

    visited := make([dynamic]board_set, len(boards) / 25)
    defer delete(visited)

    for num, num_idx in numbers {
        next_board: for board_idx := 0; board_idx < len(visited); board_idx += 1 {
            board := &visited[board_idx]
            for i in 0 ..< 25 {
                if boards[board_idx * 25 + i] == num {
                    board^ |= board_set{i}
                }
            }
            for cond in WIN_CONDITIONS {
                if cond <= board^ {
                    sum := 0
                    for i in 0 ..< 25 {
                        if i not_in board^ {
                            sum += int(boards[board_idx * 25 + i])
                        }
                    }
                    fmt.println("score:", sum * int(num))
                    remove_range(&boards, board_idx * 25, (board_idx + 1) * 25)
                    ordered_remove(&visited, board_idx)
                    board_idx -= 1
                    continue next_board
                }
            }
        }
    }
}

