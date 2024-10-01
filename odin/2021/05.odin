#+private
package aoc

import "core:fmt"
import "core:math/linalg"
import "core:os"
import "core:slice"
import "core:strconv"
import "core:strings"
import "core:text/regex"

vec :: [2]int
segment :: struct {
    src: vec,
    dst: vec,
}

main :: proc() {
    bytes, ok := os.read_entire_file(os.args[1])
    file := transmute(string)bytes
    defer delete(bytes)
    assert(ok, "couldn't open input file")
    segments: [dynamic]segment

    for line in strings.split_lines_iterator(&file) {
        s := line[:]
        i := strings.index_rune(s, ',')
        a := strconv.atoi(s[:i])
        s = s[i + 1:]
        i = strings.index_rune(s, ' ')
        b := strconv.atoi(s[:i])
        s = s[i + 4:]
        i = strings.index_rune(s, ',')
        c := strconv.atoi(s[:i])
        s = s[i + 1:]
        d := strconv.atoi(s)
        append(&segments, segment{{a, b}, {c, d}})
    }

    fmt.println("part 1:", solve(segments[:], false))
    fmt.println("part 2:", solve(segments[:], true))
}

solve :: proc(segments: []segment, allow_diagonal: bool) -> int {
    table := make(map[vec]int)
    defer delete(table)

    for seg in segments {
        dir := linalg.clamp(seg.dst - seg.src, -1, 1)
        if (!allow_diagonal && dir.x != 0 && dir.y != 0) {
            continue
        }
        for v := seg.src; v != seg.dst + dir; v += dir {
            count, ok := &table[v]
            if ok {
                count^ += 1
            } else {
                table[v] = 1
            }
        }
    }

    overlap := 0
    for v, count in table {
        if count >= 2 {
            overlap += 1
        }
    }
    return overlap
}

