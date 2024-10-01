#+private
package aoc

import "core:fmt"
import "core:os"

main :: proc() {
    bytes, ok := os.read_entire_file(os.args[1])
    fish := make([]int, len(bytes) / 2)
    defer delete(fish)

    for i in 0 ..< len(bytes) / 2 {
        fish[i] = int(bytes[i * 2] - '0')
    }

    cache := make(map[int]int, 256)
    defer delete(cache)
    fmt.println("part 1:", solve(fish, 80, &cache))
    fmt.println("part 2:", solve(fish, 256, &cache))
}

solve :: proc(fish: []int, days: int, cache: ^map[int]int) -> int {
    sum := len(fish)
    for f in fish do sum += calc(days - f, cache)
    return sum

    calc :: proc(remaining: int, cache: ^map[int]int) -> int {
        res, ok := cache[remaining]
        if ok do return res
        for t := remaining; t > 0; t -= 7 {
            res += 1 + calc(t - 9, cache)
        }
        cache[remaining] = res
        return res
    }
}

