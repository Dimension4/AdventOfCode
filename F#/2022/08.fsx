let split (x: char) (s: string) = s.Split(x)

let grid = fsi.CommandLineArgs[1] |> split '\n'

let raycast seed successValue outOfBoundValue reduction x y dx dy =
    let height = grid[y][x]
    let rec march x y res =
        let x = x + dx
        let y = y + dy
        if y < 0 || y >= grid.Length || x < 0 || x >= grid[y].Length then
            reduction res outOfBoundValue
        elif grid[y][x] < height then
            march x y (reduction res successValue)
        else
            res
    march x y seed

let isVisible (x, y) =
    let r = raycast true true false (&&) x y
    not (r 1 0 && r -1 0 && r 0 1 && r 0 -1)

Seq.allPairs [0..grid.Length-1] [0..grid[0].Length-1]
|> Seq.filter isVisible
|> Seq.length
|> printfn "part 1: %d"

// part 2

let scenicScore (x, y) =
    let r = raycast 1 1 -1 (+) x y
    r 1 0 * r -1 0 * r 0 1 * r 0 -1

Seq.allPairs [0..grid.Length-1] [0..grid[0].Length-1]
|> Seq.map scenicScore
|> Seq.max
|> printfn "part 2: %d"