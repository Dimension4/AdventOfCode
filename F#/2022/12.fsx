let split (x: char) (s: string) = s.Split(x)

let grid = fsi.CommandLineArgs[1] |> split '\n' |> array2D

let find value (arr: 'a[,]) =
    let rec iter x y =
        if x >= Array2D.length2 arr then iter 0 (y + 1)
        elif arr[y, x] = value then (x, y)
        else iter (x + 1) y
    iter 0 0

let sx, sy = find 'S' grid
grid[sy, sx] <- 'a'

let dx, dy = find 'E' grid
grid[dy, dx] <- 'z'

let pathFind grid destX destY =
    let w, h = Array2D.length2 grid, Array2D.length1 grid
    let weights = Array2D.create h w System.Int32.MaxValue
    let inBounds x  y = x >= 0 && x < w && y >= 0 && y < h

    let rec walk x y prevElev len =
        if inBounds x y then
            let elev = grid[y, x]
            if elev >= prevElev - '\001' && weights[y, x] > len then
                weights[y, x] <- len
                let len = len + 1
                walk (x - 1) y elev len
                walk (x + 1) y elev len
                walk x (y - 1) elev len
                walk x (y + 1) elev len
        ()

    walk destX destY 'z' 0
    weights

let weights = pathFind grid dx dy
printfn $"part 1: {weights[sy, sx]}"

// part 2

let shortestClimb weights grid =
    let mutable dist = System.Int32.MaxValue
    for y in 0..Array2D.length1 grid - 1 do
        for x in 0..Array2D.length2 grid - 1 do
            if grid[y, x] = 'a' then
                dist <- Array2D.get weights y x |> min dist
    dist

shortestClimb weights grid
|> printfn "part 2: %d"
