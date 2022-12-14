let split (x: string) (s: string) = s.Split(x, System.StringSplitOptions.RemoveEmptyEntries)
let arrayToTuple [| x; y |] = x, y

let rockPaths =
    fsi.CommandLineArgs[1]
    |> split "\n"
    |> Array.map (split " -> " >> Array.map (split "," >> Array.map int >> arrayToTuple))

let sandSpawn = (500, 0)

let lowerBound, upperBound =
    rockPaths
    |> Seq.concat
    |> Seq.fold (fun ((xmin, ymin), (xmax, ymax)) (x, y) ->
        ((min xmin x, min ymin y), (max xmax x, max ymax y))) (sandSpawn, sandSpawn)

let createMap (xmin, ymin) (xmax, ymax) rockPaths =
    let map = Array2D.create (ymax - ymin + 1) (xmax - xmin + 1) '.'

    let offset (x, y) = x - xmin, y - ymin

    let rec drawRock ((x0, y0), (x1, y1)) =
        let dx, dy = sign (x1 - x0), sign (y1 - y0)
        map[y0, x0] <- '#'
        if dx = 0 && dy = 0 then ()
        else drawRock ((x0 + dx, y0 + dy), (x1, y1))

    rockPaths |> Seq.iter (Seq.map offset >> Seq.pairwise >> Seq.iter drawRock)
    map

let map = createMap lowerBound upperBound rockPaths

let drawMap =
    Array2D.iteri (fun y x (a: char) ->
        if x = 0 && y > 0 then printfn ""
        printf $"{a}")

let simulateSand map (xsand, ysand) (xmin, ymin) =
    let sandSpawn = xsand - xmin, ysand - ymin
    let w, h = Array2D.length2 map, Array2D.length1 map
    let outOfBounds x  y = x < 0 || x >= w || y < 0 || y >= h
    let tryGet x y = if outOfBounds x y then None else Some map[y, x]
    let fallRules = [0; -1; 1]
    let rec spawnSand count =
        let rec fall (x, y) =
            let rec probe = function
                | r::rs ->
                    match tryGet (x + r) (y + 1) with
                    | Some '.' -> fall ((x + r), (y + 1))
                    | None -> None
                    | _ -> probe rs
                | [] when map[y, x] = '.' -> Some (x, y)
                | [] -> None
            probe fallRules
        match fall sandSpawn with
        | Some (x, y) ->
            map[y, x] <- 'o'
            spawnSand (count + 1)
        | None -> count
    spawnSand 0

simulateSand map sandSpawn lowerBound
|> printfn "part 1: %d"

// drawMap map

// part 2

let newLowerBound, newMap =
    let _, ymax = upperBound
    let sx, _ = sandSpawn
    let hw = ymax + 2
    let lowerBound = (sx - hw, snd lowerBound)
    let upperBound = (sx + hw, ymax + 2)

    lowerBound,
    rockPaths
    |> Seq.append [[| (fst lowerBound, snd upperBound); upperBound |]]
    |> createMap lowerBound upperBound

simulateSand newMap sandSpawn newLowerBound
|> printfn "part 2: %d"

// drawMap newMap
