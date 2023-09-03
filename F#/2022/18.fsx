open System
open System.IO

let parse (s: string) =
    s.Split([| '\n'; ',' |], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.map int

[<Struct>]
type Point =
    { X: int; Y: int; Z: int}
    static member (+)(a, b) = { X = a.X + b.X; Y = a.Y + b.Y; Z = a.Z + b.Z }
    static member (-)(a, b) = { X = a.X - b.X; Y = a.Y - b.Y; Z = a.Z - b.Z }
    static member (*)(a, b) = { X = a.X * b; Y = a.Y * b; Z = a.Z * b }
    static member (/)(a, b) = { X = a.X / b; Y = a.Y / b; Z = a.Z / b }

module Point =
    let zero = { X = 0; Y = 0; Z = 0 }
    let uniform value = { X = value; Y = value; Z = value }
    let fromArray [| a; b; c |] = { X = a; Y = b; Z = c }
    let toArray p = [| p.X; p.Y; p.Z |]
    let toTuple p = p.X, p.Y, p.Z

let normalizePoints points =
    let axes = points |> Array.chunkBySize 3 |> Array.transpose
    let mins, maxes = Array.map Array.min axes, Array.map Array.max axes
    let axes = (axes, mins) ||> Array.map2 (fun ax m -> Array.map (fun x -> x - m) ax)
    let maxes = (maxes, mins) ||> Array.map2 (-)
    let max' = Point.fromArray maxes
    max', axes |> Array.transpose |> Array.map Point.fromArray

[<Flags>]
type CellState =
    | None = 0uy
    | Visited = 0b01uy
    | Blocked = 0b10uy
    | Forbidden = 0b11uy

module Array =
    let allPairs3 a b c =
        Array.allPairs b c
        |> Array.allPairs a
        |> Array.map (fun (a, (b, c)) -> a, b, c)

let buildGrid maxBounds points =
    let offset = Point.uniform 2
    let max' = maxBounds + offset * 2
    let points = Array.map ((+) offset) points
    let grid = max' + Point.uniform 1 |> Point.toTuple |||> Array3D.create <| CellState.Blocked
    for x, y, z in points |> Array.map Point.toTuple do
        grid[x, y, z] <- CellState.None
    [|
        Array.allPairs3 [| 0; max'.X |] [| 0..max'.Y |] [| 0..max'.Z |]
        Array.allPairs3 [| 0..max'.X |] [| 0; max'.Y |] [| 0..max'.Z |]
        Array.allPairs3 [| 0..max'.X |] [| 0..max'.Y |] [| 0; max'.Z |]
    |]
    |> Array.concat
    |> Array.iter (fun (x, y, z) -> grid[x, y, z] <- CellState.Blocked ||| CellState.Visited)
    points, grid

let invertGrid grid =
    for x in [| 0..Array3D.length1 grid - 1 |] do
        for y in [| 0..Array3D.length2 grid - 1 |] do
            for z in [| 0..Array3D.length3 grid - 1 |] do
                grid[x, y, z] <-
                    match grid[x, y, z] with
                    | CellState.Blocked -> CellState.None
                    | CellState.None -> CellState.Blocked
                    | x -> x
    grid

let rec floodFillWallCount (grid: CellState[,,]) acc = function
    | (x, y, z)::tl ->
        let cur = &grid[x, y, z]
        if cur.HasFlag(CellState.Visited) then floodFillWallCount grid acc tl
        elif cur.HasFlag(CellState.Blocked) then floodFillWallCount grid (acc + 1) tl
        else
            cur <- cur ||| CellState.Visited
            let next = [
                (x + 1, y, z); (x - 1, y, z)
                (x, y + 1, z); (x, y - 1, z)
                (x, y, z + 1); (x, y, z - 1)
            ]
            floodFillWallCount grid acc (next @ tl)
    | [] -> acc

let measureExteriorSurface grid =
    let grid = invertGrid grid
    floodFillWallCount grid 0 [(1, 1, 1)]

let measureSurface points grid =
    points
    |> Array.map (Point.toTuple >> List.singleton)
    |> Array.sumBy (floodFillWallCount grid 0)

let setup input = input |> parse |> normalizePoints ||> buildGrid

File.ReadAllText $"{__SOURCE_DIRECTORY__}/18.txt"
|> setup
||> measureSurface
|> printfn "part 1: %d"

File.ReadAllText $"{__SOURCE_DIRECTORY__}/18.txt"
|> setup
|> snd
|> measureExteriorSurface
|> printfn "part 2: %d"