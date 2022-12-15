let mapping =
    let split (x: string[]) (s: string) = s.Split(x, System.StringSplitOptions.RemoveEmptyEntries)

    let tryParse (s: string) =
        match System.Int32.TryParse s with
        | true, x -> Some x
        | _ -> None

    fsi.CommandLineArgs[1]
    |> split [| "\n" |]
    |> Array.map (
        split [| " "; ","; "="; ":" |]
        >> Array.choose tryParse
        >> (fun [| a; b; c; d |] -> (a, b), (c, d)))
    |> Array.toList

let manhatten (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let rowCover row ((sx, sy), b) =
    let maxDist = manhatten (sx, sy) b
    let minDist = abs (row - sy)
    let hw = maxDist - minDist
    if hw >= 0 then Some (sx - hw, sx + hw) else None

let mergeCovers covers =
    let rec iter acc (a0, a1) = function
        | (b0, b1)::tl ->
            if b1 <= a1 then iter acc (a0, a1) tl
            elif a1 < b0 then iter ((a0, a1)::acc) (b0, b1) tl
            else iter acc (a0, b1) tl
        | [] -> (a0, a1)::acc |> List.rev
    match covers with
    | hd::tl -> iter [] hd tl
    | x -> x

let calcCover row =
    mapping
    |> List.choose (rowCover row)
    |> List.sortBy fst
    |> mergeCovers

let countBlocked row =
    let covered =
        calcCover row
        |> List.sumBy (fun (a, b) -> b - a + 1)
    let beaconOnRow =
        mapping
        |> List.map snd
        |> List.filter (fun (_, y) -> y = row)
        |> List.distinct
        |> List.length
    covered - beaconOnRow

countBlocked 2_000_000
|> printfn "part 1: %d"

// part 2

let findDistressBeacon limit =
    let rec iterCol x = function
        | _ when x > limit -> ValueNone
        | (x0, x1)::tl ->
            if x < x0 then ValueSome x
            elif x <= x1 then iterCol (x1 + 1) tl
            else iterCol (x + 1) tl
        | [] -> ValueSome x
    let rec iterRow y =
        if y > limit then failwith "out of bounds"
        match calcCover y |> iterCol 0 with
        | ValueSome x -> x, y
        | ValueNone -> iterRow (y + 1)
    iterRow 0

let tuningFreq (x, y) = int64 x * 4_000_000L + int64 y

findDistressBeacon 4_000_000
|> tuningFreq
|> printfn "part 2: %d"
