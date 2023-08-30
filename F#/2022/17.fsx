open System
open System.Collections
open System.Runtime.CompilerServices
open System.Text

[<Struct>]
type Point =
    { Y: int64; X: int64 }
    static member (+)(a, b) = { Y = a.Y + b.Y; X = a.X + b.X }
    static member (-)(a, b) = { Y = a.Y - b.Y; X = a.X - b.X }

module Point =
    let zero = { X = 0; Y = 0 }
    let X p = p.X
    let Y p = p.Y

type Rock = { Shape: Point[]; Pos: Point }

type Move = Left | Right

let BoardWidth = 7L

module Seq =
    let takeLong (count: int64) (s: seq<_>) = seq {
        let mutable count = count
        use iter = s.GetEnumerator()
        while count > 0 && iter.MoveNext() do
            yield iter.Current
            count <- count - 1L
    }

module BitArray =
    let empty = BitArray 0

    let setMany value (arr: BitArray) (indices: seq<int>) =
        let copy = BitArray arr
        indices
        |> Seq.iter (fun i ->
            copy.Length <- max copy.Length (i + 1)
            copy.Set(i, value))
        copy

    let getOr fallback (arr: BitArray) index =
        if index < arr.Length then arr.Get index else fallback

let rockShapes = [|
    [| { X = 0L; Y = 0L }; { X = 1L; Y = 0L }; { X = 2L; Y = 0L }; { X = 3L; Y = 0L }; |]
    [| { X = 1L; Y = 0L }; { X = 0L; Y = 1L }; { X = 1L; Y = 1L }; { X = 2L; Y = 1L }; { X = 1L; Y = 2L } |]
    [| { X = 0L; Y = 0L }; { X = 1L; Y = 0L }; { X = 2L; Y = 0L }; { X = 2L; Y = 1L }; { X = 2L; Y = 2L } |]
    [| { X = 0L; Y = 0L }; { X = 0L; Y = 1L }; { X = 0L; Y = 2L }; { X = 0L; Y = 3L }; |]
    [| { X = 0L; Y = 0L }; { X = 0L; Y = 1L }; { X = 1L; Y = 0L }; { X = 1L; Y = 1L } |]
|]

let rec generateShapes () = seq {
    yield [| { X = 0L; Y = 0L }; { X = 1L; Y = 0L }; { X = 2L; Y = 0L }; { X = 3L; Y = 0L }; |]
    yield [| { X = 1L; Y = 0L }; { X = 0L; Y = 1L }; { X = 1L; Y = 1L }; { X = 2L; Y = 1L }; { X = 1L; Y = 2L } |]
    yield [| { X = 0L; Y = 0L }; { X = 1L; Y = 0L }; { X = 2L; Y = 0L }; { X = 2L; Y = 1L }; { X = 2L; Y = 2L } |]
    yield [| { X = 0L; Y = 0L }; { X = 0L; Y = 1L }; { X = 0L; Y = 2L }; { X = 0L; Y = 3L }; |]
    yield [| { X = 0L; Y = 0L }; { X = 0L; Y = 1L }; { X = 1L; Y = 0L }; { X = 1L; Y = 1L } |]
    yield! generateShapes ()
}

let plot (board: Set<Point>) =
    let yMin = board.MinimumElement.Y
    let yMax = board.MaximumElement.Y
    let ySpace = yMax |> abs |> float |> log10 |> int |> (+) 2
    let width = 7 + ySpace + 1
    let buf = Array.create (int (yMax - yMin + 1L) * width) '.'
    for i in (width - 1)..width..(buf.Length - 1) do buf[i] <- '\n'
    for y in yMin..yMax do y.ToString().PadLeft(ySpace).CopyTo(buf.AsSpan(7 + int (yMax - y) * width))
    for p in board do buf[int p.X + int (yMax - p.Y) * width] <- '#'
    Console.WriteLine(buf)

let plotDiff (first: Set<Point>) (second: Set<Point>) =
    let yMin = first.MinimumElement.Y
    let yMax = first.MaximumElement.Y
    let ySpace = yMax |> abs |> float |> log10 |> int |> (+) 2
    let width = 7 + ySpace + 1
    let buf = Array.create (int (yMax - yMin + 1L) * width) '.'
    for i in (width - 1)..width..(buf.Length - 1) do buf[i] <- '\n'
    for y in yMin..yMax do y.ToString().PadLeft(ySpace).CopyTo(buf.AsSpan(7 + int (yMax - y) * width))
    for p in first do buf[int p.X + int (yMax - p.Y) * width] <- '#'
    Console.WriteLine(buf)


let translate r = Array.map (fun p -> p + r.Pos) r.Shape

let toBoardIndex (x, y) = x + y * BoardWidth

let collides rock board =
    let points = translate rock
    Array.exists (fun p -> p.X < 0L || p.X >= 7L || p.Y < 0L) points ||
    points |> Array.exists (fun p -> Set.contains p board)

let tryMove board x y rock =
    let test = { rock with Pos = rock.Pos + { X = x; Y = y } }
    if collides test board then false, rock else true, test

let upperBound rock boardHeight = translate rock |> Array.map Point.Y |> Array.max |> max boardHeight


let optimizeBoard board yMax =
    let mutable visited = Set.empty
    let mutable result = Set.empty
    let rec visit p =
        if Set.contains p visited then
            ()
        else
            visited <- Set.add p visited
            if Set.contains p board then
                result <- Set.add p result
            else
                if p.X + 1L < BoardWidth then visit { p with X = p.X + 1L }
                if p.X - 1L >= 0L then visit { p with X = p.X - 1L }
                if p.Y > 0L then visit { p with Y = p.Y - 1L }
    visit { X = 0; Y = yMax + 1L }
    // printfn $"optimizatized from {board.Count} to {result.Count} (y-max: {yMax})"
    // plot board
    // plot result
    result

let removeMoveRepetitions (moves: string) =
    let arr = moves.ToCharArray()
    let chunkSizes = [| for i in 2..arr.Length do if arr.Length % i = 0 then i |]
    chunkSizes
    |> Array.filter (fun size ->
        arr
        |> Array.chunkBySize size
        |> Array.pairwise
        |> Array.fold (fun s (a, b) -> s && Array.forall2 (=) a b ) true)
    |> Array.tryHead
    |> Option.defaultValue arr.Length
    |> Array.sub arr 0
    |> String

let checkCycle board rockIdx moveIdx =
    if rockIdx = 0 && moveIdx = 0 then
        let maxY = Set.maxElement board |> Point.Y
        let maxLevelRocks = board |> Set.filter (Point.Y >> (=) maxY) |> Set.count
        maxLevelRocks = 7
    else
        false

let run moves (rockCount: int64) =
    let moves' = removeMoveRepetitions moves
    printfn $"Move optimization from {moves.Length} to {moves'.Length}"
    let mutable moveIdx = -1
    let nextMove =
        let moves = moves |> Seq.map (function '<' -> -1L | '>' -> +1L) |> Seq.toArray
        (fun () -> moveIdx <- (moveIdx + 1) % moves.Length; moves[moveIdx])
    let mutable optimizationThreshold = 64

    let rec step rock board yMax =
        let board =
            if Set.count board > optimizationThreshold then
                let res = optimizeBoard board yMax
                optimizationThreshold <- Set.count res * 2
                res
            else
                board
        let move = nextMove ()
        match
            rock
            |> tryMove board move 0
            |> snd
            |> tryMove board 0 -1
        with
            | true, newRock ->
                // plot board (Some newRock)
                step newRock board yMax
            | false, newRock ->
                // plot (Array.append board (translate newRock)) (Some newRock)
                let newBoard = translate newRock |> Set.ofArray |> Set.union board
                newBoard, (upperBound rock yMax)

    let _, yMax =
        seq { 1L..rockCount }
        |> Seq.fold (fun (board, yMax) count ->
            let rockIdx = int (count % int64 rockShapes.Length)
            if checkCycle board rockIdx ((moveIdx + 1) % moves.Length) then
                printfn $"cycle after {count} iterations"
                exit 0
            if count % 1_000_000L = 0L then printfn $"iteration {count:N0}"
            step { Shape = rockShapes[rockIdx]; Pos = { X = 2; Y = yMax + 4L } } board yMax)
            (Set.empty, -1L)
    yMax

run fsi.CommandLineArgs[1] 2022
|> printfn "part 1: %d"

run fsi.CommandLineArgs[1] 1_000_000_000_000L
|> printfn "part 1: %d"
