open System
open System.Collections.Generic

let split (x: string) (s: string) = s.Split(x, StringSplitOptions.RemoveEmptyEntries)
let rec gcd x y = if y = 0L then x else gcd y (x % y)
let lcm x y = x * y / (gcd x y)

let (|Int|_|) (s: string) =
    match Int64.TryParse s with
    | true, x -> Some x
    | _ -> None

type Monkey = {
    StartingItems: int64[]
    Operation: int64 -> int64
    Divisor: int64
    TrueTarget: int
    FalseTarget: int
}

let parseOperation line =
    let parseOp = function "*" -> (*) | "+" -> (+)
    let [| _; _; _; term1; ops; term2 |] = split " " line
    let op = parseOp ops
    match term1, term2 with
    | "old", "old" -> (fun x -> op x x)
    | "old", Int a -> (fun x -> op x a)
    | Int a, "old" -> (fun x -> op a x)

let parseMonkey [| _; items; op; test; trueCase; falseCase |] =
    {
        StartingItems = items |> Seq.skip 18 |> Seq.toArray |> String |> split ", " |> Array.map int64
        Operation = parseOperation op
        Divisor = test |> split " " |> Array.last |> int64
        TrueTarget = trueCase |> split " " |> Array.last |> int
        FalseTarget = falseCase |> split " " |> Array.last |> int
    }

let playRounds count worryLevelDivisor monkeys =
    let div = monkeys |> Seq.map (fun m -> m.Divisor) |> Seq.reduce lcm
    let worryLevels = monkeys |> Array.map (fun m -> Queue<_>(m.StartingItems))

    let rec work (q: Queue<_>) m c =
        match q.TryDequeue() with
        | true, lvl ->
            let newLevel = ((m.Operation lvl) / worryLevelDivisor) % div
            let idx = if newLevel % m.Divisor = 0L then m.TrueTarget else m.FalseTarget
            worryLevels[idx].Enqueue newLevel
            work q m (c + 1L)
        | _ -> c

    let rec play inspections = function
        | 0 -> inspections
        | n ->
            let inspections = Array.map3 work worryLevels monkeys inspections
            play inspections (n - 1)

    play (Array.zeroCreate monkeys.Length) count
    |> Array.sortDescending
    |> Array.take 2
    |> Array.reduce (*)

let monkeys =
    fsi.CommandLineArgs[1]
    |> split "\n"
    |> Array.chunkBySize 6
    |> Array.map parseMonkey

monkeys
|> playRounds 20 3
|> printfn "part 1: %d"

// part 2

monkeys
|> playRounds 10000 1
|> printfn "part 2: %d"
