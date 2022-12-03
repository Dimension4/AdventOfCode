type Shape =
    | Rock = 0
    | Paper = 1
    | Scissors = 2

let parseShape = function
    | "A" | "X" -> Shape.Rock
    | "B" | "Y" -> Shape.Paper
    | "C" | "Z" -> Shape.Scissors

let arrayToTuple [| a; b |] = a, b
let split (s: string) = s.Split(' ')

let score (a: Shape, b: Shape) = ((4 + int b - int a) % 3) * 3 + 1 + int b

let input = fsi.CommandLineArgs[1].Split('\n') |> Array.map split

input
|> Seq.map (Array.map parseShape >> arrayToTuple >> score)
|> Seq.sum
|> printfn "part 1: %i"

// part 2

let parseOutcome outcome shape =
    shape, (int shape + match outcome with | "X" -> 2 | "Y" -> 0 | "Z" -> 1) % 3 |> enum<Shape>

input
|> Seq.map (fun [| a; b |] -> a |> parseShape |> parseOutcome b |> score)
|> Seq.sum
|> printfn "part 2: %i"
