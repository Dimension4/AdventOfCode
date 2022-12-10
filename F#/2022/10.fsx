let split (x: char) (s: string) = s.Split(x)

type Instruction =
    | Noop
    | Addx of int

let instructions =
    fsi.CommandLineArgs[1]
    |> split '\n'
    |> Seq.map (split ' ' >> function
        | [| "noop" |] -> Noop
        | [| "addx"; x |] -> int x |> Addx)
    |> Seq.toList

let interpret instructions =
    let rec step (reg::cycles) = function
        | Noop::rest -> step (reg::reg::cycles) rest
        | Addx x::rest -> step ((reg + x)::reg::reg::cycles) rest
        | [] -> cycles
    step [1] instructions
    |> List.rev

let cycles = interpret instructions

[20..40..220]
|> Seq.map (fun x -> x * List.item (x - 1) cycles)
|> Seq.sum
|> printfn "part 1: %d"

// part 2

printfn "part 2:"

for i, x in Seq.indexed cycles |> Seq.take 240 do
    let pos = i % 40
    if pos = 0 then printfn ""
    match x - pos with
    | -1 | 0 | 1 -> printf "#"
    | _ -> printf "."
