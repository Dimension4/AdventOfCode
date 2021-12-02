open System.IO

type Instruction = 
    | Forward of int
    | Up of int
    | Down of int

let parse (s: string) =
    match s.Split(" ") with
    | [| "forward"; x |] -> int x |> Forward
    | [| "up"; x |] -> int x |> Up
    | [| "down"; x |] -> int x |> Down

let interpret (hor, depth) = function
    | Forward x -> (hor + x, depth)
    | Up x -> (hor, depth - x)
    | Down x -> (hor, depth + x)

let instructions = 
    __SOURCE_DIRECTORY__ + "/02.txt"
    |> File.ReadAllLines
    |> Array.map parse

let (hor, depth) = Array.fold interpret (0, 0) instructions
printfn $"Day 2 Part One: {hor * depth}"

let interpret2 (hor, depth, aim) = function
    | Forward x -> (hor + x, depth + aim * x, aim)
    | Up x -> (hor, depth, aim - x)
    | Down x -> (hor, depth, aim + x)
    
let (hor2, depth2, _) = Array.fold interpret2 (0, 0, 0) instructions
printfn $"Day 2 Part Two: {hor2 * depth2}"