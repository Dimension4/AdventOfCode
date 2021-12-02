open System.IO

let input = File.ReadAllLines (__SOURCE_DIRECTORY__ + "/01.txt") |> Array.map int

let countIncreases: int seq -> int = 
    Seq.windowed 2
    >> Seq.filter (fun [|a; b|] -> b > a)
    >> Seq.length

countIncreases input |> printfn "Day 1 Part One: %i"

input
|> Seq.windowed 3
|> Seq.map Array.sum
|> countIncreases
|> printfn "Day 1 Part Two: %i"
