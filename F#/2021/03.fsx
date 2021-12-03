open System.IO

let parse (s: string) = System.Convert.ToInt32(s, 2)

let data = 
    __SOURCE_DIRECTORY__ + "/03.txt"
    |> File.ReadAllLines
    |> Array.map parse

let mostCommonBit data index = 
    let mask = 1 <<< index
    let count = 
        data
        |> Seq.filter (fun x -> (x &&& mask) <> 0)
        |> Seq.length
    if Array.length data - count <= count then 1 else 0

let width = 12

let gamma = 
    [0..width - 1]
    |> Seq.map (fun x -> (mostCommonBit data x) <<< x)
    |> Seq.sum

let mask = (1 <<< width) - 1
let epsilon = ~~~gamma &&& mask

printfn $"Day 3 Part One: {gamma * epsilon}"

let rec filter criteria data index = 
    let mcb = mostCommonBit data index
    let getBit x = (x >>> index) &&& 1
    let data = Array.filter (getBit >> criteria mcb) data
    if Array.length data > 1 then 
        filter criteria data (index - 1)
    else
        data.[0]

let oxygen = filter (=) data (width - 1)
let co2 = filter (<>) data (width - 1)

printfn $"Day 3 Part Two: {oxygen * co2}"