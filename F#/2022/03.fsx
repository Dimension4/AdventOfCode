let rucksacks = fsi.CommandLineArgs[1].Split('\n') |> Seq.map Seq.toArray |> Seq.toList
let compartments = rucksacks |> List.map (Seq.splitInto 2 >> Seq.toList)

let findCommon (x::xs) = x |> Array.find (fun a -> List.forall (Array.contains a) xs)

let prioritize = function
    | x when x >= 'a' -> int x - int 'a' + 1
    | x -> int x - int 'A' + 27

compartments
|> Seq.sumBy (findCommon >> prioritize)
|> printfn "part 1: %d"

// part 2

rucksacks
|> List.chunkBySize 3
|> List.sumBy (findCommon >> prioritize)
|> printfn "part 2: %d"