let split (x: char) (s: string) = s.Split(x)

let pairs =
    fsi.CommandLineArgs[1]
    |> split '\n'
    |> Seq.map (split ',' >> Array.map (split '-' >> Array.map int))

pairs
|> Seq.filter (fun [| [| a; b |]; [| c; d |] |] -> a >= c && b <= d || c >= a && d <= b)
|> Seq.length
|> printfn "part 1: %d"

// part 2

pairs
|> Seq.filter (fun [| [| a; b |]; [| c; d |] |] -> a <= c && c <= b || c <= a && a <= d)
|> Seq.length
|> printfn "part 2: %d"
