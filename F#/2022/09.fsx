let split (x: char) (s: string) = s.Split(x)

type Vec = { X: int; Y: int }

module Vec =
    let zero = { X = 0; Y = 0 }
    let abs a = { X = abs a.X; Y = abs a.Y }
    let planck a = { X = sign a.X; Y = sign a.Y }

let (+) a b = { X = a.X + b.X; Y = a.Y + b.Y }
let (-) a b = { X = a.X - b.X; Y = a.Y - b.Y }
let (<.>) a b =
    let x = Vec.abs (a - b)
    x.X <= 1 && x.Y <= 1

let moves =
    fsi.CommandLineArgs[1]
    |> split '\n'
    |> Seq.map (split ' ' >> function
        | [| "U"; x |] -> { X = 0; Y = int x }
        | [| "D"; x |] -> { X = 0; Y = -int x }
        | [| "L"; x |] -> { X = -int x; Y = 0 }
        | [| "R"; x |] -> { X = int x; Y = 0 })
    |> Seq.toList

let interpret ropeLen moves =
    let chase a b = if a <.> b then b else b + Vec.planck (a - b)
    let rec march (head::tail) visited = function
        | {X = 0; Y = 0}::moves -> march (head::tail) visited moves
        | move::moves ->
            let step = Vec.planck move
            let rope = List.scan chase (head + step) tail
            march rope (List.last rope::visited) (move-step::moves)
        | [] -> visited

    march (List.replicate ropeLen Vec.zero) [Vec.zero] moves

moves
|> interpret 2
|> Seq.distinct
|> Seq.length
|> printfn "part 1: %d"

// part 2

moves
|> interpret 10
|> Seq.distinct
|> Seq.length
|> printfn "part 2: %d"