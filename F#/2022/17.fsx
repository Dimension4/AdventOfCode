let pattern = fsi.CommandLineArgs[1]

// let rec simulate stepCount (board: int[]) =
//     let nextPat x = (x + 1) % pattern.Length
//     let rockWidth = [| 4; 3; 3; 1; 2 |]
//     let rec spawn i pat rock =
//         if i >= stepCount then Array.max board else
//         let xmax = Array.length board - rockWidth[rock]
//         let collides x y =
//             match rock with
//             | 0 -> [0..3] |> List.exists (fun i -> board[x + i] = y)
//             | 1 -> board[x] = y + 1 || board[x + 1] = y || board[x + 2] = y + 1
//             | 2 -> [0..2] |> List.exists (fun i -> board[x + i] = y)
//             | 3 -> board[x] = y
//             | 4 -> [0..1] |> List.exists (fun i -> board[x + i] = y)
//         let rec fall pat x y =
//             if collides x y then
//                 match rock with
//                 | 0 -> [0..3] |> List.iter (fun i -> board[x + i] <- y + 1)
//                 | 1 ->
//                     board[x] <- y + 2
//                     board[x + 1] <- y + 3
//                     board[x + 2] <- y + 2
//                 | 2 ->
//                     board[x] <- y + 1
//                     board[x + 1] <- y + 1
//                     board[x + 2] <- y + 3
//                 | 3 -> board[x] <- y + 4
//                 | 4 ->
//                     board[x] <- y + 2
//                     board[x + 1] <- y + 2
//                 pat
//             else
//                 match pattern[pat] with
//                 | '<' -> fall (nextPat pat) (max 0 (x - 1)) (y - 1)
//                 | '>' -> fall (nextPat pat) (min xmax (x + 1)) (y - 1)
//         let pat = fall pat 2 (Array.max board + 4)
//         spawn (i + 1) pat ((rock + 1) % 5)
//     spawn 0 0 0

let rocks = [|
    array2D ["####"]
    array2D [" # "; "###"; " # "]
    array2D ["  #"; "  #"; "###"]
    array2D ("####" |> Seq.map Seq.singleton)
    array2D ["##"; "##"]
|]

module Array2D =
    let existsi pred arr =
        let rec iter x y =
            if x >= Array2D.length2 arr then iter 0 (y + 1)
            elif y >= Array2D.length1 arr then false
            elif pred y x arr[y, x] then true
            else iter (x + 1) y
        iter 0 0

    let copyMask mask src x y dst  =
        let rec copy x1 y1 =
            if x1 >= Array2D.length2 src then copy 0 (y1 + 1)
            elif y1 >= Array2D.length1 src then ()
            else
                if src[y1, x1] = mask then Array2D.set dst (y + y1) (x + x1) mask
                copy (x1 + 1) y1
        copy 0 0

let rec simulate stepCount (board: char[,]) (rocks: char[,][]) (pattern: string) =
    let nextPat x = (x + 1) % pattern.Length
    let nextRock x = (x + 1) % rocks.Length
    let rec spawn i (pat: int) rockIdx height =
        if i >= stepCount then height - 4 else
        let rock = rocks[rockIdx]
        let overlaps x y =
            rock |> Array2D.existsi (fun y1 x1 v ->
                let y = y + Array2D.length1 rock - y1 - 1
                let x = x + x1
                if x < 0 || x >= Array2D.length2 board || y < 0 then true
                else v = '#' && board[y, x] = '#')
        let rec fall pat x y =
            if overlaps x y then
                let ymax = (y + 1 + Array2D.length1 rock)
                Array2D.copyMask '#' rock x ymax board
                pat, ymax
            else
                match pattern[pat] with
                | '<' ->
                    let x = if overlaps (x - 1) y then x else x - 1
                    fall (nextPat pat) x (y - 1)
                | '>' ->
                    let x = if overlaps (x + 1) y then x else x + 1
                    fall (nextPat pat) x (y - 1)
        let pat, ymax = fall pat 2 (height + 4)
        spawn (i + 1) pat (nextRock rockIdx) ymax
    spawn 0 0 0 0

let steps = 1
let board = Array2D.create (steps * 4 + 4) 7 '.'

let height = simulate steps board rocks pattern
printfn "part 1: %d" height

for y in 0..height do
    for x in 0..Array2D.length2 board - 1 do printf $"{board[y, x]}"
    printfn ""
