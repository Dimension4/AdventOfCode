open System
open System.IO

let file = File.ReadAllLines (__SOURCE_DIRECTORY__ + "/04.txt")

let numbers = file.[0].Split(',') |> Seq.map int |> Seq.toList

let parseBoard = 
    Array.take 5     
    >> Array.map (fun (x: string) -> x.Split(' ', StringSplitOptions.RemoveEmptyEntries))
    >> Array.concat
    >> Array.map int

let boards = 
    file.[2..]
    |> Seq.chunkBySize 6
    |> Seq.map parseBoard
    |> Seq.toArray

let guesses = Array.init boards.Length (fun _ -> Array.zeroCreate<int8> 25)

let update guess (boards: int[][]) (guesses: int8[][]) = 
    let updateBoard (board: int[]) (guesses: int8[]) =
        Array.iteri (fun i b -> if b = guess then guesses.[i] <- 1y else ()) board        
    
    Array.iter2 updateBoard boards guesses

let checkWinner (guesses: int8[][]) = 
    let checkBoard board = 
        let indices = 
            Seq.concat [
                Seq.init 5 (fun i -> Array.init 5 (fun j -> i * 5 + j))
                Seq.init 5 (fun i -> Array.init 5 (fun j -> i + 5 * j))
            ]

        Seq.exists (Array.map (Array.get board) >> Array.sum >> (=) 5y) indices

    Seq.indexed guesses
    |> Seq.tryFind (snd >> checkBoard)
    |> Option.map fst

type Winner = {
    BoardIndex: int
    Board: int[]
    Guesses: int8[]
    Guess: int
    LeftGuesses: int list
}

let rec run boards guesses = function
    | num::rest -> 
        update num boards guesses
        match checkWinner guesses with
        | Some i -> 
            Some { 
                BoardIndex = i
                Board = boards.[i]
                Guesses = guesses.[i]
                Guess = num
                LeftGuesses = rest 
            }
        | _ -> run boards guesses rest
    | _ -> None

let calcScore winner =
    Seq.init 25 id 
    |> Seq.filter (Array.get winner.Guesses >> (=) 0y)
    |> Seq.map (Array.get winner.Board)
    |> Seq.sum
    |> (*) winner.Guess

let winner = run boards guesses numbers |> Option.get
let score = calcScore winner

printfn $"Day 04 Part One: {score}"

let rec lastWinner boards guesses currentWinner =
    match run boards guesses currentWinner.LeftGuesses with
    | Some winner -> 
        let boards = Array.removeAt winner.BoardIndex boards
        let guesses = Array.removeAt winner.BoardIndex guesses
        printfn $"{winner}"
        lastWinner boards guesses winner
    | None -> printfn $"{currentWinner}"; currentWinner

let score2 = lastWinner boards guesses winner |> calcScore

printfn $"Day 04 Part Two: {score2}"