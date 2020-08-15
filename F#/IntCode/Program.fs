// https://adventofcode.com/2019/day/2

module IntCode

let split (sep: char) (s: string) = s.Split(sep)

let removeWhitespaces (s: string) = s.Replace(" ", "")

let createMemory : (string seq -> int array) = String.concat "" >> removeWhitespaces >> split ',' >> Array.map int

[<EntryPoint>]
let main argv =
    let state =
        match Array.toList argv with
        | "-solve" :: target :: mem -> Interpreter.solve (createMemory mem) 0 (int target)
        | mem -> Interpreter.interpret (createMemory mem) 0

    match state with
    | Ok result -> printfn "Result: %d" result; 0
    | Error msg -> eprintfn "%s" msg; 1