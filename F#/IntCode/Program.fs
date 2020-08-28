// https://adventofcode.com/2019/day/2

module IntCode

let split (sep: char) (s: string) = s.Split(sep)

let removeWhitespaces (s: string) = s.Replace(" ", "")

let createMemory : (string -> int array) = removeWhitespaces >> split ',' >> Array.map int

[<EntryPoint>]
let main argv =
    let state =
        match Array.toList argv with
        | "-solve" :: target :: [mem] -> Solver.findNounAndVerb (createMemory mem) 0 (int target) IO.readFromConsole IO.printToConsole
        | "-amp" :: [mem] -> Solver.findMaxAmpThrust (createMemory mem) 5
        | [mem] -> Interpreter.interpret (createMemory mem) 0 IO.readFromConsole IO.printToConsole
        | _ -> Error("Invalid program input.")

    match state with
    | Ok result -> printfn "Result: %d" result; 0
    | Error msg -> eprintfn "%s" msg; 1