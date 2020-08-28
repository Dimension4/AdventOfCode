module Solver

let rec private bruteForceSolve (memory: int array) instructionPtr result noun verb inputFunc outputFunc =
    let mem = Array.copy memory
    mem.[1..2] <- [| noun; verb |]
    match Interpreter.interpret mem instructionPtr inputFunc outputFunc with
    | Ok res when res = result -> Ok (noun * 100 + verb)
    | _ when noun < 99 || verb < 99 ->
        let nextVerb = (verb + 1) % 100
        let nextNoun = if nextVerb > verb then noun else noun + 1
        bruteForceSolve memory instructionPtr result nextNoun nextVerb inputFunc outputFunc
    | _ -> Error "Program could not be solved."


let findNounAndVerb (memory: int array) instructionPtr result inputFunc outputFunc =
    bruteForceSolve memory instructionPtr result 0 0 inputFunc outputFunc


let rec private distribute e = function
| [] -> [[e]]
| x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]


let rec private permute = function
| [] -> [[]]
| e::xs -> List.collect (distribute e) (permute xs)


let private runAmps (memory: int array) numAmps phases =
    let mutable outFuncs, inFuncs = seq { for _ in 2..numAmps -> IO.createIoPipe () } |> Seq.toList |> List.unzip
    let finalOutput, outFunc = IO.outputCapture ()
    inFuncs <- (fun () -> 0) :: inFuncs
    outFuncs <- outFuncs @ [outFunc]

    let run phase inp out = async {
        printfn "starting phase %d" phase // todo: remove
        let mem = Array.copy memory
        let input = IO.composeInputs (fun () -> phase) inp
        let res = Interpreter.interpret mem 0 input out
        printfn "finished phase %d: %A" phase res // todo: remove
    }
    printfn "phases: %A" phases // todo: remove
    (phases, inFuncs, outFuncs)
    |||> List.map3 run
    |> Async.Parallel
    |> Async.Ignore
    |> Async.RunSynchronously
    Seq.tryLast finalOutput


let findMaxAmpThrust (memory: int array) numAmps =
    let phases = [0..numAmps-1]
    let run = runAmps memory numAmps

    permute phases
    |> Seq.choose run
    |> Seq.max
    |> Ok

