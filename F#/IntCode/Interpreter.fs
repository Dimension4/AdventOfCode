module Interpreter

open System

type OpCode =
    | Add = 1
    | Multiply = 2
    | Input = 3
    | Output = 4
    | JumpIfTrue = 5
    | JumpIfFalse = 6
    | LessThan = 7
    | EqualsTo = 8
    | Halt = 99

type ParamMode =
    | Position = 0
    | Immediate = 1


let rec readUserInput (msg: string) =
    Console.Write(msg)
    let input = Console.ReadLine() |> Int32.TryParse
    match input with
    | true, num -> num
    | _ ->
        Console.WriteLine "Bad input"
        readUserInput msg


let load (mem: int array) operandAddress paramMode =
    match paramMode with
    | ParamMode.Position -> mem.[mem.[operandAddress]]
    | ParamMode.Immediate -> mem.[operandAddress]
    | x -> sprintf "Unexpected ParamMode: %d" (int x) |> ArgumentException |> raise


let store (mem: int array) operandAddress value =
    mem.[mem.[operandAddress]] <- value


let decodeInstruction instruction =
    instruction % 100 |> enum<OpCode>,
    [|
        instruction / 100 % 10 |> enum<ParamMode>
        instruction / 1000 % 10 |> enum<ParamMode>
        instruction / 10000 % 10 |> enum<ParamMode>
    |]


let binaryOp load store op =
    (load 0, load 1) ||> op  |> store 2


let jumpOp load pos condition =
    match load 0 with
    | x when x <> 0 = condition -> load 1
    | _ -> pos + 3

let lessThan a b = if a < b then 1 else 0
let equalsTo a b = if a = b then 1 else 0


let rec interpret (mem: int array) pos =
    let opCode, paramModes = decodeInstruction mem.[pos]
    let load idx = load mem (pos + idx + 1) paramModes.[idx]
    let store idx = store mem (pos + idx + 1)
    let binaryOp = binaryOp load store
    let jumpOp = jumpOp load pos

    match opCode with
        | OpCode.Add ->
            binaryOp (+)
            interpret mem (pos + 4)
        | OpCode.Multiply ->
            binaryOp (*)
            interpret mem (pos + 4)
        | OpCode.Input ->
            readUserInput "Input: " |> store 0
            interpret mem (pos + 2)
        | OpCode.Output ->
            load 0 |> printfn "Output: %d"
            interpret mem (pos + 2)
        | OpCode.JumpIfTrue ->
            jumpOp true |> interpret mem
        | OpCode.JumpIfFalse ->
            jumpOp false |> interpret mem
        | OpCode.LessThan ->
            binaryOp lessThan
            interpret mem (pos + 4)
        | OpCode.EqualsTo ->
            binaryOp equalsTo
            interpret mem (pos + 4)
        | OpCode.Halt -> Ok mem.[0]
        | x -> Error (sprintf "Error. Core dumped.\n\n%A\n\nInvalid op code %d at %d. Aborting." mem (int x) pos)


let rec bruteForceSolve (memory: int array) instructionPtr result noun verb =
    let mem = Array.copy memory
    mem.[1..2] <- [| noun; verb |]
    match interpret mem instructionPtr with
    | Ok res when res = result -> Ok (noun * 100 + verb)
    | _ when noun < 99 || verb < 99 ->
        let nextVerb = (verb + 1) % 100
        let nextNoun = if nextVerb > verb then noun else noun + 1
        bruteForceSolve memory instructionPtr result nextNoun nextVerb
    | _ -> Error "Program could not be solved."


let solve (memory: int array) instructionPtr result =
    bruteForceSolve memory instructionPtr result 0 0