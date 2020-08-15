module Interpreter

open System

type BinaryOp =
    { Lhs: int
      Rhs: int
      Destination: int }

type OpCode =
    | Add = 1
    | Multiply = 2
    | Input = 3
    | Output = 4
    | Halt = 99

type ParamMode =
    | Position = 0
    | Immediate = 1

type InterpreterAction =
    | Increment of int
    | Halt of int
    | Abort of string


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
    op (load 0) (load 1) |> store 2


let execute (mem: int array) pos =
    let opCode, paramModes = decodeInstruction mem.[pos]
    let load idx = load mem (pos + idx + 1) paramModes.[idx]
    let store idx = store mem (pos + idx + 1)
    let binaryOp = binaryOp load store
    match opCode with
        | OpCode.Add -> binaryOp (+); Increment 4
        | OpCode.Multiply -> binaryOp (*); Increment 4
        | OpCode.Input -> readUserInput "Input: " |> store 0; Increment 2
        | OpCode.Output -> load 0 |> printfn "Output: %d"; Increment 2
        | OpCode.Halt -> Halt mem.[0]
        | x -> Abort (sprintf "Invalid op code %d at %d. Aborting." (int x) pos)


let rec interpret (memory: int array) instructionPtr =
    match execute memory instructionPtr with
    | Increment offset -> interpret memory (instructionPtr + offset)
    | Halt result -> Ok result
    | Abort msg -> Error msg


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