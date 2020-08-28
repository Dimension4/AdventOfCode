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
        Console.WriteLine "Bad input."
        readUserInput msg


let load (mem: int array) operandAddress paramMode =
    match paramMode with
    | ParamMode.Position -> mem.[mem.[operandAddress]]
    | ParamMode.Immediate -> mem.[operandAddress]
    | x -> sprintf "Unexpected ParamMode: %d." (int x) |> ArgumentException |> raise


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
    (load 0, load 1) ||> op |> store 2


let jumpOp load pos condition =
    match load 0 with
    | x when x <> 0 = condition -> load 1
    | _ -> pos + 3


let lessThan a b = if a < b then 1 else 0
let equalsTo a b = if a = b then 1 else 0


let rec interpret (mem: int array) pos inputFunc outputFunc =
    let opCode, paramModes = decodeInstruction mem.[pos]
    let load idx = load mem (pos + idx + 1) paramModes.[idx]
    let store idx = store mem (pos + idx + 1)
    let binaryOp op = binaryOp load store op; pos + 4
    let jumpOp = jumpOp load pos

    let newPos =
        match opCode with
        | OpCode.Add -> binaryOp (+)
        | OpCode.Multiply -> binaryOp (*)
        | OpCode.Input ->
            inputFunc () |> store 0
            pos + 2
        | OpCode.Output ->
            load 0 |> outputFunc
            pos + 2
        | OpCode.JumpIfTrue -> jumpOp true
        | OpCode.JumpIfFalse -> jumpOp false
        | OpCode.LessThan -> binaryOp lessThan
        | OpCode.EqualsTo -> binaryOp equalsTo
        | x -> -1

    match opCode, newPos with
    | OpCode.Halt, _ -> Ok mem.[0]
    | _, x when x >= 0 -> interpret mem x inputFunc outputFunc
    | op, _ -> sprintf "Error. Core dumped.\n\n%A\n\nInvalid op code %d at %d. Aborting." mem (int op) pos |> Error
