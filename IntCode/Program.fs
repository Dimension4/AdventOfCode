// https://adventofcode.com/2019/day/2

module IntCode

open System

type BinaryOp =
    { Lhs: int
      Rhs: int
      Destination: int }

type Instruction =
    | Add of BinaryOp
    | Multiply of BinaryOp
    | Halt

type OpCode =
    | Add = 1
    | Multiply = 2
    | Halt = 99

type ProgramState =
    | Updated of int array
    | Finished of int

let execute instruction (memory: int array) =
    match instruction with
    | Add op ->
        memory.[op.Destination] <- memory.[op.Lhs] + memory.[op.Rhs]
        Updated memory
    | Multiply op ->
        memory.[op.Destination] <- memory.[op.Lhs] * memory.[op.Rhs]
        Updated memory
    | Halt -> Finished memory.[0]


let parse (mem: int array) pos =
    match (enum<OpCode> mem.[pos]) with
        | OpCode.Add -> Ok (Add {Lhs = mem.[pos + 1]; Rhs = mem.[pos + 2]; Destination = mem.[pos + 3]})
        | OpCode.Multiply -> Ok (Multiply { Lhs = mem.[pos + 1]; Rhs  = mem.[pos + 2]; Destination = mem.[pos + 3] })
        | OpCode.Halt -> Ok Halt
        | x -> Error (sprintf "Invalid op code %d at %d. Aborting." (int x) pos)


let rec interpret (mem: int array) pos =
    let parseResult = parse mem pos
    match parseResult with
    | Ok instruction ->
        match execute instruction mem with
        | Updated memory -> interpret memory (pos + 4)
        | Finished result -> Ok result
    | Error msg -> Error msg

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
    | Error msg -> printfn "%s" msg; 1