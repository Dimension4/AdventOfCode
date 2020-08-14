module Interpreter

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


let rec interpret (memory: int array) instructionPtr =        
    let parseResult = parse memory instructionPtr
    match parseResult with 
    | Ok instruction ->
        match execute instruction memory with
        | Updated memory -> interpret memory (instructionPtr + 4)
        | Finished result -> Ok result
    | Error msg -> Error msg


let rec bruteForceSolve (memory: int array) instructionPtr result noun verb =
    let mem = Array.copy memory
    mem.[1..2] <- [| noun; verb |]
    match interpret mem instructionPtr with
    | Ok res when res = result -> Ok (noun * 100 + verb)
    | _ when noun < 100 && verb < 99 -> 
        let nextVerb = (verb + 1) % 100
        let nextNoun = if nextVerb > verb then noun else noun + 1
        bruteForceSolve mem instructionPtr result nextNoun nextVerb
    | _ -> Error "An unexpected error occured. Aborting."


let solve (memory: int array) instructionPtr result =
    bruteForceSolve memory instructionPtr result 0 0