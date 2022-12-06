open System

let split (x: string) (s: string) = s.Split(x, StringSplitOptions.RemoveEmptyEntries)

let [| stacksDef; instructionsDef |] = fsi.CommandLineArgs[1] |> split "\n\n"
let stackLines = stacksDef |> split "\n"
let stackCount = Array.last stackLines |> split " " |> Seq.length
let stacks =
    Array.init stackCount (fun i ->
        stackLines[..stackLines.Length - 1]
        |> Seq.map (fun s -> s[4 * i + 1])
        |> Seq.filter Char.IsAsciiLetter
        |> Seq.toList)

let asOpt = function true, x -> Some x | _ -> None

let instructions =
    instructionsDef
    |> split "\n"
    |> Array.map (split " " >> Array.choose (Int32.TryParse >> asOpt))

let interpret (stacks: _[]) [| count; src; dest |] =
    for _ in 1..count do
        let x::s = stacks[src - 1]
        stacks[src - 1] <- s
        stacks[dest - 1] <- x::stacks[dest - 1]
    stacks

instructions
|> Array.fold interpret (Array.copy stacks)
|> Array.choose List.tryHead
|> String
|> printfn "part 1: %s"

// part 2

let interpret2 (stacks: _[]) [| count; src; dest |] =
    let buf, s = List.splitAt count stacks[src - 1]
    let d = buf @ stacks[dest - 1]
    stacks[src - 1] <- s
    stacks[dest - 1] <- d
    stacks

instructions
|> Array.fold interpret2 stacks
|> Array.choose List.tryHead
|> String
|> printfn "part 2: %s"