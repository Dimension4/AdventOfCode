let split (x: char) (s: string) = s.Split(x, System.StringSplitOptions.RemoveEmptyEntries)

let (|Digit|_|) (c: char) = if c >= '0' && c <= '9' then Some (int c - int '0') else None

type Packet =
    | Value of int
    | List of Packet list

let parsePacket (src: string) =
    let rec parse idx =
        match src[idx] with
        | '[' -> parseList (idx + 1) []
        | _ -> parseValue idx 0

    and parseList idx items =
        match src[idx] with
        | ']' -> (idx + 1, List.rev items |> List)
        | ',' -> parseList (idx + 1) items
        | _ ->
            let idx, item = parse idx
            parseList idx (item::items)

    and parseValue idx value =
        match src[idx] with
        | Digit x -> parseValue (idx + 1) (value * 10 + x)
        | _ -> (idx, Value value)

    parse 0 |> snd

let rec cmpPackets a b =
    match a, b with
    | Value x, Value y -> compare x y
    | Value x, y -> cmpPackets (List [Value x]) y
    | x, Value y -> cmpPackets x (List [Value y])
    | List x, List y ->
        let rec cmp a b =
            match a, b with
            | x::xs, y::ys ->
                match cmpPackets x y with
                | 0 -> cmp xs ys
                | o -> o
            | [], [] -> 0
            | [], _ -> -1
            | _ -> 1
        cmp x y

let arrayToTuple [| x; y |] = x, y
let (|>>) a b x = a x ||> b

let packets =
    fsi.CommandLineArgs[1]
    |> split '\n'
    |> Array.map parsePacket

packets
|> Array.chunkBySize 2
|> Array.map arrayToTuple
|> Seq.indexed
|> Seq.map (fun (x, y) -> (x + 1), y)
|> Seq.filter (snd |>> cmpPackets >> (<>) 1)
|> Seq.map fst
|> Seq.sum
|> printfn "part 1: %d"

// part 2

let divider1 = List [List [Value 2]]
let divider2 = List [List [Value 6]]

let ordered =
    packets
    |> Array.append [| divider1; divider2 |]
    |> Array.sortWith cmpPackets

(Array.findIndex ((=) divider1) ordered + 1,
Array.findIndex ((=) divider2) ordered + 1)
||> (*)
|> printfn "part 2: %d"
