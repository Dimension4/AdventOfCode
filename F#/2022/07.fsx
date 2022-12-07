open System

type File = {
    Name: string
    Size: int
}

type Dir = {
    Parent: Dir option
    Name: string
    Files: ResizeArray<File>
    Dirs: ResizeArray<Dir>
}

let split (x: char) (s: string) = s.Split(x)

let makeDir cwd name =
    match Seq.tryFind (fun d -> d.Name = name) cwd.Dirs with
    | Some d -> d
    | None ->
        cwd.Dirs.Add { Parent = Some cwd; Name = name; Files = ResizeArray<_>(); Dirs = ResizeArray<_>() }
        cwd.Dirs[cwd.Dirs.Count - 1]

let (|Int|_|) (s: string) =
    match Int32.TryParse s with
    | true, x -> Some x
    | _ -> None

let rec fillDir dir = function
    | line::rest ->
        match split ' ' line with
        | [| "dir"; name |] ->
            makeDir dir name |> ignore
            fillDir dir rest
        | [| Int size; name |] ->
            dir.Files.Add {Name = name; Size = size }
            fillDir dir rest
        | _ -> line::rest, dir
    | [] -> [], dir

let rec findRoot = function
    | { Parent = Some x } -> findRoot x
    | x -> x

let rec parseCmd lines cwd =
    match lines with
    | line::rest ->
        match split ' ' line with
        | [| "$"; "cd"; "/" |] -> findRoot cwd |> parseCmd rest
        | [| "$"; "cd"; ".." |] -> cwd.Parent |> Option.get |> parseCmd rest
        | [| "$"; "cd"; dir |] -> dir |> makeDir cwd |> parseCmd rest
        | [| "$"; "ls" |] -> fillDir cwd rest ||> parseCmd
        | _ -> failwith "unrecognized command"
    | [] -> findRoot cwd

type DirInfo = {
    Name: string
    Size: int
    Children: DirInfo list
}

let rec getSize dir =
    let children = dir.Dirs |> Seq.map getSize |> Seq.toList
    let dirSize = children |> List.sumBy (fun x -> x.Size)
    let fileSize = dir.Files |> Seq.sumBy (fun x -> x.Size)
    { Name = dir.Name; Size = dirSize + fileSize; Children = children }

let rec iterInfo info = seq {
    yield info
    for c in info.Children do
        yield! iterInfo c
}

let fs =
    fsi.CommandLineArgs[1]
    |> split '\n'
    |> Array.toList
    |> parseCmd <| { Parent = None; Name = ""; Files = ResizeArray<_>(); Dirs = ResizeArray<_>() }
    |> getSize

iterInfo fs
|> Seq.map (fun x -> x.Size)
|> Seq.filter ((>=) 100_000)
|> Seq.sum
|> printfn "part 1: %d"

// part 2

let missingSize = 30_000_000 - (70_000_000 - fs.Size)

iterInfo fs
|> Seq.map (fun x -> x.Size)
|> Seq.filter ((<=) missingSize)
|> Seq.min
|> printfn "part 2: %d"
