type Valve = { Pressure: int; Connected: string[]; Open : bool }
type Edge = { From: string; To: string; Cost: int}

let valves =
    let split (x: string[]) (s: string) = s.Split(x, System.StringSplitOptions.RemoveEmptyEntries)

    fsi.CommandLineArgs[1]
    |> split [| "\n" |]
    |> Array.map (
        split [| " "; ","; "="; ";" |]
        >> (fun arr -> arr[1], { Pressure = int arr[5]; Connected = arr[10..]; Open = false }))
    |> Map.ofArray

let findMaxPressureRelease valves =
    let update current = Map.change current (Option.map (fun v -> { v with Open = true }))
    // let update current next =
    //     Map.change current (function
    //         | Some v ->
    //             match Array.findIndex ((=) next) v.Connected |> Array.removeAt <| v.Connected with
    //             | [||] -> None
    //             | xs -> Some {v with Open = true }
    //         | None -> None)
    let rec walk (valves: Map<_,_>) name remaining path pressure =
        if remaining < 2 then List.rev path, pressure else
        let v = valves[name]
        let time = if v.Open then 1 else 2
        let p = if v.Open then 0 else v.Pressure * remaining
        v.Connected
        |> Array.map (fun n -> walk (update name valves) n (remaining - time) (n::path) (pressure + p))
        |> Array.maxBy snd
    walk valves "AA" 30 [] 0

let find2 valves =
    let relevant =
        valves
        |> Map.filter (fun _ x -> x.Pressure <> 0)
        |> Map.keys
        |> Seq.insertAt 0 "AA"
        |> Seq.toList
    let rec shortestPath (valves: Map<_,_>) src dst cost maxCost =
        if cost >= maxCost then maxCost
        elif src = dst then cost
        else
            let current = valves[src]
            let valves = Map.remove src valves
            current.Connected
            |> Array.filter (fun x -> Map.containsKey x valves)
            |> Array.fold (fun limit  name -> shortestPath valves name dst (cost + 1) limit) maxCost
    let rec createEdges edges = function
        | src::srcs ->
            let rec iter edges = function
                | dst::dsts ->
                    let cost = shortestPath valves src dst 0 100
                    iter ((src, dst, cost)::edges) dsts
                | [] -> edges
            createEdges (iter edges srcs) srcs
        | [] -> edges
    let edges = createEdges [] relevant
    let rec findPath remaining cost = function
        | name::rest ->
            let valve = valves[name]
        let iterEdges = function
            | (src, dst, cst)::tl ->



findMaxPressureRelease valves
||> printfn "part 1: %A %d"
