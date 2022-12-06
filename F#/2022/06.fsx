let findMarker (markerLength: int) =
    Seq.windowed markerLength
    >> Seq.takeWhile (Array.distinct >> Array.length >> ((<>) markerLength))
    >> Seq.length
    >> (+) markerLength

fsi.CommandLineArgs[1]
|> findMarker 4
|> printfn "prat 1: %d"

// part 2

fsi.CommandLineArgs[1]
|> findMarker 14
|> printfn "prat 2: %d"