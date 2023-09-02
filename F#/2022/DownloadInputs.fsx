open System.IO
open System.Net.Http

let sessionId = fsi.CommandLineArgs[1]
let year = 2022

task {
    use client = new HttpClient()
    client.DefaultRequestHeaders.Add("Cookie", $"session={sessionId}")
    for i in 1..24 do
        try
            let fileName = Path.Combine(__SOURCE_DIRECTORY__, $"%02d{i}.txt")
            if File.Exists fileName then
                printfn $"'{fileName}' already exists, skipping."
            else
                let! response = client.GetStringAsync $"https://adventofcode.com/{year}/day/{i}/input"
                do! File.WriteAllTextAsync(fileName, response)
                printfn $"Saved '{fileName}'"
        with _ -> eprintfn $"Failed to fetch input for day {i}."
}
|> Async.AwaitTask
|> Async.RunSynchronously


