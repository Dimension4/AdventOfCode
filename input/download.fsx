open System
open System.IO
open System.Net.Http
open System.Text.RegularExpressions
open System.Threading.Tasks
open System.Xml

let sessionId = fsi.CommandLineArgs[1]
let year = Array.tryItem 2 fsi.CommandLineArgs |> Option.map int |> Option.defaultValue DateTime.Now.Year
let dest = Array.tryItem 3 fsi.CommandLineArgs |> Option.defaultValue (Path.Join(__SOURCE_DIRECTORY__, string year))

let client = new HttpClient()
client.DefaultRequestHeaders.Add("Cookie", $"session={sessionId}")

[|
    task {
        for i in 1..24 do
            try
                let fileName = Path.Combine(dest, $"%02d{i}.txt")
                if File.Exists fileName then
                    printfn $"'{fileName}' already exists, skipping."
                else
                    let! response = client.GetStringAsync $"https://adventofcode.com/{year}/day/{i}/input"
                    do! File.WriteAllTextAsync(fileName, response)
                    printfn $"Saved '{fileName}'"
            with e -> eprintfn $"Failed to fetch input for day {i} ({e.Message})."
    }
    task {
        for i in 1..24 do
            try
                let fileName = Path.Combine(dest, $"%02d{i}-desc.md")
                let needsFetch = 
                    if File.Exists fileName then
                        let content = File.ReadAllText fileName
                        content.Contains("--- Part Two ---", StringComparison.InvariantCultureIgnoreCase) |> not
                    else
                        true
                if needsFetch then
                    let! response = client.GetStringAsync $"https://adventofcode.com/{year}/day/{i}"
                    let text = Regex.Matches(response, "<article.+?>.+?</article>",RegexOptions.Singleline) |> Seq.map _.Value |> String.concat Environment.NewLine
                    do! File.WriteAllTextAsync(fileName, text)
                    printfn $"Saved '{fileName}'"
                else 
                    printfn $"'{fileName}' already exists, skipping."
            with e -> eprintfn $"Failed to fetch description for day {i} ({e.Message})."
    }
|]
|> Task.WhenAll
|> Async.AwaitTask
|> Async.RunSynchronously
