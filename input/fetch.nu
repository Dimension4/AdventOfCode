def main [sessionId: string, --year (-y): int, --out (-o): string] {
    let year = if ($year == null) { date now | date to-record | get year } else { $year }
    let out = if ($out == null) { $year | into string } else { $out }
    
    print $"Downloading puzzle inputs to '($out | path expand)'..."
    mkdir $out
    1..25 | each {
        let name = $in | into string | fill -a r -w 2 -c 0 | $in ++ ".txt"
        let path = [$out $name] | path join
        print $"Downloading ($path)...(ansi -e 1F)"
        http get $"https://adventofcode.com/($year)/day/($in)/input" --headers [Cookie $"session=($sessionId)"]
        | save $path
    } | ignore
}
