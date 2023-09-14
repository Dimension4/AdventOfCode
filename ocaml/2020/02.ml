let read_file path =
  let channel = open_in path in
  let rec loop acc =
    try
      let line = input_line channel in
      loop (line :: acc)
    with End_of_file ->
      close_in channel;
      List.rev acc
  in
    loop []

type password =
  {
    min: int;
    max: int;
    ch: char;
    text: string;
  }

let validate_pw pw =
  let count = String.fold_left (fun t c -> if c = pw.ch then t + 1 else t) 0 pw.text in
  pw.min <= count && count <= pw.max

let validate_pw2 pw =
  ((pw.text.[pw.min - 1] = pw.ch) <> (pw.text.[pw.max - 1] = pw.ch))

let process validate part =
    read_file "02.txt"
    |> List.map (fun line -> Scanf.sscanf line "%d-%d %c: %s" (fun a b c d -> { min = a; max = b; ch = c; text = d}))
    |> List.filter validate
    |> List.length
    |> Printf.printf "part %d: %d\n" part

let input =
  process validate_pw 1;
  process validate_pw2 2