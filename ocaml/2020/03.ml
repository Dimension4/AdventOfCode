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

let wrapping_get s i = s.[abs i mod (String.length s)]

let count_trees dx dy board =
  let rec loop x y acc =
    if y >= Array.length board then acc else
    let tree = if Array.get board y x = '#' then 1 else 0 in
    loop (x + dx) (y + dy) (acc + tree)
  in
    loop dx dy 0

let () =
  let board =
    read_file "03.txt"
    |> List.map wrapping_get
    |> Array.of_list
  in
    count_trees 3 1 board |> Printf.printf "part 1: %d\n";
    [
      1, 1;
      3, 1;
      5, 1;
      7, 1;
      1, 2;
    ]
    |> List.map (fun (dx, dy) -> count_trees dx dy board)
    |> List.fold_left Int.mul 1
    |> Printf.printf "part 2: %d\n"