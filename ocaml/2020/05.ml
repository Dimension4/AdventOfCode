module IntSet = Set.Make(Int)

let (||>) (a, b) f = f a b
let (>>) f g x = g (f x)
let (|>>) f g (x, y) = (f x y) ||> g

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

type partition = Lower | Upper

let rec apply_partition min max = function
  | [Upper] -> max
  | [Lower] -> min
  | Lower::tl -> apply_partition min ((max + min) / 2) tl
  | Upper::tl -> apply_partition ((max + min + 1) / 2) max tl
  | [] -> failwith "missing partitions"

let seat_id row col = row * 8 + col

let parse_partition = String.to_seq >> Seq.map (function 'F' | 'L' -> Lower | _ -> Upper) >> List.of_seq

let input =
  read_file "05.txt"
  |> List.map (fun s ->
      String.sub s 0 7 |> parse_partition,
      String.sub s 7 3 |> parse_partition)

let find_seat row col = apply_partition 0 127 row, apply_partition 0 7 col

let () =
  let seats =
    input
    |> List.map (find_seat |>> seat_id)
    |> IntSet.of_list
  in
    IntSet.fold max seats 0
    |> Printf.printf "part 1: %d\n";

  let all_seats =
    List.init 128 (fun row -> List.init 8 (fun col -> seat_id row col))
    |> List.concat
    |> IntSet.of_list
  in
    IntSet.diff all_seats seats
    |> IntSet.find_first (fun i -> IntSet.mem (i - 1) seats && IntSet.mem (i + 1) seats)
    |> Printf.printf "part 2: %d\n"

