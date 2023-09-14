module IntSet = Set.Make(Int)

let (||>) (a, b) f = f a b
let (>>) f g x = g (f x)

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

let input =
  read_file "01.txt"
  |> List.map int_of_string
  |> IntSet.of_list

let find_sum target num_count set =
  let rec loop len nums remaining =
    match len, remaining with
    | _, 0 when len = num_count -> Some nums
    | _ when len = num_count -> None
    | _ ->
      let sub, _, _ = IntSet.split (remaining + 1) set in
      IntSet.to_seq sub
      |> Seq.filter_map (fun i -> loop (len + 1) (i::nums) (remaining - i))
      |> Seq.uncons
      |> Option.map fst
    in
      loop 0 [] target

let print_result part =
  Option.get
  >> List.fold_left Int.mul 1
  >> Printf.printf "part %d: %d\n" part

let () =
    find_sum 2020 2 input |> print_result 1;
    find_sum 2020 3 input |> print_result 2