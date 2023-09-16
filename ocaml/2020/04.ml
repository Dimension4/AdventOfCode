module StringMap = Map.Make(String)

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

let get_fields lines =
  let rec join lines acc res =
    match lines, acc with
    | ""::tl, _ -> join tl "" (acc::res)
    | hd::tl, "" -> join tl hd res
    | hd::tl, _ -> join tl (acc ^ " " ^ hd) res
    | [], "" -> res
    | [], _ -> acc::res
  in
    join lines "" []
    |> List.map (
      String.trim
      >> String.split_on_char ' '
      >> List.map (String.split_on_char ':' >> (function [a; b] -> a, b | _ -> failwith "unreachable"))
      >> List.to_seq
      >> StringMap.of_seq)


let validation_map =
  let id x = x in
  let mk_tuple a b = a, b in
  let in_range a b c = a <= c && c <= b in
  [
    "byr", int_of_string >> in_range 1920 2002;
    "iyr", int_of_string >> in_range 2010 2020;
    "eyr", int_of_string >> in_range 2020 2030;
    "hgt", (fun x ->
      match Scanf.sscanf x "%d%s" mk_tuple with
      | num, "in" -> in_range 59 76 num
      | num, "cm" -> in_range 150 193 num
      | _ -> false);
    "hcl", (fun x -> Scanf.sscanf_opt x "#%x" id |> Option.is_some);
    "ecl", (fun x -> List.mem x ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]);
    "pid", String.length >> (=) 9;
    (* "cid"; *)
  ]
  |> List.to_seq
  |> StringMap.of_seq

let is_valid1 pp =
  validation_map
  |> StringMap.bindings
  |> List.for_all (fun (x, _) -> StringMap.find_opt x pp |> Option.is_some)

let is_valid2 pp =
  validation_map
  |> StringMap.bindings
  |> List.for_all (fun (x, f) ->
      match StringMap.find_opt x pp with
      | Some x when f x -> true
      | _ -> false)

let process validation part =
  read_file "04.txt"
  |> get_fields
  |> List.filter validation
  |> List.length
  |> Printf.printf "part %d: %d\n" part

let () =
  process is_valid1 1;
  process is_valid2 2