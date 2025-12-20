open Core

let filename = "input.txt"
let lines = In_channel.read_lines filename;;

(* this is so dumb*)
List.fold lines ~init:0 ~f:(fun acc line ->
  let split = String.split line ~on:':' in
  match split with
  | [ dimensions; shape_counts ] ->
    let dim_split = String.split dimensions ~on:'x' in
    let x, y =
      match dim_split with
      | [ x; y ] -> x |> int_of_string, y |> int_of_string
      | _ -> failwith "invalid dimension"
    in
    let shape_counts_ls =
      shape_counts
      |> String.strip
      |> String.split ~on:' '
      |> List.map ~f:int_of_string
    in
    let res =
      if
        List.reduce_exn shape_counts_ls ~f:(fun acc v -> acc + (v * 9)) <= x * y
      then 1
      else 0
    in
    acc + res
  | _ -> failwith "invalid line")
|> string_of_int
|> print_endline
