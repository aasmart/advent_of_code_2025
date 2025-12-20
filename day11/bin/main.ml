open Core

let read_lines = In_channel.read_lines

module StringHshtbl = Hashtbl.Make (String)

let make_adjlist lines =
  List.map lines ~f:(fun line ->
    let split_line = String.split ~on:':' line in
    match split_line with
    | [ vertex; edges ] ->
      let split_edges = String.strip edges |> String.split ~on:' ' in
      vertex, split_edges
    | _ -> failwith "invalid edge format")
  |> StringHshtbl.of_alist_exn
;;

let rec count_unqiue_paths adj_list ~from_node ~to_node =
  if String.equal from_node to_node
  then 1
  else (
    let edges = Hashtbl.find_exn adj_list from_node in
    List.fold_left edges ~init:0 ~f:(fun num_paths to_vertex ->
      num_paths + count_unqiue_paths adj_list ~from_node:to_vertex ~to_node))
;;

let filename = "input.txt" in
read_lines filename
|> make_adjlist
|> count_unqiue_paths ~from_node:"you" ~to_node:"out"
|> string_of_int
|> print_endline
