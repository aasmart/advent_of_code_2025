open Core

let read_lines = In_channel.read_lines

module StringHshtbl = Hashtbl.Make (String)
module StringSet = Hash_set.Make (String)

let make_adjlist lines =
  let adj_list =
    List.map lines ~f:(fun line ->
      let split_line = String.split ~on:':' line in
      match split_line with
      | [ vertex; edges ] ->
        let split_edges = String.strip edges |> String.split ~on:' ' in
        vertex, split_edges
      | _ -> failwith "invalid edge format")
    |> StringHshtbl.of_alist_exn
  in
  Hashtbl.add_exn adj_list ~key:"out" ~data:[];
  adj_list
;;

let rec count_unqiue_paths adj_list ~from_node ~to_node =
  if String.equal from_node to_node
  then 1
  else (
    let edges = Hashtbl.find_exn adj_list from_node in
    List.fold_left edges ~init:0 ~f:(fun num_paths to_vertex ->
      num_paths + count_unqiue_paths adj_list ~from_node:to_vertex ~to_node))
;;

let count_dac_fft_paths adj_list ~from_node ~to_node =
  let incoming_edges =
    List.zip_exn
      (Hashtbl.keys adj_list)
      (List.init (Hashtbl.length adj_list) ~f:(fun _ -> 0))
    |> StringHshtbl.of_alist_exn
  in
  Hashtbl.iter adj_list ~f:(fun edges ->
    List.iter edges ~f:(fun v ->
      Hashtbl.update incoming_edges v ~f:(fun v -> Option.value_exn v + 1)));
  let path_counts =
    List.zip_exn
      (Hashtbl.keys adj_list)
      (List.init (Hashtbl.length adj_list) ~f:(fun _ -> 0, 0, 0, 0))
    |> StringHshtbl.of_alist_exn
  in
  Hashtbl.update path_counts from_node ~f:(fun _ -> 1, 0, 0, 0);
  let rec traverse to_visit =
    match to_visit with
    | curr_node :: remaining_to_visit ->
      (* print_endline curr_node; *)
      let edges = Hashtbl.find_exn adj_list curr_node in
      let curr_node_paths = Hashtbl.find_exn path_counts curr_node in
      let to_visit' =
        List.fold_left
          edges
          ~init:remaining_to_visit
          ~f:(fun remaining_to_visit' edge ->
            Hashtbl.update path_counts edge ~f:(fun v ->
              let curr_none, curr_dac, curr_ffa, curr_both = curr_node_paths in
              let v_none, v_dac, v_ffa, v_both = Option.value_exn v in
              let is_dac = String.equal "dac" edge in
              let is_ffa = String.equal "fft" edge in
              (* merge the paths together *)
              (* the different node type just moves the path type between the states*)
              let none' =
                if is_dac || is_ffa then v_none else v_none + curr_none
              in
              let dac' =
                (if is_dac then curr_none + v_dac else v_dac)
                + if not is_ffa then curr_dac else 0
              in
              let ffa' =
                if is_ffa
                then curr_none + v_ffa
                else v_ffa + if not is_dac then curr_ffa else 0
              in
              let both' =
                (if is_ffa
                 then v_both + curr_dac
                 else if is_dac
                 then v_both + curr_ffa
                 else v_both)
                + curr_both
              in
              none', dac', ffa', both');
            let incoming_count =
              Hashtbl.update_and_return incoming_edges edge ~f:(fun v ->
                Option.value_exn v - 1)
            in
            if incoming_count <= 0
            then edge :: remaining_to_visit'
            else remaining_to_visit')
      in
      traverse to_visit'
    | [] ->
      let _, _, _, both = Hashtbl.find_exn path_counts to_node in
      both
  in
  traverse
    (Hashtbl.filter incoming_edges ~f:(fun count -> count = 0) |> Hashtbl.keys)
;;

let filename = "sample.txt" in
read_lines filename
|> make_adjlist
|> count_unqiue_paths ~from_node:"you" ~to_node:"out"
|> string_of_int
|> print_endline;
let filename = "input.txt" in
read_lines filename
|> make_adjlist
|> count_dac_fft_paths ~from_node:"svr" ~to_node:"out"
|> string_of_int
|> print_endline
