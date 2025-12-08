open Core

(* Here lies my first mistake... *)
(* let table = *)
(*   List.map boxes ~f:(fun line -> *)
(*     let split_str = String.split line ~on:',' in *)
(*     match split_str with *)
(*     | [ x; y; z ] -> *)
(*       let pos = x |> int_of_string, y |> int_of_string, z |> int_of_string in *)
(*       { pos; visited = false; distance = None } *)
(*     | _ -> failwith "invalid coordinate format") *)
(* in *)
(* let updated_table = *)
(*   List.fold_left (List.range 0 1000) ~init:table ~f:(fun table _ -> *)
(*     let closest_index, closest_entry = *)
(*       List.fold_left *)
(*         table *)
(*         ~init:(0, { pos = 0, 0, 0; visited = false; distance = None }) *)
(*         ~f:(fun (i, curr) entry -> *)
(*           if *)
(*             (not (Option.is_some curr.distance)) *)
(*             || (Option.is_some entry.distance *)
(*                 && (not entry.visited) *)
(*                 && Float.compare *)
(*                      (Option.value_exn curr.distance) *)
(*                      (Option.value_exn entry.distance) *)
(*                    = -1) *)
(*           then i + 1, entry *)
(*           else i + 1, curr) *)
(*     in *)
(*     if not (Option.is_some closest_entry.distance) then failwith "rip"; *)
(*     let updated_entry = { closest_entry with visited = true } in *)
(*     let table : table_entry list = *)
(*       List.mapi table ~f:(fun i entry -> *)
(*         if i = closest_index *)
(*         then updated_entry *)
(*         else ( *)
(*           let dist = distance updated_entry.pos entry.pos in *)
(*           if *)
(*             Option.is_none entry.distance *)
(*             || Float.compare dist (Option.value_exn entry.distance) = -1 *)
(*           then { entry with distance = Some dist } *)
(*           else entry)) *)
(*     in *)
(*     table) *)
(* in *)
(* () *)
(**)

(* type pos = int * int * int *)

(* type table_entry = *)
(*   { pos : pos *)
(*   ; distance : float option *)
(*   } *)

let distance (pos_a_x, pos_a_y, pos_a_z) (pos_b_x, pos_b_y, pos_b_z) =
  let delta_x = pos_a_x - pos_b_x in
  let delta_y = pos_a_y - pos_b_y in
  let delta_z = pos_a_z - pos_b_z in
  sqrt
    ((delta_x * delta_x) + (delta_y * delta_y) + (delta_z * delta_z)
     |> float_of_int)
;;

let all_point_pairs points =
  let rec all_points i points results =
    match points with
    | [] -> results
    | hd :: tl ->
      all_points
        (i + 1)
        tl
        (List.mapi tl ~f:(fun j p -> i, j + i + 1, distance p hd) @ results)
  in
  all_points 0 points []
;;

module UnionFind = struct
  type elt = int * int
  type t = elt array

  let create n : t = Array.init n ~f:(fun i -> 1, i)

  let rec find (reps : t) (x : int) =
    let size, rep = reps.(x) in
    if rep = x
    then size, x
    else (
      let _, rep = reps.(x) in
      let size, rep = find reps rep in
      reps.(x) <- size, rep;
      size, rep)
  ;;

  let union (reps : t) (x : int) (y : int) =
    let x_len, x_rep = find reps x in
    let y_len, y_rep = find reps y in
    if x_rep <> y_rep
    then (
      reps.(x_rep) <- x_len, y_rep;
      reps.(y_rep) <- x_len + y_len, y_rep)
  ;;
end

let connect_boxes_pt1 boxes =
  let points =
    List.map boxes ~f:(fun line ->
      let split_str = String.split line ~on:',' in
      match split_str with
      | [ x; y; z ] ->
        x |> int_of_string, y |> int_of_string, z |> int_of_string
      | _ -> failwith "invalid coordinate format")
  in
  let edges = all_point_pairs points in
  let sorted_edges =
    List.sort edges ~compare:(fun (_, _, lhs_d) (_, _, rhs_d) ->
      Float.compare lhs_d rhs_d)
  in
  let uf = UnionFind.create (List.length points) in
  List.iter (List.take sorted_edges 1000) ~f:(fun (x, y, _) ->
    UnionFind.union uf x y);
  let filtered_uf = Array.filteri uf ~f:(fun i (_, rep) -> i = rep) in
  Array.sort filtered_uf ~compare:(fun (lhs_len, _) (rhs_len, _) ->
    Int.compare rhs_len lhs_len);
  List.take (Array.to_list filtered_uf) 3
  |> List.fold_left ~init:1 ~f:(fun curr (len, _) -> curr * len)
  |> string_of_int
  |> print_endline
;;

let connect_boxes_pt2 boxes =
  let points =
    List.map boxes ~f:(fun line ->
      let split_str = String.split line ~on:',' in
      match split_str with
      | [ x; y; z ] ->
        x |> int_of_string, y |> int_of_string, z |> int_of_string
      | _ -> failwith "invalid coordinate format")
  in
  let edges = all_point_pairs points in
  let sorted_edges =
    List.sort edges ~compare:(fun (_, _, lhs_d) (_, _, rhs_d) ->
      Float.compare lhs_d rhs_d)
  in
  let num_points = List.length points in
  let uf = UnionFind.create (List.length points) in
  let rec connect_all edges =
    match edges with
    | [] -> failwith "what even happened"
    | (x, y, _) :: tl ->
      UnionFind.union uf x y;
      let len, _ = UnionFind.find uf x in
      if len = num_points
      then (
        match List.nth points x, List.nth points y with
        | Some (lhs_x, _, _), Some (rhs_x, _, _) -> lhs_x * rhs_x
        | _ -> failwith "invalid connection")
      else connect_all tl
  in
  connect_all sorted_edges |> string_of_int |> print_endline
;;

let read_input filename =
  let lines = In_channel.read_lines filename in
  lines
;;

let filename = "input.txt" in
connect_boxes_pt1 (read_input filename);
connect_boxes_pt2 (read_input filename)
