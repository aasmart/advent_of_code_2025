open Core

type pos = int * int

let point_of_line line : pos =
  let split = String.split line ~on:',' in
  ( Option.value_exn (List.nth split 0) |> int_of_string
  , Option.value_exn (List.nth split 1) |> int_of_string )
;;

let points_of_lines = List.map ~f:point_of_line

let biggest_rectange points =
  let rec biggest_rectange points' largest_rec =
    match points' with
    | [] -> largest_rec
    | (hd_x, hd_y) :: tl ->
      biggest_rectange
        tl
        (List.fold_left
           tl
           ~init:largest_rec
           ~f:(fun curr_largest (other_x, other_y) ->
             max
               curr_largest
               (abs (hd_x - other_x + 1) * abs (hd_y - other_y + 1))))
  in
  biggest_rectange points 0
;;

(* let all_rectangle_bounds points = *)
(*   let rec all_rectangle_bounds points' bounds = *)
(*     match points' with *)
(*     | [] -> bounds *)
(*     | (hd_x, hd_y) :: tl -> *)
(*       all_rectangle_bounds *)
(*         tl *)
(*         (List.fold_left *)
(*            tl *)
(*            ~init:bounds *)
(*            ~f:(fun curr_bounds (other_x, other_y) -> *)
(*              ( (min hd_x other_x, max hd_x other_x) *)
(*              , (min hd_y other_y, max hd_y other_y) ) *)
(*              :: curr_bounds)) *)
(*   in *)
(*   all_rectangle_bounds points [] *)
(* ;; *)
(**)
(* (* this doesn't work for the general case, but certainly works for the shape of the given input*) *)
(* (* basically what this does is take the bounds of rectangles, and then check to see if the *)
(*    edge crosses through that rectangle*) *)
(* let filter_valid_rectangle_bounds points bounds = *)
(*   let first_point = Option.value_exn (List.hd points) in *)
(*   let filter_bounds (x_1, y_1) (x_2, y_2) bounds' = *)
(*     let edge_min_x, edge_max_x = min x_1 x_2, max x_1 x_2 in *)
(*     let edge_min_y, edge_max_y = min y_1 y_2, max y_1 y_2 in *)
(*     let is_edge_horizontal = y_1 = y_2 in *)
(*     let filtered_bounds' = *)
(*       List.filter bounds' ~f:(fun ((lower_x, upper_x), (lower_y, upper_y)) -> *)
(*         not *)
(*           (if is_edge_horizontal *)
(*            then *)
(*              y_1 > lower_y *)
(*              && y_1 < upper_y *)
(*              && edge_min_x < upper_x *)
(*              && edge_max_x > lower_x *)
(*            else *)
(*              x_1 > lower_x *)
(*              && x_1 < upper_x *)
(*              && edge_min_y < upper_y *)
(*              && edge_max_y > lower_y)) *)
(*     in *)
(*     filtered_bounds' *)
(*   in *)
(*   let rec iter_edges points' bounds' = *)
(*     match points' with *)
(*     | [] -> failwith "invalid" *)
(*     | hd :: [] -> filter_bounds hd first_point bounds' *)
(*     | point_a :: point_b :: tl -> *)
(*       let filtered_bounds' = filter_bounds point_a point_b bounds' in *)
(*       iter_edges (point_b :: tl) filtered_bounds' *)
(*   in *)
(*   iter_edges points bounds *)
(* ;; *)

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

(* alternate part 2 *)
let compress points =
  let row_set, col_set =
    List.fold_left
      points
      ~init:(IntSet.empty, IntSet.empty)
      ~f:(fun (row_set, col_set) (x, y) ->
        let row_map' = Set.add row_set y in
        let col_set' = Set.add col_set x in
        row_map', col_set')
  in
  let col_to_compressed_col =
    List.mapi (Set.to_list col_set) ~f:(fun i col -> col, i + 1)
    |> IntMap.of_alist_exn
  in
  let row_to_compressed_row =
    List.mapi (Set.to_list row_set) ~f:(fun i row -> row, i + 1)
    |> IntMap.of_alist_exn
  in
  let grid =
    Array.init
      (Set.length row_set + 2)
      ~f:(fun _ -> Array.create 'x' ~len:(Map.length col_to_compressed_col + 2))
  in
  (* compress the points according to our mapping *)
  let compressed_points =
    List.map points ~f:(fun (x, y) ->
      Map.find_exn col_to_compressed_col x, Map.find_exn row_to_compressed_row y)
  in
  (* walks along an edge and replaces it with # *)
  let fill_edge (p1_x, p1_y) (p2_x, p2_y) =
    let min_x, max_x = min p1_x p2_x, max p1_x p2_x in
    let min_y, max_y = min p1_y p2_y, max p1_y p2_y in
    let rec walk_edge (from_x, from_y) (to_x, to_y) =
      grid.(from_y).(from_x) <- '#';
      if from_x < to_x
      then walk_edge (from_x + 1, from_y) (to_x, to_y)
      else if from_y < to_y
      then walk_edge (from_x, from_y + 1) (to_x, to_y)
    in
    walk_edge (min_x, min_y) (max_x, max_y)
  in
  (* loop over adjacent points and fill in the edge between them *)
  let first_point = Option.value_exn (List.hd compressed_points) in
  let rec fill_all_edges points' =
    match points' with
    | [] -> failwith "invalid"
    | hd :: [] -> fill_edge hd first_point
    | point_a :: point_b :: tl ->
      fill_edge point_a point_b;
      fill_all_edges (point_b :: tl)
  in
  fill_all_edges compressed_points;
  List.zip_exn compressed_points points, grid
;;

let flood_fill grid =
  let rec flood_fill (px, py) =
    if
      Char.compare grid.(py).(px) '#' = 0 || Char.compare grid.(py).(px) '.' = 0
    then ()
    else (
      grid.(py).(px) <- '.';
      if px + 1 < Array.length grid.(0) then flood_fill (px + 1, py);
      if py + 1 < Array.length grid.(0) then flood_fill (px, py + 1);
      if px - 1 > 0 then flood_fill (px - 1, py);
      if py - 1 > 0 then flood_fill (px, py - 1) else ())
  in
  flood_fill (0, 0)
;;

let biggest_rectange_pt2 grid points =
  (* an edge is in the shape if all points along that edge are an x or # *)
  let is_edge_in_shape (p1_x, p1_y) (p2_x, p2_y) =
    let min_x, max_x = min p1_x p2_x, max p1_x p2_x in
    let min_y, max_y = min p1_y p2_y, max p1_y p2_y in
    let rec walk_edge (from_x, from_y) (to_x, to_y) =
      if
        Char.compare grid.(from_y).(from_x) 'x' <> 0
        && Char.compare grid.(from_y).(from_x) '#' <> 0
      then false
      else if from_x <= to_x
      then walk_edge (from_x + 1, from_y) (to_x, to_y)
      else if from_y <= to_y
      then walk_edge (from_y, from_y + 1) (to_x, to_y)
      else true
    in
    walk_edge (min_x, min_y) (max_x, max_y)
  in
  let rec biggest_rectange points' largest_rec =
    match points' with
    | [] -> largest_rec
    | ((compressed_hd_x, compressed_hd_y), (hd_x, hd_y)) :: tl ->
      biggest_rectange
        tl
        (List.fold_left
           tl
           ~init:largest_rec
           ~f:
             (fun
               curr_largest
               ((compressed_other_x, compressed_other_y), (other_x, other_y))
             ->
             if
               is_edge_in_shape
                 (compressed_hd_x, compressed_hd_y)
                 (compressed_other_x, compressed_hd_y)
               && is_edge_in_shape
                    (compressed_hd_x, compressed_other_y)
                    (compressed_other_x, compressed_other_y)
               && is_edge_in_shape
                    (compressed_hd_x, compressed_hd_y)
                    (compressed_hd_x, compressed_other_y)
               && is_edge_in_shape
                    (compressed_other_x, compressed_hd_y)
                    (compressed_other_x, compressed_other_y)
             then
               max
                 curr_largest
                 ((abs (hd_x - other_x) + 1) * (abs (hd_y - other_y) + 1))
             else curr_largest))
  in
  biggest_rectange points 0
;;

let read_lines = In_channel.read_lines;;

let filename = "input.txt" in
let points = points_of_lines (read_lines filename) in
biggest_rectange points |> string_of_int |> print_endline;
(* part 2 *)
(* all_rectangle_bounds points *)
(* |> filter_valid_rectangle_bounds points *)
(* |> List.fold_left ~init:0 ~f:(fun curr_max ((x_1, x_2), (y_1, y_2)) -> *)
(*   max curr_max ((x_2 + 1 - x_1) * (y_2 - y_1 + 1))) *)
(* |> string_of_int *)
(* |> print_endline; *)
let points', grid = compress points in
flood_fill grid;
(* Array.iter grid ~f:(fun row -> *)
(*   Array.iter row ~f:(fun c -> String.of_char c |> print_string); *)
(*   print_endline ""); *)
biggest_rectange_pt2 grid points' |> string_of_int |> print_endline
