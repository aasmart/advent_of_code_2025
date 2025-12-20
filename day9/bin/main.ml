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

let all_rectangle_bounds points =
  let rec all_rectangle_bounds points' bounds =
    match points' with
    | [] -> bounds
    | (hd_x, hd_y) :: tl ->
      all_rectangle_bounds
        tl
        (List.fold_left
           tl
           ~init:bounds
           ~f:(fun curr_bounds (other_x, other_y) ->
             ( (min hd_x other_x, max hd_x other_x)
             , (min hd_y other_y, max hd_y other_y) )
             :: curr_bounds))
  in
  all_rectangle_bounds points []
;;

(* this doesn't work for the general case, but certainly works for the shape of the given input*)
(* basically what this does is take the bounds of rectangles, and then check to see if the
   edge crosses through that rectangle*)
let filter_valid_rectangle_bounds points bounds =
  let first_point = Option.value_exn (List.hd points) in
  let filter_bounds (x_1, y_1) (x_2, y_2) bounds' =
    let edge_min_x, edge_max_x = min x_1 x_2, max x_1 x_2 in
    let edge_min_y, edge_max_y = min y_1 y_2, max y_1 y_2 in
    let is_edge_horizontal = y_1 = y_2 in
    let filtered_bounds' =
      List.filter bounds' ~f:(fun ((lower_x, upper_x), (lower_y, upper_y)) ->
        not
          (if is_edge_horizontal
           then
             y_1 > lower_y
             && y_1 < upper_y
             && edge_min_x < upper_x
             && edge_max_x > lower_x
           else
             x_1 > lower_x
             && x_1 < upper_x
             && edge_min_y < upper_y
             && edge_max_y > lower_y))
    in
    filtered_bounds'
  in
  let rec iter_edges points' bounds' =
    match points' with
    | [] -> failwith "invalid"
    | hd :: [] -> filter_bounds hd first_point bounds'
    | point_a :: point_b :: tl ->
      let filtered_bounds' = filter_bounds point_a point_b bounds' in
      iter_edges (point_b :: tl) filtered_bounds'
  in
  iter_edges points bounds
;;

let read_lines = In_channel.read_lines;;

let filename = "input.txt" in
let points = points_of_lines (read_lines filename) in
biggest_rectange points |> string_of_int |> print_endline;
(* part 2 *)
all_rectangle_bounds points
|> filter_valid_rectangle_bounds points
|> List.fold_left ~init:0 ~f:(fun curr_max ((x_1, x_2), (y_1, y_2)) ->
  max curr_max ((x_2 + 1 - x_1) * (y_2 - y_1 + 1)))
|> string_of_int
|> print_endline
