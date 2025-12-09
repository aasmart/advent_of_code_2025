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

let read_lines = In_channel.read_lines;;

let filename = "input.txt" in
points_of_lines (read_lines filename)
|> biggest_rectange
|> string_of_int
|> print_endline
