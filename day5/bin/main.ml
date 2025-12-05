open Core

module Interval = struct
  type t = int * int [@@deriving sexp]

  (* let compare (lhs_start, lhs_end) (rhs_start, rhs_end) : int = *)
  (*   if rhs_start <= lhs_start && rhs_end >= lhs_end *)
  (*   then 0 *)
  (*   else if lhs_start > rhs_end *)
  (*   then -1 *)
  (*   else 1 *)
  (* ;; *)
end

(* module IntervalSet = Set.Make (Interval) *)
type interval_list = Interval.t list

let rec make_interval_list (lines : String.t list) (interval_ls : interval_list)
  : String.t list * interval_list
  =
  match lines with
  | [] -> raise (Failure "")
  | line :: tl ->
    if String.is_empty line
    then tl, interval_ls
    else (
      let dash_pos = String.index line '-' in
      match dash_pos with
      | Some pos ->
        let interval_start : int =
          String.sub line ~pos:0 ~len:pos |> int_of_string
        in
        let interval_end : int =
          String.sub line ~pos:(pos + 1) ~len:(String.length line - (pos + 1))
          |> int_of_string
        in
        let interval : Interval.t = interval_start, interval_end in
        interval :: interval_ls |> make_interval_list tl
      | None -> raise (Failure ""))
;;

let count_valid_ids ((lines, interval_map) : String.t list * interval_list)
  : int
  =
  let res =
    List.fold lines ~init:0 ~f:(fun (count : int) (line : String.t) ->
      let id = line |> int_of_string in
      (match
         List.find interval_map ~f:(fun (start, nd) -> id >= start && id <= nd)
       with
       | Some _ -> 1
       | None -> 0)
      + count)
  in
  res
;;

let read_input filename =
  let lines = In_channel.read_lines filename in
  lines
;;

make_interval_list (read_input "input.txt") []
|> count_valid_ids
|> string_of_int
|> print_endline
