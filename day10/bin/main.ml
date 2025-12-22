open Core

let read_lines filname = In_channel.read_lines filname
let lights_regex = Re2.create_exn "(\\[[.#]+\\])"
let toggle_regex = Re2.create_exn "(\\([0-9,]*\\))"
let joltage_regex = Re2.create_exn "({[0-9,]*})"
let num_regex = Re2.create_exn "(\\d*)"

let gen_all_button_combinations target toggles =
  (* since pressing multiple buttons twice does nothing, our only option at each point is to press it once, or not at all*)
  (* so to generate all the combinations, we use the button, or we dont*)
  let rec all_combinations toggles current_state current_used_toggles =
    if Bitv.equal current_state target
    then [ current_used_toggles ]
    else (
      match toggles with
      | hd :: tl ->
        let use =
          all_combinations
            tl
            (Bitv.bw_xor current_state hd)
            (hd :: current_used_toggles)
        in
        let dont_use = all_combinations tl current_state current_used_toggles in
        List.append dont_use use
      | [] -> [])
  in
  all_combinations toggles (Bitv.create (Bitv.length target) false) []
;;

let part_1 filename =
  List.fold (read_lines filename) ~init:0 ~f:(fun total line ->
    let target =
      match Re2.find_submatches lights_regex line with
      | Ok subs ->
        let target_str = Option.value_exn subs.(0) in
        let target_str =
          String.sub target_str ~pos:1 ~len:(String.length target_str - 2)
        in
        let target_set = Bitv.create (String.length target_str) false in
        String.iteri target_str ~f:(fun i target ->
          Bitv.set target_set i (Char.compare target '#' = 0));
        target_set
      | Error _ -> failwith "invalid target"
    in
    let toggle_sets =
      match Re2.find_all toggle_regex line with
      | Ok toggles ->
        List.map toggles ~f:(fun toggle_str ->
          let toggle_set = Bitv.create (Bitv.length target) false in
          String.iter toggle_str ~f:(fun c ->
            match Char.get_digit c with
            | Some c -> Bitv.set toggle_set c true
            | None -> ());
          toggle_set)
      | Error _ -> failwith "no toggles"
    in
    total
    + List.fold_left
        (gen_all_button_combinations target toggle_sets)
        ~init:Int.max_value
        ~f:(fun m l -> min m (List.length l)))
  |> string_of_int
  |> print_endline
;;

let solve problem =
  match Lp_glpk.solve ~term_output:false problem with
  | Ok (_, xs) ->
    Lp.PMap.bindings xs
    |> List.fold ~init:0. ~f:(fun acc (_, res) -> acc +. res)
    |> int_of_float
  | Error msg -> failwith msg
;;

(* each button is basically a column in a matrix, and the target is the joltage values *)
(* the number of times we press the button is the scale factor of the column *)
let part_2 filename =
  List.fold (read_lines filename) ~init:0 ~f:(fun total line ->
    let extract_digit_list str =
      Re2.find_all_exn num_regex str
      |> List.filter ~f:(fun s -> String.is_empty s |> not)
      |> List.map ~f:int_of_string
    in
    let buttons =
      match Re2.find_all toggle_regex line with
      | Ok toggles -> List.map toggles ~f:extract_digit_list
      | Error _ -> failwith "no toggles"
    in
    let joltage_requirements =
      Option.value_exn (Re2.find_submatches_exn joltage_regex line).(0)
      |> extract_digit_list
    in
    (* create the problem we want to optimize *)
    let problem =
      let button_vars =
        List.init (List.length buttons) ~f:(fun i ->
          Lp.var
            ~integer:true
            ~lb:0.
            (String.of_char (Char.of_int_exn (i + 65))))
      in
      (* we want to minimize the sum of the number of times we press each button*)
      let objective =
        Lp.minimize
          (List.reduce_exn button_vars ~f:(fun curr_poly next ->
             Lp.( ++ ) curr_poly next))
      in
      (* build the polynomial portion of the constraints *)
      let constraint_polys =
        Array.create ~len:(List.length joltage_requirements) (Lp.c 0.)
      in
      List.iter2_exn buttons button_vars ~f:(fun set var ->
        List.iter set ~f:(fun counter ->
          constraint_polys.(counter) <- Lp.( ++ ) constraint_polys.(counter) var));
      (* add the joltage equivalence to each constraint *)
      let constraints =
        List.map2_exn
          (Array.to_list constraint_polys)
          joltage_requirements
          ~f:(fun poly joltage ->
            let c = Lp.( =~ ) poly (Lp.c (joltage |> float_of_int)) in
            c)
      in
      Lp.make objective constraints
    in
    total + solve problem)
;;

let filename = "input.txt" in
part_1 filename;
part_2 filename |> string_of_int |> print_endline
