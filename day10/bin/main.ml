open Core

let read_lines filname = In_channel.read_lines filname
let lights_regex = Re2.create_exn "(\\[[.#]+\\])"
let toggle_regex = Re2.create_exn "(\\([0-9,]*\\))"
let joltage_regex = Re2.create_exn "({[0-9,]*})"
let num_regex = Re2.create_exn "(\\d*)"

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
    (* perform a level-order search to reach the end state *)
    let rec find_min_toggles current_states move_count =
      let next_states, found_target =
        List.fold
          current_states
          ~init:([], false)
          ~f:(fun (next_states, found_target) current_state ->
            let next_states', found_target' =
              List.fold
                toggle_sets
                ~init:(next_states, found_target)
                ~f:(fun (next_states, found_target) toggle_set ->
                  let next_state = Bitv.bw_xor current_state toggle_set in
                  let found_target' =
                    Bitv.equal target next_state || found_target
                  in
                  next_state :: next_states, found_target')
            in
            ( List.dedup_and_sort next_states' ~compare:(fun a b ->
                Int.compare (Bitv.to_int_us a) (Bitv.to_int_us b))
            , found_target' ))
      in
      if found_target
      then move_count
      else find_min_toggles next_states move_count + 1
    in
    total + find_min_toggles [ Bitv.create (Bitv.length target) false ] 1)
  |> string_of_int
  |> print_endline
;;

let solve problem =
  (* For other interfaces, use Lp_glpk_js or Lp_gurobi instead *)
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
      let button_var =
        List.init (List.length buttons) ~f:(fun i ->
          Lp.var
            ~integer:true
            ~lb:0.
            (String.of_char (Char.of_int_exn (i + 65))))
      in
      (* we want to minimize the sum of the number of times we press each button*)
      let objective =
        Lp.minimize
          (List.reduce_exn button_var ~f:(fun curr_poly next ->
             Lp.( ++ ) curr_poly next))
      in
      (* build the polynomial portion of the constraints *)
      let constraint_polys =
        Array.create ~len:(List.length joltage_requirements) (Lp.c 0.)
      in
      List.iter2_exn buttons button_var ~f:(fun set var ->
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
(* in *)
(*     ; *)
;;

let filename = "input.txt" in
part_1 filename;
part_2 filename |> string_of_int |> print_endline
