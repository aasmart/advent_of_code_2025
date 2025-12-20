open Core

let read_lines filname = In_channel.read_lines filname
let lights_regex = Re2.create_exn "(\\[[.#]+\\])"
let toggle_regex = Re2.create_exn "(\\([0-9,]*\\))"

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

let filename = "input.txt" in
part_1 filename
