open Core.Std
exception Test_Failure of string

let rec sorted_list_equal l1 l2 =
  if List.length l1 <> List.length l2 then false
  else if List.is_empty l1 then true
  else
    match l1, l2 with
    | h1 :: l1, h2 :: l2 ->
      if h1 = h2 then sorted_list_equal l1 l2
      else false
    | _ -> false

let print_list l =
  print_string "[";
  List.iteri l ~f:(fun i n ->
      print_int n ;
      if (i + 1) < List.length l then
        print_string ", ");
  print_string "]"

let simulate_game tracker_fn jack_starts actions verbose : int list =
  let jack_tracker = tracker_fn () in

  jack_tracker#jack_start_locs jack_starts;
  if verbose then jack_tracker#print ();

  List.iter actions ~f:(fun action ->
      if verbose then print_endline (Tracker.Action.to_string action);
      jack_tracker#perform_action action ;
      if verbose then jack_tracker#print ());

  jack_tracker#curr_jack_locs ()

let run_test tracker_fn test_filename verbose : unit =
  let info = Night_tracker.save_file_info test_filename in
  print_endline ("Running " ^ test_filename ^ "...");
  let found_jack_locs = simulate_game 
      tracker_fn
      info.Night_tracker.jack_starts 
      info.Night_tracker.action_list verbose in

  let expected = List.sort info.Night_tracker.jack_locs ~cmp:Int.compare in
  let found = List.sort found_jack_locs ~cmp:Int.compare in
  if sorted_list_equal expected found then
    print_endline ("PASSED " ^ test_filename)
  else
    (print_endline ("FAILED " ^ test_filename);
     print_string "  exp: "; print_list expected; print_newline ();
     print_string "  fnd: "; print_list found; print_newline ();
     raise (Test_Failure test_filename))

(* Generate tests for a jack tracker. *)
let run_tests tracker_fn test_path ~verbose : unit =
  if Sys.is_directory_exn test_path then
    Sys.readdir test_path
    |> Array.map ~f:(fun file -> test_path ^ "/" ^ file)
    |> Array.iter ~f:(fun test_path -> run_test tracker_fn test_path verbose)
  else
    run_test tracker_fn test_path verbose
