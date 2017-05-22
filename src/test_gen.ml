open Core.Std
open Yojson.Basic.Util

let random_node nodes : int =
  let i = Random.int (List.length nodes) in
  List.nth_exn nodes i

let random_loc locs : int list =
  let i = Random.int (List.length locs) in
  List.nth_exn locs i

let nodes_minus_single_jack_loc nodes jack_locs : int list =
  if (List.length jack_locs) = 1 then
    let jack_loc = List.hd_exn jack_locs in
    nodes |> List.filter ~f:(fun n -> n <> jack_loc)
  else
    nodes

let rec random_jack_starting_locs board jack_start_limit : int list =
  (* 1 node  = 75%
   * 2 nodes = 25% *)
  let white_nodes = Board.white_nodes board in
  if jack_start_limit < 2 then
    [random_node white_nodes]
  else
    let i = Random.int 100 in
    if i < 75 then
      [random_node white_nodes]
    else
      List.dedup [random_node white_nodes; random_node white_nodes]

let rec random_det_locs board : int list =
  (* 1 node  = 20%
   * 2 nodes = 20%
   * 3 nodes = 20%
   * 4 nodes = 20%
   * 5 nodes = 20% *) 
  let black_nodes = Board.black_nodes board in
  let i = Random.int 100 in
  if i < 20 then
    [random_node black_nodes]
  else if i < 40 then
    List.dedup [random_node black_nodes; random_node black_nodes]
  else if i < 60 then
    List.dedup [random_node black_nodes;
                random_node black_nodes;
                random_node black_nodes]
  else if i < 80 then
    List.dedup [random_node black_nodes;
                random_node black_nodes;
                random_node black_nodes;
                random_node black_nodes]
  else 
    List.dedup [random_node black_nodes;
                random_node black_nodes;
                random_node black_nodes;
                random_node black_nodes;
                random_node black_nodes]

let rec random_action board jack_tracker : Tracker.Action.t =
  (* DetSniffHit   = 15%
   * DetSniffMiss  = 25%
   * DetArrestMiss = 10%
   * JackWalk      = 25%
   * JackCar       = 10%
   * JackAlley     = 10%
   * DetLocChange  = 5% *)
  let i = Random.int 100 in
  if i < 15 then
    try
      let nodes = List.range 0 ((jack_tracker#curr_turn ()) + 1)
                  |> List.fold ~init:[] ~f:(fun acc turn -> 
                      (jack_tracker#jack_locs turn) :: acc)
                  |> List.concat
                  |> List.dedup in
      if List.is_empty nodes then random_action board jack_tracker
      else (
        let node = random_node nodes in
        jack_tracker#test_action (Tracker.Action.DetSniffHit node);
        Tracker.Action.DetSniffHit node
      )
    with
    | Tracker.Illegal_Action msg -> random_action board jack_tracker
  else if i < 40 then
    try
      let node = random_node (Board.white_nodes board) in
      jack_tracker#test_action (Tracker.Action.DetSniffMiss node);
      Tracker.Action.DetSniffMiss node
    with
    | Tracker.Illegal_Action msg -> random_action board jack_tracker
  else if i < 50 then
    try
      let nodes = nodes_minus_single_jack_loc 
          (Board.white_nodes board)
          (jack_tracker#curr_jack_locs ()) in
      if List.is_empty nodes then random_action board jack_tracker
      else (
        let node = random_node nodes in
        jack_tracker#test_action (Tracker.Action.DetArrestMiss node);
        Tracker.Action.DetArrestMiss node
      )
    with
    | Tracker.Illegal_Action msg -> random_action board jack_tracker
  else if i < 75 then
    try
        jack_tracker#test_action Tracker.Action.JackWalk;
        Tracker.Action.JackWalk
      with
      | Tracker.Illegal_Action msg -> random_action board jack_tracker
  else if i < 85 then Tracker.Action.JackCar
  else if i < 95 then Tracker.Action.JackAlley
  else Tracker.Action.DetLocChange (random_det_locs board)

let gen_tests board tracker_fn prefix ~num_tests ~num_actions ~jack_start_limit : unit =
  Random.self_init ();
  (* create a directory to save the tests *)
  let dirname = prefix ^ "_tests" in
  if not (Sys.file_exists_exn dirname) then Unix.mkdir dirname;

  for i = 1 to num_tests do
    let test_name = dirname ^ "/" ^ prefix ^ "_" ^ (Int.to_string i) in
    let jack_tracker = tracker_fn () in
    let night_tracker = Night_tracker.create jack_tracker in

    (* jack starting locations *)
    let jack_starts = random_jack_starting_locs board jack_start_limit in
    Night_tracker.set_jack_start_locs night_tracker jack_starts;

    (* det starting locations *)
    let det_starts_action = Tracker.Action.DetLocChange (random_det_locs board) in
    Night_tracker.perform_action night_tracker det_starts_action;

    (* execute jack actions *)
    List.range 0 num_actions
    |> List.iter ~f:(fun _ -> 
        let action = random_action board jack_tracker in
        Night_tracker.perform_action night_tracker action);

    Night_tracker.save_to_file night_tracker test_name
  done
