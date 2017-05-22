open Core.Std

(* Algorithm:
 * Maintain a list of all possible paths for Jack's route:
 * - Jack's moves will add elements at the end of each path (and/or create new paths), based on the board.
 * - Detective actions will filter the path list
 *)

(* Path tracker.
 *   board      = the Whitechapel board
 *   jack_paths = list of all possible movement paths for Jack
 *   det_locs   = the current detective locations.
 *)
type t =
  { board : Board.t;
    jack_paths : int list list;
    det_locs : int list;
  }

(* Init the jack tracker using the board with the specified filename. *)
let use_board board : t =
  { board = board;
    jack_paths = [];
    det_locs = []; }

(* == Jack Actions == *)

(* Set jack's starting location. *)
let jack_start_locs tracker (start_indices : int list) : t =
  { tracker with
    jack_paths = List.map start_indices ~f:(fun index -> [index]) }

(* Record jacks walk move. *)
let jack_walk tracker : t =
  let new_paths =
    (* add all possible adj white spaces to existing paths *)
    tracker.jack_paths
    |> List.concat_map ~f:(fun path -> 
        let src = List.last_exn path in
        let adjs = Board.adj_to_white_node tracker.board src in
        List.map adjs ~f:(fun adj -> path @ [adj]))
    (* remove all paths which contain all paths which passed through a detective *)
    |> List.filter ~f:(fun path ->
        let src = List.nth_exn path ((List.length path) - 2) in
        let dst = List.last_exn path in
        let blk_paths = Board.white_path_black_node_paths tracker.board (src, dst) in
        not (List.for_all blk_paths ~f:(fun blk_path ->
            List.exists blk_path ~f:(fun blk ->
                List.exists tracker.det_locs ~f:(fun det_loc -> det_loc = blk))))) in
  if List.is_empty new_paths then raise (Tracker.Illegal_Action "Jack had an impossible walk") else
  { tracker with
    jack_paths = new_paths }

(* Record jacks car move. *)
let jack_car tracker : t =
  let new_paths =
    (* add all possible adj white spaces to existing paths *)
    tracker.jack_paths
    |> List.concat_map ~f:(fun path -> 
        let src = List.last_exn path in
        let adjs = Board.adj_to_white_node tracker.board src in
        List.map adjs ~f:(fun adj -> path @ [adj]))
    (* add all possible adj white spaces, not equal to the first move, to existing paths *)
    |> List.concat_map ~f:(fun path -> 
        let fst = List.nth_exn path ((List.length path) - 2) in
        let src = List.last_exn path in
        let adjs = Board.adj_to_white_node tracker.board src
                   |> List.filter ~f:(fun dst -> dst <> fst) in
        List.map adjs ~f:(fun adj -> path @ [adj])) in

  { tracker with
    jack_paths = new_paths }

(* Record jacks alley move. *)
let jack_alley tracker : t =
  let new_paths = 
    (* add all possible alley moves to existing paths. Don't include src position as a dst *)
    tracker.jack_paths
    |> List.concat_map ~f:(fun path ->
        let src = List.last_exn path in
        let dsts = Board.block_white_adj_nodes tracker.board src in
        List.map dsts ~f:(fun dst -> path @ [dst])) in

  { tracker with
    jack_paths = new_paths }

(* == Detective Actions == *)

(* Set the detectives locations. *)
let det_locs tracker (locs : int list) : t =
  { tracker with
    det_locs = locs }

(* Record a detective sniff that located jack. *)
let det_sniff_hit tracker index : t =
  let new_paths = List.filter tracker.jack_paths ~f:(fun path -> List.exists path ~f:(fun i -> i = index)) in
  if List.is_empty new_paths then raise (Tracker.Illegal_Action ("Detective had an impossible sniff hit at: " ^ (Int.to_string index))) else
  { tracker with
    jack_paths =  new_paths }

(* Record a detective sniff that missed jack. *)
let det_sniff_miss tracker index : t =
  let new_paths = List.filter tracker.jack_paths ~f:(fun path -> not (List.exists path ~f:(fun i -> i = index))) in
  if List.is_empty new_paths then raise (Tracker.Illegal_Action ("Detective had an impossible sniff miss at: " ^ (Int.to_string index))) else
  { tracker with
    jack_paths = new_paths }

(* Record a detective arrest that missed jack. *)
let det_arrest_miss tracker index : t =
  { tracker with
    jack_paths = List.filter tracker.jack_paths ~f:(fun path -> ((List.last_exn path) <> index)) }

(* == Query Tracker == *)

(* Return the current turn int the game. *)
let curr_turn tracker : int =
  (tracker.jack_paths
   |> List.hd_exn
   |> List.length) - 1

(* Return potential locations for jack at the specified turn. *)
let jack_locs tracker turn : int list =
  tracker.jack_paths
  |> List.map ~f:(fun path -> List.nth_exn path turn)
  |> List.dedup
  |> List.sort ~cmp:Int.compare

(* Return the current detective locations. *)
let curr_det_locs tracker : int list =
  tracker.det_locs

(* Return potential locations for jack on the current turn. *)
let curr_jack_locs tracker : int list =
  jack_locs tracker (curr_turn tracker)

(* Return potential locations for jack their percentages at the specified turn. *)
let jack_locs_pct tracker turn : float Int.Map.t =
  let total = Float.of_int (List.length tracker.jack_paths) in
  tracker.jack_paths
  |> List.map ~f:(fun path -> List.nth_exn path turn)
  |> List.sort ~cmp:Int.compare
  |> List.group ~break:(<>)
  |> List.map ~f:(fun grp -> List.hd_exn grp, Float.of_int (List.length grp) /. total)
  |> Int.Map.of_alist_exn

(* Return potential locations for jack their percentages on the current turn. *)
let curr_jack_locs_pct tracker : float Int.Map.t =
  jack_locs_pct tracker (curr_turn tracker)

(* Perform an action on the tracker. *)
let perform_action tracker action : t =
  match action with
  | Tracker.Action.DetSniffHit n -> det_sniff_hit tracker n
  | Tracker.Action.DetSniffMiss n -> det_sniff_miss tracker n
  | Tracker.Action.DetArrestMiss n -> det_arrest_miss tracker n
  | Tracker.Action.JackWalk -> jack_walk tracker
  | Tracker.Action.JackCar -> jack_car tracker
  | Tracker.Action.JackAlley -> jack_alley tracker
  | Tracker.Action.DetLocChange ns -> det_locs tracker ns

(* Print the current state of the tracker to stdout. *)
let print tracker : unit =
  let jack_locs = curr_jack_locs tracker in
  let det_locs = tracker.det_locs in
  print_string "[";
  List.iteri jack_locs ~f:(fun i loc ->
      print_int loc;
      if (i + 1) < List.length jack_locs then
        print_string ", "
    );
  print_endline "]";
  print_string "{";
  List.iteri det_locs ~f:(fun i loc ->
      print_int loc;
      if (i + 1) < List.length det_locs then
        print_string ", "
    );
  print_endline "}";
  print_endline "[";
  List.iter tracker.jack_paths ~f:(fun path ->
      print_string "  [";
      List.iter path ~f:(fun i -> print_int i; print_string " ";);
      print_endline "]");
  print_endline "]"

class path_jack_tracker (board : Board.t) : Tracker.jack_tracker =
  object(self)
    val mutable tracker = use_board board

    method print () = print tracker
    method curr_turn () = curr_turn tracker
    method jack_locs turn = jack_locs tracker turn
    method curr_jack_locs () = curr_jack_locs tracker
    method curr_det_locs () = curr_det_locs tracker
    method jack_start_locs locs = tracker <- jack_start_locs tracker locs
    method perform_action action = tracker <- perform_action tracker action
    method test_action action =
      perform_action tracker action |> ignore;
      ()
  end

let create (board : Board.t) : Tracker.jack_tracker =
  new path_jack_tracker board
