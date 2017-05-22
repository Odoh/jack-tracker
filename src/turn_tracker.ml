open Core.Std

(* Algorithm:
 * Store enough information each turn to build Jack's route as needed.
 * For each turn, store:
 * - Jack's potential locations
 * - detective locations
 * - Jack action performed on entering this state
 * 
 *   Example:
 *     turn 1 = { jack_action_1, jack_loc_1, det_loc_A }
 *     turn 2 = { jack_action_2, jack_loc_2, det_loc_A }
 *     
 *     Forward in time:
 *     { jack_loc_1, det_loc_A } -> apply jack_action_2 =
 *     { jack_loc_2, det_loc_A }
 *     
 *     Backward in time:
 *     { jack_loc_2, det_loc_A } -> apply jack_action_2 =
 *     { jack_loc_1, det_loc_A }
 *
 * This allows for recreating Jack's path and encoding all of Jack's moves;
 * however, this is not able to encode detective filtering actions (sniff hits/misses).
 * To encode sniff hits and misses using the algorithm above, the follow approach is taken.
 * 
 * Sniff Misses:
 * Sniff misses can be thought of as nodes that "no longer exist" on the Whitechapel board for that turn.
 * A structure called a Miss_board stores the true Whitechapel board and all current sniff misses.
 * The Miss_board is then used instead of the true Whitechapel board by the algorithm above;
 * missed nodes are then filtered out as they "no longer exist" according to the board.
 *
 *  Example:
 *    mboard.adj_to_white_node =  { 1 : [2; 3; 4] }
 *    sniff miss at 3
 *    mboard.adj_to_white_node =  { 1 : [2; 4] }
 *
 * Sniff Hits:
 * Sniff hits filter the algorithm above by rebuilding Jack's route sourced at the hit node and comparing
 * the resulting route to the original route. Multiple sniff hits are recursively applied upon each other.
 *)
let max_turns = 100

module Miss_board : sig 
  type t

  val with_board : Board.t -> t

  val sniff_miss : t -> int -> int -> unit

  val arrest_miss : t -> int -> int -> unit

  val adj_to_white_node : t -> int -> int -> int list

  val block_white_adj_nodes : t -> int -> int -> int list

  val white_path_black_node_paths : t -> int * int -> int list list

  val clone : t -> t
end =
struct
  type t = {
    misses: Int.Set.t array;
    board: Board.t;
  }

  let with_board board : t =
    { misses = Array.init (max_turns + 1) ~f:(fun _ -> Int.Set.empty);
      board = board; }

  let sniff_miss mboard turn node : unit =
    for t = 0 to turn do
      mboard.misses.(t) <- Int.Set.add mboard.misses.(t) node
    done

  let arrest_miss mboard turn node : unit =
    mboard.misses.(turn) <- Int.Set.add mboard.misses.(turn) node

  let filter_misses mboard turn nodes : int list =
    nodes |> List.filter ~f:(fun node ->
      not (Int.Set.exists mboard.misses.(turn) ~f:(fun miss -> miss = node)))

  let adj_to_white_node mboard turn node : int list =
    Board.adj_to_white_node mboard.board node
    |> filter_misses mboard turn

  let block_white_adj_nodes mboard turn node : int list =
    Board.block_white_adj_nodes mboard.board node
    |> filter_misses mboard turn

  let white_path_black_node_paths mboard white_path : int list list =
    Board.white_path_black_node_paths mboard.board white_path

  let clone mboard : t =
    { mboard with
      misses = Array.copy mboard.misses }
end

module Jack_action = struct
  type t =
    | None
    | JackWalk
    | JackCarOne
    | JackCarTwo
    | JackAlley

  let to_string t : string =
    match t with
    | None -> "None"
    | JackWalk -> "JackWalk"
    | JackCarOne -> "JackCarOne"
    | JackCarTwo -> "JackCarTwo"
    | JackAlley -> "JackAlley"

  let walk_dsts mboard turn srcs ~det_locs : int list =
    (* find all adj spaces *)
    srcs
    |> List.concat_map ~f:(fun src ->
      let dsts = Miss_board.adj_to_white_node mboard turn src in
      List.map dsts ~f:(fun dst -> src :: [dst]))
    (* remove all paths which contain paths which passed through a detective *)
    |> List.filter ~f:(fun path ->
      let src, dst = List.hd_exn path, List.last_exn path in
      let black_paths = Miss_board.white_path_black_node_paths mboard (src, dst) in
      not (List.for_all black_paths ~f:(fun black_path ->
        List.exists black_path ~f:(fun black_node ->
          List.exists det_locs ~f:(fun det_loc -> det_loc = black_node))))) 
    |> List.map ~f:(fun path -> List.last_exn path)
    |> List.dedup

  let car_dsts ?one_dsts mboard turn1 turn2 srcs : (int list) * (int list) =
    (* find all adj spaces *)
    let paths = srcs
                |> List.concat_map ~f:(fun src ->
                  let dsts = Miss_board.adj_to_white_node mboard turn1 src in
                  List.map dsts ~f:(fun dst -> src :: [dst]))

                (* add all adj spaces, not equal to the first space *)
                |> List.concat_map ~f:(fun path ->
                  let fst = List.hd_exn path in
                  let src = List.last_exn path in
                  let dsts = Miss_board.adj_to_white_node mboard turn2 src
                             |> List.filter ~f:(fun dst -> dst <> fst) in
                  List.map dsts ~f:(fun dst -> path @ [dst])) in

    (* if provided, filter paths by the required one_dsts and/or two_dsts *)
    let paths = match one_dsts with
      | Some one_dsts ->
        List.filter paths ~f:(fun path ->
          let path_one_dst = List.nth_exn path 1 in
          List.exists one_dsts ~f:(fun one_dst -> one_dst = path_one_dst))
      | None -> paths in

    (* return the dsts for the first and second part of the car move *)
    let one_dsts = paths
                   |> List.map ~f:(fun path -> List.nth_exn path 1)
                   |> List.dedup in
    let two_dsts = paths
                   |> List.map ~f:(fun path -> List.last_exn path)
                   |> List.dedup in
    one_dsts, two_dsts

  let alley_dsts mboard turn srcs : int list =
    (* find all adj spaces through an alley *)
    srcs
    |> List.concat_map ~f:(fun src -> 
      let dsts = Miss_board.block_white_adj_nodes mboard turn src in
      List.map dsts ~f:(fun dst -> dst))
    |> List.dedup

  let dsts ?one_dsts ?turn2 action mboard turn srcs ~det_locs : int list =
    let turn2 = match turn2 with
      | Some turn2 -> turn2
      | None -> max_turns in
    match action with
    | JackWalk -> walk_dsts mboard turn srcs ~det_locs:det_locs
    | JackAlley -> alley_dsts mboard turn srcs
    | JackCarOne ->
      let one, _ = match one_dsts with
        | Some one_dsts -> car_dsts mboard turn turn2 srcs ~one_dsts:one_dsts 
        | None -> car_dsts mboard turn turn2 srcs in one
    | JackCarTwo -> 
      let _, two = match one_dsts with
        | Some one_dsts -> car_dsts mboard turn turn2 srcs ~one_dsts:one_dsts 
        | None -> car_dsts mboard turn turn2 srcs in two
    | None -> srcs
end

module One_tracker : sig
  type t

  exception Invalid_Action of string * int

  val use_board : Board.t -> t
  val clone : t -> t

  val root_node : t -> int
  val curr_turn : t -> int
  val jack_locs : t -> int -> int list
  val curr_jack_locs : t -> int list
  val curr_det_locs : t -> int list

  (* == Detective Actions == *)
  val det_set_locs : t -> int list -> unit
  val det_sniff_hit : t -> int -> unit
  val det_sniff_miss : t -> int -> unit
  val det_arrest_miss : t -> int -> unit

  (* == Jack Actions == *)
  val jack_start_loc : t -> int -> unit
  val jack_walk : t -> unit
  val jack_car : t -> unit
  val jack_alley : t -> unit

  (* == Actions == *)
  (* Perform an action on the tracker. *)
  val perform_action : t -> Tracker.Action.t -> unit 
  val print : t -> unit
end =
struct 
  exception Invalid_Action of string * int

  type turn = 
    { mutable jack_action : Jack_action.t;
      mutable jack_locs : int list;
      mutable det_locs : int list;
    }

  type t = 
    { mboard : Miss_board.t;
      turns : turn array;
      mutable sniff_hits : (int * int) list;
      mutable curr_turn : int; }

  let create_empty_turn _ =
    { jack_action = Jack_action.None;
      jack_locs = [];
      det_locs = []; }

  (* Init the jack tracker using the board with the specified filename. *)
  let use_board board : t =
    { mboard = Miss_board.with_board board;
      curr_turn = 0;
      sniff_hits = [];
      turns = Array.init max_turns ~f:create_empty_turn }

  let clone tracker : t =
    { mboard = Miss_board.clone tracker.mboard;
      turns = Array.map tracker.turns ~f:(fun t -> { jack_action = t.jack_action;
                                                     jack_locs = t.jack_locs;
                                                     det_locs = t.det_locs; });
      sniff_hits = List.map tracker.sniff_hits ~f:(fun e -> e);
      curr_turn = tracker.curr_turn }

  let tracker_with_jack_loc tracker jack_loc_turn jack_loc_node : t =
    let turns = Array.init max_turns ~f:create_empty_turn in
    for turn = 0 to tracker.curr_turn do
      turns.(turn).jack_action <- tracker.turns.(turn).jack_action;
      turns.(turn).jack_locs <- [];
      turns.(turn).det_locs <- tracker.turns.(turn).det_locs;
    done;
    turns.(jack_loc_turn).jack_locs <- [jack_loc_node];
    { tracker with
      turns = turns }

  let tracker_combine trackers : t =
    let tracker = List.hd_exn trackers in
    let turns = Array.init max_turns ~f:create_empty_turn in
    for turn = 0 to tracker.curr_turn do
      let jack_locs = trackers
                      |> List.concat_map ~f:(fun tracker -> tracker.turns.(turn).jack_locs)
                      |> List.dedup in
      turns.(turn).jack_action <- tracker.turns.(turn).jack_action;
      turns.(turn).jack_locs <- jack_locs;
      turns.(turn).det_locs <- tracker.turns.(turn).det_locs;
    done;
    { tracker with
      turns = turns }

  exception Break

  let tracker_missing_jack_locs tracker : bool =
    let missing = ref false in
    try
      for turn = 0 to tracker.curr_turn do
        if List.is_empty tracker.turns.(turn).jack_locs then (
          missing := true;
          raise Break
        )
      done;
      !missing
    with
    | Break -> !missing

  (* == Query Tracker == *)

  let root_node tracker : int =
    let root_jack_locs = tracker.turns.(0).jack_locs in
    if List.is_empty root_jack_locs then -1
    else List.hd_exn root_jack_locs

  (* Return the current turn int the game. *)
  let curr_turn tracker : int =
    tracker.curr_turn

  (* Return the jack action taken before the specified turn. *)
  let jack_action tracker turn : Jack_action.t =
    tracker.turns.(turn).jack_action

  (* Return potential locations for jack at the specified turn. *)
  let jack_locs tracker turn : int list =
    tracker.turns.(turn).jack_locs

  (* Return the detective locations on a specified turn. *)
  let det_locs tracker turn : int list =
    tracker.turns.(turn).det_locs

  (* Return the jack action taken before the current turn. *)
  let curr_jack_action tracker : Jack_action.t =
    jack_action tracker (curr_turn tracker)

  (* Return potential locations for jack on the current turn. *)
  let curr_jack_locs tracker : int list =
    jack_locs tracker (curr_turn tracker)

  (* Return the detective locations on the current turn. *)
  let curr_det_locs tracker : int list =
    det_locs tracker (curr_turn tracker)

  (* == Jack Actions == *)

  (* Set jack's starting location. *)
  let jack_start_loc tracker (start_index : int) : unit =
    tracker.turns.(0) <- { jack_action = Jack_action.None;
                           jack_locs = [start_index];
                           det_locs = []; };
    tracker.sniff_hits <- [start_index, 0]

  (* Record jacks walk move. *)
  let jack_walk tracker : unit =
    let curr_turn = curr_turn tracker in
    let curr_jack_locs = curr_jack_locs tracker in
    let curr_det_locs = curr_det_locs tracker in
    let dsts = Jack_action.walk_dsts tracker.mboard (curr_turn + 1) curr_jack_locs ~det_locs:curr_det_locs in
    let next_turn = curr_turn + 1 in
    tracker.turns.(next_turn).jack_action <- Jack_action.JackWalk;
    tracker.turns.(next_turn).jack_locs <- dsts;
    tracker.turns.(next_turn).det_locs <- curr_det_locs;
    tracker.curr_turn <- next_turn;
    if tracker_missing_jack_locs tracker then
      raise (Invalid_Action ("Invalid walk", (root_node tracker)))

  (* Record jacks car move. *)
  let jack_car tracker : unit =
    let curr_turn = curr_turn tracker in
    let curr_jack_locs = curr_jack_locs tracker in
    let curr_det_locs = curr_det_locs tracker in
    let one_dsts, two_dsts = Jack_action.car_dsts tracker.mboard (curr_turn + 1) (curr_turn + 2) curr_jack_locs in
    (* car move one *)
    let one_turn = curr_turn + 1 in
    tracker.turns.(one_turn).jack_action <- Jack_action.JackCarOne;
    tracker.turns.(one_turn).jack_locs <- one_dsts;
    tracker.turns.(one_turn).det_locs <- curr_det_locs;
    tracker.curr_turn <- one_turn;
    (* car move two *)
    let two_turn = curr_turn + 2 in
    tracker.turns.(two_turn).jack_action <- Jack_action.JackCarTwo;
    tracker.turns.(two_turn).jack_locs <- two_dsts;
    tracker.turns.(two_turn).det_locs <- curr_det_locs;
    tracker.curr_turn <- two_turn

  (* Record jacks alley move. *)
  let jack_alley tracker : unit =
    let curr_turn = curr_turn tracker in
    let curr_jack_locs = curr_jack_locs tracker in
    let curr_det_locs = curr_det_locs tracker in
    let dsts = Jack_action.alley_dsts tracker.mboard (curr_turn + 1) curr_jack_locs in
    let next_turn = curr_turn + 1 in
    tracker.turns.(next_turn).jack_action <- Jack_action.JackAlley;
    tracker.turns.(next_turn).jack_locs <- dsts;
    tracker.turns.(next_turn).det_locs <- curr_det_locs;
    tracker.curr_turn <- next_turn

  (* == Detective Actions == *)

  (* Print the current state of the tracker to stdout. *)
  let print tracker : unit =
    for turn = 0 to curr_turn tracker do
      let jack_action = jack_action tracker turn in
      print_int turn;
      print_string " ";
      print_string (Jack_action.to_string jack_action);
      print_newline ();

      let jack_locs = jack_locs tracker turn in
      print_string "  [";
      List.iteri jack_locs ~f:(fun i loc ->
        print_int loc;
        if (i + 1) < List.length jack_locs then
          print_string ", "
      );
      print_endline "]";

      let det_locs = det_locs tracker turn in
      print_string "  {";
      List.iteri det_locs ~f:(fun i loc ->
        print_int loc;
        if (i + 1) < List.length det_locs then
          print_string ", "
      );
      print_endline "}"
    done 

  let filter_if_in_list nodes in_nodes =
    nodes
    |> List.filter ~f:(fun node ->
      List.exists in_nodes ~f:(fun in_node -> node = in_node))

  let in_each_list nodes1 nodes2 =
    let all_nodes = [nodes1; nodes2]
                    |> List.concat
                    |> List.dedup in
    let nodes = ref [] in
    List.iter all_nodes ~f:(fun node ->
      if (List.exists nodes1 ~f:(fun n -> node = n)) &&
         (List.exists nodes2 ~f:(fun n -> node = n)) then
        nodes := node :: !nodes);
    !nodes

  let rec propagate_hit_up tracker hit_tracker turn : unit =
    if turn < 0 then () else (
      let jack_action = jack_action hit_tracker (turn + 1) in
      match jack_action with
      | Jack_action.JackCarOne ->
        let one_dsts = jack_locs hit_tracker (turn + 1) in
        let srcs = filter_if_in_list (Jack_action.dsts Jack_action.JackCarOne hit_tracker.mboard turn one_dsts ~det_locs:[])
                     (jack_locs tracker turn) in
        let two_dsts = filter_if_in_list (Jack_action.dsts Jack_action.JackCarTwo hit_tracker.mboard (turn + 1) ~turn2:(turn + 2) srcs ~one_dsts:one_dsts ~det_locs:[])
                         (jack_locs tracker (turn + 2)) in
        hit_tracker.turns.(turn).jack_locs <- srcs;
        hit_tracker.turns.(turn + 2).jack_locs <- two_dsts;
        propagate_hit_up tracker hit_tracker (turn - 1)

      | Jack_action.JackCarTwo ->
        let two_dsts = jack_locs hit_tracker (turn + 1) in
        let one_dsts, srcs = Jack_action.car_dsts hit_tracker.mboard turn (turn - 1) two_dsts in
        let one_dsts = filter_if_in_list (one_dsts)
                         (jack_locs tracker turn) in
        let srcs = filter_if_in_list (srcs)
                     (jack_locs tracker (turn - 1)) in
        hit_tracker.turns.(turn).jack_locs <- one_dsts;
        hit_tracker.turns.(turn - 1).jack_locs <- srcs;
        propagate_hit_up tracker hit_tracker (turn - 2)

      | _ ->
        let srcs = jack_locs hit_tracker (turn + 1) in
        let det_locs = det_locs hit_tracker turn in
        let dsts = filter_if_in_list (Jack_action.dsts jack_action hit_tracker.mboard turn srcs ~det_locs:det_locs)
                     (jack_locs tracker turn) in
        hit_tracker.turns.(turn).jack_locs <- dsts;
        propagate_hit_up tracker hit_tracker (turn - 1)
    )

  let rec propagate_hit_down tracker hit_tracker curr_turn turn : unit =
    if turn > curr_turn then () else (
      let jack_action = jack_action hit_tracker turn in
      match jack_action with
      | Jack_action.JackCarOne ->
        let srcs = jack_locs hit_tracker (turn - 1) in
        let one_dsts, two_dsts = Jack_action.car_dsts hit_tracker.mboard turn (turn + 1) srcs in
        let one_dsts = filter_if_in_list (one_dsts)
                         (jack_locs tracker turn) in
        let two_dsts = filter_if_in_list (two_dsts)
                         (jack_locs tracker (turn + 1)) in
        hit_tracker.turns.(turn).jack_locs <- one_dsts;
        hit_tracker.turns.(turn + 1).jack_locs <- two_dsts;
        propagate_hit_down tracker hit_tracker curr_turn (turn + 2)

      | Jack_action.JackCarTwo ->
        let one_dsts = jack_locs hit_tracker (turn - 1) in
        let srcs = filter_if_in_list (Jack_action.dsts Jack_action.JackCarOne hit_tracker.mboard (turn - 2) one_dsts ~det_locs:[])
                     (jack_locs tracker (turn - 2)) in
        let two_dsts = filter_if_in_list (Jack_action.dsts Jack_action.JackCarTwo hit_tracker.mboard (turn - 1) ~turn2:turn srcs ~one_dsts:one_dsts ~det_locs:[])
                         (jack_locs tracker turn) in
        hit_tracker.turns.(turn).jack_locs <- two_dsts;
        hit_tracker.turns.(turn - 2).jack_locs <- srcs;
        propagate_hit_down tracker hit_tracker curr_turn (turn + 1)

      | _ ->
        let srcs = jack_locs hit_tracker (turn - 1) in
        let det_locs = det_locs hit_tracker (turn - 1) in
        let dsts = filter_if_in_list (Jack_action.dsts jack_action hit_tracker.mboard turn srcs ~det_locs:det_locs)
                     (jack_locs tracker turn) in
        hit_tracker.turns.(turn).jack_locs <- dsts;
        propagate_hit_down tracker hit_tracker curr_turn (turn + 1)
    )

  let rec rebuild_with_sniff_hits_recur tracker sniff_hits : unit =
    if List.is_empty sniff_hits then () else (
      let curr_turn = curr_turn tracker in
      let sniff_hit, sniff_turn = List.hd_exn sniff_hits in

      (* iterate over each turn propagating the hit up and down for each found hit node *)
      let sniff_hit_trackers = ref [] in
      for turn = sniff_turn downto 0 do
        let turn_jack_locs = jack_locs tracker turn in
        if List.exists turn_jack_locs ~f:(fun loc -> loc = sniff_hit) then (
          (* clone a new tracker with only the hit node *)
          let hit_tracker = tracker_with_jack_loc tracker turn sniff_hit in

          (* propagate the sniff hit up and down *)
          let jack_action = jack_action tracker turn in
          (match jack_action with
           | Jack_action.JackCarOne ->
             propagate_hit_up tracker hit_tracker (turn - 1);
             propagate_hit_down tracker hit_tracker curr_turn (turn + 2)
           | Jack_action.JackCarTwo ->
             propagate_hit_up tracker hit_tracker (turn - 1);
             propagate_hit_down tracker hit_tracker curr_turn (turn + 1)
           | _ ->
             propagate_hit_up tracker hit_tracker (turn - 1);
             propagate_hit_down tracker hit_tracker curr_turn (turn + 1));

          (* if missing a node for any turn, don't include the sniff_hit tracker *)
          if not (tracker_missing_jack_locs hit_tracker) then
            sniff_hit_trackers := hit_tracker :: !sniff_hit_trackers;
        )
      done;

      (* apply subsequent sniff hits *)
      List.iter !sniff_hit_trackers ~f:(fun sniff_hit_tracker ->
        rebuild_with_sniff_hits_recur sniff_hit_tracker (List.tl_exn sniff_hits));

      (* update original tracker.
       * an empty list means the path was not possible: clear it as an option *)
      if (List.is_empty !sniff_hit_trackers) then
        for turn = 0 to curr_turn do
          tracker.turns.(turn).jack_locs <- []
        done
      else
        let combined_tracker = tracker_combine !sniff_hit_trackers in
        for turn = 0 to curr_turn do
          tracker.turns.(turn).jack_locs <- combined_tracker.turns.(turn).jack_locs
        done
    )

  let rebuild_with_sniff_hits tracker : unit =
    rebuild_with_sniff_hits_recur tracker tracker.sniff_hits

  (* Set the detectives locations. *)
  let det_set_locs tracker (locs : int list) : unit =
    tracker.turns.(curr_turn tracker).det_locs <- locs

  (* Record a detective arrest that missed jack. *)
  let det_arrest_miss tracker node : unit =
    let curr_turn = curr_turn tracker in
    let curr_jack_locs = curr_jack_locs tracker in
    Miss_board.arrest_miss tracker.mboard curr_turn node;
    let filtered_jack_locs = List.filter curr_jack_locs ~f:(fun jack_loc -> jack_loc <> node) in
    if List.is_empty filtered_jack_locs then
      raise (Invalid_Action ("Invalid arrest miss", (root_node tracker)))
    else
      tracker.turns.(curr_turn).jack_locs <- filtered_jack_locs;
    rebuild_with_sniff_hits tracker;
    if tracker_missing_jack_locs tracker then
      raise (Invalid_Action ("Invalid arrest miss", (root_node tracker)))

  (* Record a detective sniff that missed jack. *)
  let det_sniff_miss tracker node : unit =
    if List.exists tracker.sniff_hits ~f:(fun (hit, turn) -> hit = node) then
      raise (Invalid_Action ("Invalid sniff miss", (root_node tracker)));

    let curr_turn = curr_turn tracker in
    Miss_board.sniff_miss tracker.mboard curr_turn node;
    for turn = curr_turn downto 0 do
      let jack_locs = jack_locs tracker turn in
      let filtered_jack_locs = List.filter jack_locs ~f:(fun jack_loc -> jack_loc <> node) in
      if List.is_empty filtered_jack_locs then
        raise (Invalid_Action ("Invalid sniff miss", (root_node tracker)))
      else
        tracker.turns.(turn).jack_locs <- filtered_jack_locs;
    done;
    rebuild_with_sniff_hits tracker;
    if tracker_missing_jack_locs tracker then
      raise (Invalid_Action ("Invalid sniff miss", (root_node tracker)))

  (* Record a detective sniff that located jack. *)
  let det_sniff_hit tracker node : unit =
    let curr_turn = curr_turn tracker in
    if List.exists tracker.sniff_hits ~f:(fun (n, t) ->
        node = n && t = curr_turn) 
    then ()
    else (
      tracker.sniff_hits <- ((node, curr_turn) :: tracker.sniff_hits);
      rebuild_with_sniff_hits tracker;
      if tracker_missing_jack_locs tracker then
        raise (Invalid_Action ("Invalid sniff hit", (root_node tracker)))
    )

  (* == Actions == *)

  (* Perform an action on the tracker. *)
  let perform_action tracker action : unit =
    match action with
    | Tracker.Action.DetSniffHit n -> det_sniff_hit tracker n
    | Tracker.Action.DetSniffMiss n -> det_sniff_miss tracker n
    | Tracker.Action.DetArrestMiss n -> det_arrest_miss tracker n
    | Tracker.Action.JackWalk -> jack_walk tracker
    | Tracker.Action.JackCar -> jack_car tracker
    | Tracker.Action.JackAlley -> jack_alley tracker
    | Tracker.Action.DetLocChange ns -> det_set_locs tracker ns
end


type t = 
  { mutable trackers: One_tracker.t list }

exception Invalid_Action of string

(* Init the jack tracker using board. *)
let use_board board : t =
  { trackers = [One_tracker.use_board board;
                One_tracker.use_board board;] }

let clone t : t =
  { trackers = List.map t.trackers ~f:(fun tracker -> One_tracker.clone tracker) }

(* == Query Tracker == *)

let curr_turn t : int =
  let tracker = List.hd_exn t.trackers in
  One_tracker.curr_turn tracker

let jack_locs t turn : int list =
  t.trackers
  |> List.concat_map ~f:(fun tracker -> One_tracker.jack_locs tracker turn)
  |> List.dedup

(* Return the detective locations for the current turn. *)
let curr_det_locs t : int list =
  let tracker = List.hd_exn t.trackers in
  One_tracker.curr_det_locs tracker

(* Return potential locations for jack on the current turn. *)
let curr_jack_locs t : int list =
  t.trackers
  |> List.concat_map ~f:One_tracker.curr_jack_locs
  |> List.dedup

(* == Jack Actions == *)

(* Set jack's starting location. *)
let jack_start_locs t jack_starts : unit =
  (if List.length jack_starts = 1 then
     t.trackers <- (List.tl_exn t.trackers));
  List.zip_exn t.trackers jack_starts 
  |> List.iter ~f:(fun zip -> 
    let tracker, jack_start = zip in
    One_tracker.jack_start_loc tracker jack_start)

(* == Actions == *)

(* Perform an action on the tracker. *)
let perform_action t action : unit =
  let invalid_actions = ref [] in
  for i = 0 to (List.length t.trackers - 1) do
    let tracker = List.nth_exn t.trackers i in
    try
      One_tracker.perform_action tracker action
    with
    | One_tracker.Invalid_Action (str, root) -> 
      invalid_actions := (str, root) :: !invalid_actions
  done;

  match !invalid_actions with
  | [] -> ()
  | ex :: [] ->
    let str, root_node = ex in
    if List.length t.trackers > 1 then
      t.trackers <- List.filter t.trackers ~f:(fun tracker ->
        (One_tracker.root_node tracker) <> root_node)
    else raise (Tracker.Illegal_Action str)
  | _ -> 
    let str, _ = List.hd_exn !invalid_actions in
    raise (Tracker.Illegal_Action str)

(* Print the current state of the tracker to stdout. *)
let print t : unit =
  if List.length t.trackers = 1 then
    One_tracker.print (List.hd_exn t.trackers)
  else (
    print_endline "Two Trackers:";
    List.iter t.trackers ~f:One_tracker.print;
    print_endline "-------------";
  )

class turn_jack_tracker (board : Board.t) : Tracker.jack_tracker =
  object(self)
    val tracker = use_board board

    method print () = print tracker
    method curr_turn () = curr_turn tracker
    method jack_locs turn = jack_locs tracker turn
    method curr_jack_locs () = curr_jack_locs tracker
    method curr_det_locs () = curr_det_locs tracker
    method jack_start_locs locs = jack_start_locs tracker locs
    method perform_action action = perform_action tracker action
    method test_action action =
        let cloned_tracker = clone tracker in
        perform_action cloned_tracker action
  end

let create (board : Board.t) : Tracker.jack_tracker =
  new turn_jack_tracker board
