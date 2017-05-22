open Core.Std

(* 
 * Jack tracker which stores jacks path in a condensed tree.
 * Implemented as a command line tool (CLT) in the 'whitechapel' submodule.
 *)

(* Spawn a process running cmd and return its output. *)
let run cmd : string list =
  let p_in = Core.Std.Unix.open_process_in cmd in
  let lines = In_channel.input_lines p_in in
  In_channel.close p_in;
  lines

(* Spawn a process running args and return its output. *)
let run args : string list =
    let argstr = args
                  |> List.map ~f:(fun arg -> arg ^ " ")
                  |> String.concat in
    run argstr

(* Print lines to stdout. *)
let print (lines : string list) : unit =
  if (List.length lines) = 1 then
    print_endline (List.hd_exn lines)
  else (
    print_endline "[";
    List.iteri lines ~f:(fun i line ->
      print_string "  ";
      print_endline line;
    );
    print_endline "]"
  )

(* Interaction with the Jack tracker process. *)
module Jack_process = struct
  let root_exec = "python3 whitechapel/jack_track.py"
  (* only use one game_name for the time being to prevent a flood of pickles
    * this means only one Tree_tracker can be used at a time *)
  let game = "treetrack"
  let game_file = game ^ "_paths.pickle"

  (* Run the Jack process sending the specified game, board, command and locs. *)
  let run_jack_locs ~game ~board cmd locs : string list =
    run ([
      root_exec;
      "-n"; game;
      "-m"; board;
      "-c"; cmd;
    ] @ (List.map locs ~f:Int.to_string))

  (* Run the Jack process with the specified game, board, and args. *)
  let run_jack ~game ~board cmd =
    run_jack_locs ~game:game ~board:board cmd []
end

type t = {
  (* the name of the CLT game.
   * the path of the board JSON to use. *)
  game : string;
  board : string;

  (* the CLT does not track the current turn or detective locations,
    * so maintain that state here. *)
  mutable turn : int;
  mutable det_locs : int list;
}

let create board_path : t =
  if Sys.is_file_exn Jack_process.game_file then
    Sys.remove Jack_process.game_file;

  let game = Jack_process.game in
  Jack_process.run_jack ~game:game ~board:board_path "init" |> print;
  { game = Jack_process.game;
    board = board_path;
    turn = 0;
    det_locs = []; }

let det_set_loc t locs : unit =
  t.det_locs <- locs;

  (* update the CLT which reads the locations - 1000 *)
  let locs = List.map locs ~f:(fun loc -> loc - 1000) in
  Jack_process.run_jack_locs ~game:t.game ~board:t.board "detloc" locs |> print

let det_miss t locs : unit =
  Jack_process.run_jack_locs ~game:t.game ~board:t.board "detmiss" locs |> print

let det_arrest t locs : unit =
  Jack_process.run_jack_locs ~game:t.game ~board:t.board "detarrest" locs |> print

let det_hit t locs : unit =
  Jack_process.run_jack_locs ~game:t.game ~board:t.board "dethit" locs |> print

let jack_start t locs : unit =
  t.turn <- 0;
  Jack_process.run_jack_locs ~game:t.game ~board:t.board "jackstart" locs |> print

type jack_move =
  | Alley
  | Car
  | Walk

let jack_move t jack_move : unit =
  let cmd = "jack " ^ 
            (match jack_move with
             | Alley -> 
               t.turn <- t.turn + 1;
               "alley"
             | Car -> 
               t.turn <- t.turn + 2;
               "car"
             | Walk ->
               t.turn <- t.turn + 1;
               "walk") in 
  Jack_process.run_jack ~game:t.game ~board:t.board cmd |> print

let jack_locs t : int list =
  let lines = Jack_process.run_jack ~game:t.game ~board:t.board "print" in
  print lines;

  let line = List.hd_exn lines in
  let one, two = String.lsplit2_exn line ~on:'[' in
  let loc_str = "[" ^ two in
  loc_str
  |> String.tr ~target:'\'' ~replacement:' '
  |> Yojson.Basic.from_string
  |> Yojson.Basic.Util.to_list
  |> Yojson.Basic.Util.filter_int

let debug t : unit =
  Jack_process.run_jack ~game:t.game ~board:t.board "debug" |> print

class tree_jack_tracker (board_path : string) : Tracker.jack_tracker =
  object(self)
    val tracker = create board_path

    method print () = debug tracker
    method curr_turn () = tracker.turn
    method jack_locs turn = jack_locs tracker
    method curr_jack_locs () = jack_locs tracker
    method curr_det_locs () = tracker.det_locs
    method jack_start_locs locs = jack_start tracker locs
    method perform_action action =
      match action with
      | Tracker.Action.DetSniffHit loc -> det_hit tracker [loc]
      | Tracker.Action.DetSniffMiss loc -> det_miss tracker [loc]
      | Tracker.Action.DetArrestMiss loc -> det_arrest tracker [loc]
      | Tracker.Action.JackWalk -> jack_move tracker Walk
      | Tracker.Action.JackCar -> jack_move tracker Car
      | Tracker.Action.JackAlley -> jack_move tracker Alley
      | Tracker.Action.DetLocChange locs -> det_set_loc tracker locs
    method test_action action =
      (* no supported *)
      assert false;
  end

let create (board_path : string) : Tracker.jack_tracker =
  new tree_jack_tracker board_path
