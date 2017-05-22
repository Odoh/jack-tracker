open Core.Std
open Yojson.Basic.Util

(* A Night tracker.
 *   jack_tracker    = the Jack tracker tracking Jack
 *   actions         = the actions taken this night
 *   jack_start_locs = Jack's starting location(s)
 *)
type t = {
  jack_tracker: Tracker.jack_tracker;
  mutable actions: Tracker.Action.t list;
  mutable jack_start_locs : int list;
}

type save_file_info = {
  jack_starts: int list;
  jack_locs: int list;
  action_list: Tracker.Action.t list;
}

(* Get Jack's start locations. *)
let jack_start_locs t : int list =
  t.jack_start_locs

(* Get the actions taken. *)
let actions t : Tracker.Action.t list =
  (* actions are stored in reverse order (for faster appending) *)
  List.rev t.actions

let create jack_tracker : t =
  { jack_tracker = jack_tracker;
    actions = [];
    jack_start_locs = []; }

let curr_turn t : int =
  t.jack_tracker#curr_turn ()

(*
 * Jack Locs
 *)

let all_jack_locs t : int list =
  (* combine all locations from turn 0 to now *)
  List.range 0 ((curr_turn t) + 1)
  |> List.fold ~init:[] ~f:(fun acc turn -> 
    (t.jack_tracker#jack_locs turn) :: acc)
  |> List.concat
  |> List.dedup

let sure_jack_locs t : int list =
  (* sure locations on Jack's route include: *)
  (* - turns with only one potential location *)
  (* - locations of DetSniffHit actions *)
  let single_locs = List.range 0 ((curr_turn t) + 1)
                    |> List.fold ~init:[] ~f:(fun acc turn -> 
                        let locs = t.jack_tracker#jack_locs turn in
                        if List.length locs = 1 then locs :: acc
                        else acc)
                    |> List.concat in
  let sniff_hits = List.filter_map t.actions ~f:(fun action ->
      match action with
      | Tracker.Action.DetSniffHit loc -> Some loc
      | _ -> None) in
  List.concat [single_locs; sniff_hits]
  |> List.dedup

let curr_jack_locs t : int list =
  t.jack_tracker#curr_jack_locs ()

let new_jack_locs t : int list =
  (* new potential Jack locations are location which exist *)
  (* on the current turn but not on the previous turn *)
  let curr_turn = t.jack_tracker#curr_turn () in
  let prev_turn = curr_turn - 1 in
  if curr_turn = 0 then t.jack_tracker#curr_jack_locs ()
  else (
    let curr_locs = t.jack_tracker#jack_locs curr_turn in
    let prev_locs = t.jack_tracker#jack_locs prev_turn in
    List.filter curr_locs ~f:(fun curr_loc ->
      not (List.exists prev_locs ~f:(fun prev_loc ->
        curr_loc = prev_loc)))
  )

let prev_jack_locs t : int list =
  let all_locs = all_jack_locs t in
  let curr_locs = curr_jack_locs t in
  List.filter all_locs ~f:(fun all_loc ->
    not (List.exists curr_locs ~f:(fun curr_loc -> all_loc = curr_loc)))

let curr_det_locs t : int list =
  t.jack_tracker#curr_det_locs ()

let set_jack_start_locs t start_locs : unit =
  t.jack_start_locs <- start_locs;
  t.jack_tracker#jack_start_locs start_locs

let perform_action t action : unit =
  (* an action is performed by: *)
  (*   - adding the action to the Night tracker *)
  (*   - performing the action on the Jack tracker *)
  (* for efficiency, detect repeat actions and do nothing for them *)
  let run_action = fun () -> (
      t.actions <- action :: t.actions;
      t.jack_tracker#perform_action action
    ) in
  if List.is_empty t.actions then run_action ()
  else (
    let prev_action = List.hd_exn t.actions in
    match prev_action, action with
    | Tracker.Action.DetSniffHit n1, Tracker.Action.DetSniffHit n2 ->
      if n1 = n2 then () else run_action ()
    | Tracker.Action.DetSniffMiss n1, Tracker.Action.DetSniffMiss n2 ->
      if n1 = n2 then () else run_action ()
    | Tracker.Action.DetArrestMiss n1, Tracker.Action.DetArrestMiss n2 ->
      if n1 = n2 then () else run_action ()
    | Tracker.Action.DetLocChange _, Tracker.Action.DetLocChange locs ->
      (* remove old loc change and use new ones *)
      t.actions <- action :: (List.tl_exn t.actions);
      t.jack_tracker#perform_action action
    | _, _ -> run_action ()
  )

(*
 * Persistence
 *)

let to_json t =
  `Assoc [
    ("jackstarts", `List (List.map (jack_start_locs t) ~f:(fun n -> `Int n)));
    ("actions", `List (List.map (actions t) ~f:(fun a -> Tracker.Action.to_json a)));
    ("jacklocs", `List (List.map (curr_jack_locs t) ~f:(fun n -> `Int n)));
  ]

let from_json json : save_file_info =
  let jack_starts = json 
                    |> member "jackstarts"
                    |> to_list
                    |> filter_int in
  let jack_locs = json
                  |> member "jacklocs"
                  |> to_list
                  |> filter_int in
  let actions = json
                |> member "actions"
                |> to_list
                |> List.map ~f:(fun json ->
                    let action_str = json |> member "action" |> to_string in
                    match action_str with
                    | "DetSniffHit" ->
                      let node = json |> member "node" |> to_int in
                      Tracker.Action.DetSniffHit node
                    | "DetSniffMiss" ->
                      let node = json |> member "node" |> to_int in
                      Tracker.Action.DetSniffMiss node
                    | "DetArrestMiss" ->
                      let node = json |> member "node" |> to_int in
                      Tracker.Action.DetArrestMiss node
                    | "JackWalk" ->
                      Tracker.Action.JackWalk
                    | "JackCar" ->
                      Tracker.Action.JackCar
                    | "JackAlley" ->
                      Tracker.Action.JackAlley
                    | "DetLocChange" ->
                      let nodes = json |> member "nodes" |> to_list |> filter_int in
                      Tracker.Action.DetLocChange nodes
                    | _ -> raise (Invalid_argument ("action type not handled: " ^ action_str))) in
  { jack_starts = jack_starts;
    jack_locs = jack_locs;
    action_list = actions; }

let save_file_info filename : save_file_info =
  let json = Yojson.Basic.from_file filename in
  from_json json

let save_to_file t filename : unit =
  let json = to_json t in
  Out_channel.write_all filename ~data:(Yojson.Basic.pretty_to_string json)

let load_from_file jack_tracker filename : t =
  let info = save_file_info filename in
  (* apply actions to the jack_tracker *)
  jack_tracker#jack_start_locs info.jack_starts;
  List.iter info.action_list ~f:(fun action -> jack_tracker#perform_action action);
  { jack_tracker = jack_tracker;
    actions = List.rev info.action_list;
    jack_start_locs = info.jack_starts; }

