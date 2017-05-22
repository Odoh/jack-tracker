open Core.Std

(*
 * A Jack tracker that also stores the history of all actions taken that night.
 * A Night tracker may serialize its state and save it to a file, or
 * deserialize the state loaded from a file.
 *)

type t

(* Information stored in a Night_tracker file.
 *   jack_starts = Jack's starting location(s)
 *   jack_locs   = Jack's potential current locations
 *   action_list = List of actions taken that night.
 *)
type save_file_info = {
  jack_starts: int list;
  jack_locs: int list;
  action_list: Tracker.Action.t list;
}

(* Create a Night tracker using the specified Jack tracker. *)
val create : Tracker.jack_tracker -> t

(* Return the current turn *)
val curr_turn : t -> int

(*
 * Jack Locs
 *)

(* Return all past and present potential Jack locations. *)
val all_jack_locs : t -> int list

(* Return all locations that Jack has previously been (or is at). *)
val sure_jack_locs : t -> int list

(* Return all current potential Jack locations. *)
val curr_jack_locs : t -> int list

(* Return current potential Jack locactions. *)
val new_jack_locs : t -> int list

(* Return past potential Jack locations. *)
val prev_jack_locs : t -> int list

(* Return the current detective locations. *)
val curr_det_locs : t -> int list

(* Set Jack's starting location(s). *)
val set_jack_start_locs : t -> int list -> unit

(* Perform an Action. *)
val perform_action : t -> Tracker.Action.t -> unit

(*
 * Persistence
 *)

(* Retrieve the save_file_info from a file of the specified name. *)
val save_file_info : string -> save_file_info

(* Save the Night tracker to a file of the specified name. *)
val save_to_file : t -> string -> unit

(* Create a Night tracker using the specified Jack tracker loading state from a file. *)
val load_from_file : Tracker.jack_tracker -> string -> t
