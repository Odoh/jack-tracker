(*
 * A Jack tracker which stores information about Jack's position and moves each turn.
 * The resulting tree is then created as needed for pruning.
 *)

(* Create a new Turn tracker. *)
val create : Board.t -> Tracker.jack_tracker
