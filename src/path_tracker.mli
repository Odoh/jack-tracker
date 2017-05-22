(*
 * A Jack tracker that tracks by keeping record of all possible
 * paths Jack could have taken in a list.
 *)

(* Create a new Path tracker using the specified board. *)
val create : Board.t -> Tracker.jack_tracker
