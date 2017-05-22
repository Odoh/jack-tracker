exception Test_Failure of string

(* Generate tests for a jack tracker. *)
val run_tests : (unit -> Tracker.jack_tracker) -> string -> verbose:bool -> unit
