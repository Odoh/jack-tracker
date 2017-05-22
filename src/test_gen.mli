(* Generate tests for a jack tracker. *)
val gen_tests : Board.t -> (unit -> Tracker.jack_tracker) -> string -> num_tests:int -> num_actions:int -> jack_start_limit:int -> unit
