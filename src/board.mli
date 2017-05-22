open Core.Std

(*
 * Provides an interface for the Whitechapel game board.
 *)

type t

(* Create a board from a JSON file. *)
val from_json_file : string -> t

(* Return all white nodes adjacent to a white node. *)
val adj_to_white_node : t -> int -> int list

(* Supply a white node path and return the black node paths between it. *)
val white_path_black_node_paths : t -> int * int -> int list list

(* Return all white nodes adjacent to a white node from an alley. *)
val block_white_adj_nodes : t -> int -> int list

(* Return a list of all white nodes. *)
val white_nodes : t -> int list

(* Return a list of all black nodes. *)
val black_nodes : t -> int list
