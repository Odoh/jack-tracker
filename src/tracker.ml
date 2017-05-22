open Core.Std
open Yojson.Basic.Util

(* 
 * Collection of generic types which are shared by each Jack tracker algorithm.
 * Provides a common interface across each algorithm.
 *)

(*
 * Describes an Action that may be taken in a night of Whitechapel.
 *)
module Action = struct

(*
 * A Whitechapel night action:
 *   DetSniffHit   = detective search for a clue and found Jack's trail
 *   DetSniffMiss  = detective search for a clue and did not find Jack's trail
 *   DetArrestMiss = detect arrest which did not arrest Jack
 *   JackWalk      = Jack performs a normal movement to an adjacent node
 *   JackCar       = Jack performs a special movement, using a carriage
 *   JackAlley     = Jack performs a special movement, using an alley
 *   DetLocChange  = the detectives change locations
 *)
  type t =
    | DetSniffHit of int
    | DetSniffMiss of int
    | DetArrestMiss of int
    | JackWalk
    | JackCar
    | JackAlley
    | DetLocChange of int list

  let to_string action =
    match action with
    | DetSniffHit n -> "DetSniffHit " ^ (Int.to_string n)
    | DetSniffMiss n -> "DetSniffMiss " ^ (Int.to_string n)
    | DetArrestMiss n -> "DetArrestMiss " ^ (Int.to_string n)
    | JackWalk -> "JackWalk"
    | JackCar -> "JackCar"
    | JackAlley -> "JackAlley"
    | DetLocChange ns -> "DetLocChange " ^ (List.fold (List.tl_exn ns)
                                                      ~init:(ns |> List.hd_exn |> Int.to_string)
                                                      ~f:(fun str n -> str ^ ", " ^ (Int.to_string n)))

  let to_json action =
    match action with
    | DetSniffHit n -> `Assoc [ ("action", `String "DetSniffHit");
                                ("node", `Int n) ]
    | DetSniffMiss n -> `Assoc [ ("action", `String "DetSniffMiss");
                                 ("node", `Int n) ]
    | DetArrestMiss n -> `Assoc [ ("action", `String "DetArrestMiss");
                                  ("node", `Int n) ]
    | JackWalk -> `Assoc [ ("action", `String "JackWalk") ]
    | JackCar -> `Assoc [ ("action", `String "JackCar") ]
    | JackAlley -> `Assoc [ ("action", `String "JackAlley") ]
    | DetLocChange ns -> `Assoc [ ("action", `String "DetLocChange");
                                  ("nodes", `List (List.map ns ~f:(fun n -> `Int n))) ]
end

(* Exception thrown on an illegal game action. *)
exception Illegal_Action of string

(* Interface for Jack tracker algorithm *)
class type jack_tracker =
  object
    (* Print the current state of the tracker. *)
    method print : unit -> unit

    (* Return the current turn in the game. *)
    method curr_turn : unit -> int

    (* Return Jack's potential locations at the specified turn. *)
    method jack_locs : int -> int list

    (* Return Jack's potential current locations. *)
    method curr_jack_locs : unit -> int list

    (* Return the detective's current locations. *)
    method curr_det_locs : unit -> int list

    (* Set Jack's start location(s). *)
    method jack_start_locs : int list -> unit

    (* Perform an action, updating the tracker. *)
    method perform_action : Action.t -> unit

    (* Perform an action, but do not update the tracker. *)
    method test_action : Action.t -> unit
  end
