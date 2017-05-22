open Core.Std

(* Comparator used for an int tuple map. *)
module IntTupleComparator = Comparator.Make(struct
    type t = int * int
    let sexp_of_t tuple = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t tuple
    let t_of_sexp tuple = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp tuple
    let compare (x0, y0) (x1, y1) =
      match Pervasives.compare x0 x1 with
        0 -> Pervasives.compare y0 y1
      | c -> c
  end)

(* Handles the deserialization of a JSON map file. *)
module Json = struct
  module Yo = Yojson.Basic.Util

  (* Create a json object for a board from a file. *)
  let board_json_from_file filename =
    Yojson.Basic.from_file filename

  (* Create the black node adj map. *)
  let black_adj_nodes board_json =
    board_json
    |> Yo.member "Blacks"
    |> Yo.to_assoc
    |> List.map ~f:(fun x -> 
        let k, json = x in
        Int.of_string k, json 
                         |> Yo.to_list
                         |> Yo.filter_string
                         |> List.map ~f:(fun i -> Int.of_string i))
    |> Int.Map.of_alist_exn

  (* Create the white node adj map. *)
  let white_adj_nodes board_json =
    board_json
    |> Yo.member "WhiteAdj"
    |> Yo.to_assoc
    |> List.map ~f:(fun x ->
        let source, json = x in
        Int.of_string source, json
                              |> Yo.to_assoc
                              |> List.map ~f:(fun assoc ->
                                  let dest, vs = assoc in
                                  Int.of_string dest))
    |> Int.Map.of_alist_exn

  (* Create the white node blocks list. *)
  let blocks board_json =
    board_json
    |> Yo.member "Blocks"
    |> Yo.to_list
    |> Yo.filter_list
    |> List.map ~f:(fun block -> 
        Yo.filter_string block
        |> List.map ~f:Int.of_string)

  (* Create the white node adj path list. *)
  let white_adj_paths board_json =
    (* paths_from_json has this format:
     * [ (1, [ (2, [[1001]])
     *         (3, [[1002]]) ])
     *   (2, [ (4, [[1004]]) ]) ] *)
    let paths_from_json : (int * (int * int list list) list) list =
      board_json
      |> Yo.member "WhiteAdj"
      |> Yo.to_assoc
      |> List.map ~f:(fun x ->
          let source, json = x in
          Int.of_string source, json 
                                |> Yo.to_assoc
                                |> List.map ~f:(fun y ->
                                    let dest, json = y in
                                    Int.of_string dest,json 
                                                       |> Yo.to_list
                                                       |> Yo.filter_list
                                                       |> List.map ~f:(fun path ->
                                                           Yo.filter_string path
                                                           |> List.map ~f:Int.of_string))) in
    (* paths_to_map has this format:
     * [ (1, 2) [[1001]]
     *   (1, 3) [[1002]]
     *   (2, 4) [[1004]] ] *)
    let paths_to_map : ((int * int) * (int list list)) list =
      paths_from_json
      |> List.concat_map ~f:(fun x ->
          let source, dests = x in
          dests
          |> List.map ~f:(fun y ->
              let dest, paths = y in
              (source, dest), paths)) in

    (* finally, we have a map! *)
    paths_to_map |> Map.of_alist_exn ~comparator:IntTupleComparator.comparator
end

(* The Whitechapel game board.
 *   black_adj_nodes       = map from black node to all adjacent nodes
 *   white_adj_nodes       = map from white node to all adjacent nodes
 *   white_adj_paths       = map from white node path to all black nodes between it
 *   block_white_adj_nodes = map from white node to all block adjacent white nodes
 *)
type t = 
  { black_adj_nodes : int list Int.Map.t;
    white_adj_nodes : int list Int.Map.t;
    white_adj_paths : (int * int, int list list, IntTupleComparator.comparator_witness) Core.Std.Map.t;
    block_white_adj_nodes : int list Int.Map.t }

let from_json_file filename : t =
  let board_json = Json.board_json_from_file filename in
  let black_adj_nodes = Json.black_adj_nodes board_json in 
  let white_adj_nodes = Json.white_adj_nodes board_json in
  let white_adj_paths = Json.white_adj_paths board_json in
  let blocks = Json.blocks board_json in
  let block_white_adj_nodes = 
    Map.keys white_adj_nodes (* all white nodes *)
    |> List.map ~f:(fun src ->
        let dsts = List.filter blocks ~f:(fun block -> List.exists block ~f:(fun dst -> dst = src))
                   |> List.concat
                   |> List.filter ~f:(fun dst -> dst <> src) in
        (src, dsts))
    |> Map.of_alist_exn ~comparator:Int.comparator in
  { black_adj_nodes = black_adj_nodes;
    white_adj_nodes = white_adj_nodes;
    white_adj_paths = white_adj_paths;
    block_white_adj_nodes = block_white_adj_nodes }

let adj_to_white_node board node : int list =
  Map.find_exn board.white_adj_nodes node

let white_path_black_node_paths board white_path : int list list =
  Map.find_exn board.white_adj_paths white_path

let block_white_adj_nodes board node : int list =
  Map.find_exn board.block_white_adj_nodes node

let white_nodes board : int list =
  Map.keys board.white_adj_nodes

let black_nodes board : int list =
  Map.keys board.black_adj_nodes
