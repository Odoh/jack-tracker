open Core.Std
open Opium.Std
open Yojson.Basic.Util
open Lwt.Infix

module Session_store = struct

  let directory = "sessions"

  let filename session =
    directory ^ "/" ^ session

  type t = {
    mutable map: Night_tracker.t String.Map.t;
    tracker_fn: unit -> Tracker.jack_tracker;
  }

  let create tracker_fn : t =
    if not (Sys.is_directory_exn directory) then
      Unix.mkdir directory;

    { map = String.Map.empty;
      tracker_fn = tracker_fn }

  let find t session : Night_tracker.t option =
    match String.Map.find t.map session with
    | Some tracker -> Some tracker
    | None ->
      (* try to load it from a file *)
      let filename = filename session in
      if Sys.is_file_exn filename then
        Some (Night_tracker.load_from_file (t.tracker_fn ()) filename)
      else
        None

  let add t session : unit =
    (* remove an old save file, if it exists *)
    let filename = filename session in
    if Sys.is_file_exn filename then
      Sys.remove filename;

    let night_tracker = Night_tracker.create (t.tracker_fn ()) in
    t.map <- String.Map.add t.map ~key:session ~data:night_tracker

  let load t session : unit =
    let filename = filename session in
    let night_tracker = Night_tracker.load_from_file (t.tracker_fn ()) filename in
    t.map <- String.Map.add t.map ~key:session ~data:night_tracker

  let save night_tracker session : unit =
    let filename = filename session in
    Night_tracker.save_to_file night_tracker filename
end


let add_cors_headers (headers: Cohttp.Header.t): Cohttp.Header.t =
  Cohttp.Header.add_list headers [
    ("Access-Control-Allow-Origin", "*");
    ("Access-Control-Allow-Headers", "Content-Type");
    ("Access-Control-Allow-Methods", "POST, GET, OPTIONS");
  ]

let cors_middleware =
  let filter handler req =
    let open Opium_rock.Response in
    handler req >>= fun response ->
    (* let headers = Cohttp.Header.add response.headers *)
    (*     "Access-Control-Allow-Origin" "*" in *)
    let headers = add_cors_headers response.headers in
    Lwt.return { response with headers } in
  Rock.Middleware.create ~name:"CORS headers for Opium" ~filter
  (* Rock.Middleware.create ~name:(Info.of_string "CORS headers for Opium") ~filter *)

let accept_options = App.options "**" begin fun _ ->
  respond' (`String "OK")
end

let session_from_body body : string option =
  try
    Some (Yojson.Basic.from_string body
          |> member "session"
          |> to_string)
  with
  | _ -> None

let session_nodes_from_body body : (string * int list) option =
  try
    let json = Yojson.Basic.from_string body in
    Some (json |> member "session" |> to_string,
          json |> member "nodes" |> to_list |> filter_int)
  with
  | _ -> None

let session_nodes_action ?add store body night_tracker_action =
  match session_nodes_from_body body with
  | Some (session, nodes) -> (
      (match add with
       | Some _ -> Session_store.add store session
       | None -> ());
      match Session_store.find store session with
      | Some night_tracker -> (
          try
            night_tracker_action session night_tracker nodes;
            Session_store.save night_tracker session;
            respond (`String "Accepted")
          with
          | Tracker.Illegal_Action msg -> 
            (* with the error, the in memory tracker is now invalid. restore it with its file *)
            Session_store.load store session;
            respond ~code:Cohttp.Code.(`Bad_request) (`String ("Invalid tracker action: " ^ msg))
        )
      | None -> respond ~code:Cohttp.Code.(`Bad_request) (`String ("No tracker for session: " ^ session))
    )
  | None -> respond ~code:Cohttp.Code.(`Bad_request) (`String ("No session or nodes in request: " ^ body))

let session_action store body night_tracker_action =
  match session_from_body body with
  | Some session -> (
      match Session_store.find store session with
      | Some night_tracker -> (
          try
            night_tracker_action session night_tracker;
            Session_store.save night_tracker session;
            respond (`String "Accepted")
          with
          | Tracker.Illegal_Action msg ->
            (* with the error, the in memory tracker is now invalid. restore it with its file *)
            Session_store.load store session;
            respond ~code:Cohttp.Code.(`Bad_request) (`String ("Invalid tracker action: " ^ msg))
        )
      | None -> respond ~code:Cohttp.Code.(`Bad_request) (`String ("No tracker for session: " ^ session))
    )
  | None -> respond ~code:Cohttp.Code.(`Bad_request) (`String ("No session in request: " ^ body))


let run port tracker_fn =
  let store = Session_store.create tracker_fn in
  App.empty
  |> App.port port
  |> middleware cors_middleware
  |> accept_options
  |> get "/" begin fun req -> `String ("Jack Tracker") |> respond' end

  |> get "/det/locs/:session" begin fun req ->
    let session = param req "session" in
    match Session_store.find store session with
    | Some night_tracker ->
      let curr_det_locs = Night_tracker.curr_det_locs night_tracker in
      respond' (`Json ((`List (List.map curr_det_locs ~f:(fun n -> `Int n)))
                       |> Yojson.to_string
                       |> Ezjsonm.from_string))
    | None -> respond' ~code:Cohttp.Code.(`Bad_request) (`String ("No tracker for session: " ^ session)) end

  |> get "/jack/locs/:session" begin fun req ->
    let session = param req "session" in
    match Session_store.find store session with
    | Some night_tracker ->
      let curr_jack_locs = Night_tracker.curr_jack_locs night_tracker in
      let new_jack_locs = Night_tracker.new_jack_locs night_tracker in
      let prev_jack_locs = Night_tracker.prev_jack_locs night_tracker in
      let sure_jack_locs = Night_tracker.sure_jack_locs night_tracker in
      respond' (`Json (
          `Assoc [
            ("currlocs", (`List (List.map curr_jack_locs ~f:(fun n -> `Int n))));
            ("newlocs", (`List (List.map new_jack_locs ~f:(fun n -> `Int n))));
            ("prevlocs", (`List (List.map prev_jack_locs ~f:(fun n -> `Int n))));
            ("surelocs", (`List (List.map sure_jack_locs ~f:(fun n -> `Int n))));
          ]
          |> Yojson.to_string
          |> Ezjsonm.from_string))
    | None -> respond' ~code:Cohttp.Code.(`Bad_request) (`String ("No tracker for session: " ^ session)) end

  |> get "/jack/currlocs/:session" begin fun req ->
    let session = param req "session" in
    match Session_store.find store session with
    | Some night_tracker ->
      let curr_jack_locs = Night_tracker.curr_jack_locs night_tracker in
      respond' (`Json ((`List (List.map curr_jack_locs ~f:(fun n -> `Int n)))
                       |> Yojson.to_string
                       |> Ezjsonm.from_string))
    | None -> respond' ~code:Cohttp.Code.(`Bad_request) (`String ("No tracker for session: " ^ session)) end

  |> get "/jack/turn/:session" begin fun req ->
    let session = param req "session" in
    match Session_store.find store session with
    | Some night_tracker ->
      let curr_turn = Night_tracker.curr_turn night_tracker in
      respond' (`Json ((`List (List.map [curr_turn] ~f:(fun n -> `Int n)))
                       |> Yojson.to_string
                       |> Ezjsonm.from_string))
    | None -> respond' ~code:Cohttp.Code.(`Bad_request) (`String ("No tracker for session: " ^ session)) end

  |> post "/jack/walk" begin fun req -> 
    req |> App.string_of_body_exn |> Lwt.map (fun body ->
        session_action store body (fun session night_tracker ->
            Night_tracker.perform_action night_tracker Tracker.Action.JackWalk)) end

  |> post "/jack/car" begin fun req -> 
    req |> App.string_of_body_exn |> Lwt.map (fun body ->
        session_action store body (fun session night_tracker ->
            Night_tracker.perform_action night_tracker Tracker.Action.JackCar)) end

  |> post "/jack/alley" begin fun req -> 
    req |> App.string_of_body_exn |> Lwt.map (fun body ->
        session_action store body (fun session night_tracker ->
            Night_tracker.perform_action night_tracker Tracker.Action.JackAlley)) end

  |> post "/jack/startlocs" begin fun req -> 
    req |> App.string_of_body_exn |> Lwt.map (fun body ->
        session_nodes_action ~add:true store body (fun session night_tracker nodes ->
            Night_tracker.set_jack_start_locs night_tracker nodes)) end

  |> post "/det/sniffhit" begin fun req -> 
    req |> App.string_of_body_exn |> Lwt.map (fun body ->
        session_nodes_action store body (fun session night_tracker nodes ->
            List.iter nodes ~f:(fun node ->
                Night_tracker.perform_action night_tracker (Tracker.Action.DetSniffHit node)))) end

  |> post "/det/sniffmiss" begin fun req -> 
    req |> App.string_of_body_exn |> Lwt.map (fun body ->
        session_nodes_action store body (fun session night_tracker nodes ->
            List.iter nodes ~f:(fun node ->
                Night_tracker.perform_action night_tracker (Tracker.Action.DetSniffMiss node)))) end

  |> post "/det/arrestmiss" begin fun req -> 
    req |> App.string_of_body_exn |> Lwt.map (fun body ->
        session_nodes_action store body (fun session night_tracker nodes ->
            List.iter nodes ~f:(fun node ->
                Night_tracker.perform_action night_tracker (Tracker.Action.DetArrestMiss node)))) end

  |> post "/det/setlocs" begin fun req -> 
    req |> App.string_of_body_exn |> Lwt.map (fun body ->
        session_nodes_action store body (fun session night_tracker nodes ->
            Night_tracker.perform_action night_tracker (Tracker.Action.DetLocChange nodes))) end
  |> App.start 
  |> Lwt_main.run
