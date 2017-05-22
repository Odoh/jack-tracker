open Core.Std

(* Jack Track
 * ==========
 * Command line tool for tracking the location of Jack in the board game Letters from Whitechapel.
 * - choose the Whitechapel board JSON to use
 * - choose the tracking algorithm to use
 * - ability to generate test scenarios
 * - ability to run test scenarios comparing the results
 * - ability to run the tracker as a web server
 *)

let () =
  let cmd_spec = let open Command.Spec in empty
                                          (* required *)
                                          +> flag "--board" ~aliases:["-b"] (required string) ~doc:" name of the board json file"
                                          +> flag "--tracker" ~aliases:["-t"] (optional_with_default "turn" string) ~doc:" 'path', 'tree', or 'turn' specifying the tracker to use. Default: turn"
                                          +> anon ("command" %: string)
                                          (* testgen *)
                                          +> flag "--num-tests" (optional int) ~doc:" number of tests to generate"
                                          +> flag "--num-actions" (optional int) ~doc:" number of actions per test"
                                          +> flag "--prefix" (optional_with_default "gen" string) ~doc:" prefix for generated test files. Default: gen"
                                          +> flag "--jack-start-limit" (optional_with_default 2 int) ~doc:" max number of jack start locations to test"
                                          (* testrun *)
                                          +> flag "--test-path" (optional string) ~doc:" a test file or directory of test files"
                                          +> flag "--verbose" ~aliases:["-v"] no_arg ~doc:" whether to verbose log the state of the tracker"
                                          (* webserver *)
                                          +> flag "--port" ~aliases:["-p"] (optional_with_default 8080 int) ~doc:" listen for requests running as a server on the specified port. Default: 8080"
  in

  let cmd_cmd = Command.basic ~summary:" jack_track <testgen|testrun|server>"
      cmd_spec
      (fun board_f tracker_f command_f
           num_tests_f num_actions_f prefix_f jack_start_limit_f
           test_path_f verbose_f port_f () -> 
         let board = if Sys.is_file_exn board_f then
             Board.from_json_file board_f
           else
             (eprintf "'%s' is not a valid board json file\n" board_f; exit 1) in
         let tracker_fn = match tracker_f with
           | "path" -> (fun () -> Path_tracker.create board)
           | "turn" -> (fun () -> Turn_tracker.create board)
           | "tree" -> (fun () -> Tree_tracker.create board_f)
           | _ -> (eprintf "'%s' is not a valid tracker\n" tracker_f; exit 1) in
         match command_f with
         | "testgen" ->
           if tracker_f = "tree" then (eprintf "tree not supported for testgen\n"; exit 1);
           let num_tests = match num_tests_f with
             | Some n -> n
             | None -> (eprintf "--num-tests required to testgen\n"; exit 1) in
           let num_actions = match num_actions_f with
             | Some n -> n
             | None -> (eprintf "--num-actions required to testgen\n"; exit 1) in
           let prefix : string = prefix_f in
           Test_gen.gen_tests board
                              tracker_fn
                              prefix
                              ~num_tests:num_tests
                              ~num_actions:num_actions
                              ~jack_start_limit:jack_start_limit_f
         | "testrun" ->
           let test_path = match test_path_f with
             | Some t -> if (Sys.is_file_exn t) || (Sys.is_directory_exn t) then t 
               else (eprintf "'%s' is not a valid file or directory\n" t; exit 1)
             | None -> (eprintf "--test required to testrun\n"; exit 1) in
           Test_run.run_tests tracker_fn test_path ~verbose:verbose_f
         | "server" ->
           Web_server.run port_f tracker_fn
         | _ -> (eprintf "'%s' is not a valid command\n" tracker_f; exit 1)) in

  Command.run ~version:"1.0" cmd_cmd
