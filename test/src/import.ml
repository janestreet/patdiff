open! Core
open! Async
include Expect_test_helpers_core
include Expect_test_helpers_async

let pipe commands =
  List.map commands ~f:(fun (prog, args) ->
    String.concat ~sep:" " (List.map (prog :: args) ~f:Filename.quote))
  |> String.concat ~sep:" | "
  |> sprintf "set -o pipefail; %s"
  |> system
;;

let links =
  [ "../../bin/main.exe", `In_path_as, "patdiff"
  ; "../../../ansi_text/bin/main.exe", `In_path_as, "ansi_text"
  ]
;;

let patdiff_dir ~extra_flags ~prev ~next =
  within_temp_dir ~links (fun () ->
    let%bind () =
      Deferred.List.iter
        ~how:`Sequential
        [ "prev", prev; "next", next ]
        ~f:(fun (prefix, xs) ->
          Deferred.List.iter ~how:`Sequential xs ~f:(fun (path, contents) ->
            let path = prefix ^/ path in
            let%bind () = Unix.mkdir ~p:() (Filename.dirname path) in
            Writer.save path ~contents))
    in
    pipe
      [ "patdiff", [ "-default"; "prev"; "next" ] @ extra_flags
      ; "ansi_text", [ "visualize"; "-minimize" ]
      ])
;;

let patdiff ~extra_flags ~prev ~next =
  patdiff_dir ~extra_flags ~prev:[ "file", prev ] ~next:[ "file", next ]
;;
