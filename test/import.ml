open! Core
open! Async

include Expect_test_helpers

let pipe commands =
  List.map commands ~f:(fun (prog, args) ->
    String.concat ~sep:" " (List.map (prog :: args) ~f:Filename.quote))
  |> String.concat ~sep:" | "
  |> sprintf "set -o pipefail; %s"
  |> system
;;

let links =
  [ "../bin/patdiff.exe", `In_path_as, "patdiff"
  ; "../../ansicodes/bin/ansicodes.exe", `In_path_as, "ansicodes"
  ]

let patdiff ~extra_flags ~mine ~other = within_temp_dir ~links (fun () ->
  let%bind () = Writer.save "mine" ~contents:mine
  and      () = Writer.save "other" ~contents:other
  in
  pipe [ ("patdiff" , [ "-default" ; "mine" ; "other"] @ extra_flags)
       ; ("ansicodes", [ "visualize"; "-minimize" ])
       ])
;;
