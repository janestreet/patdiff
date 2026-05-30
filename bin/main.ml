open Core

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some version -> Build_info.V1.Version.to_string version
;;

let () =
  let result = Result.try_with (fun () -> Command_unix.run ~version Compare.command) in
  match result with
  | Ok () -> ()
  | Error exn ->
    eprintf "%s\n%!" (Exn.to_string exn);
    exit 2
;;
