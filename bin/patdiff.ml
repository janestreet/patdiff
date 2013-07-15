open Core.Std
open Core_extended.Std

let () =
  let result = Result.try_with (fun () ->
    Deprecated_command.run Compare.command ~hash_bang_expand:true
  )
  in
  match result with
  | Ok () -> ()
  | Error exn -> eprintf "%s\n%!" (Exn.to_string exn); exit 2
;;
