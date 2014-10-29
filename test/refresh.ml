open Core.Std
open Core_extended.Std

let refresh_res ~old_file ~new_file ~keep_ws ~latex =
  let res_file = old_file ^ ".res" in
  let patdiff_cmd = Common.patdiff_cmd ~old_file ~new_file ~keep_ws ~latex in
  let p_cmd = sprintf "%s > %s" patdiff_cmd res_file in
  let p_exit = Unix.system p_cmd in
  match p_exit with
  | Ok () | Error (`Exit_non_zero 1) ->
      printf "%s was refreshed\n%!" res_file
  | Error _ ->
      eprintf "\"%s\" command failed\n%!" p_cmd;
      eprintf "%s failed to be refreshed\n%!" res_file

let instance name =
  let _exit_code, keep_ws, latex = Common.parse_filename_flags name in
  let old_file = name in
  let new_file = name ^ ".new" in
  refresh_res ~old_file ~new_file ~keep_ws ~latex

let summary = "\
  Refresh the test results in the files directory

  Examples:
\t./refresh.exe
\t./refresh.exe files/empty1
\t./refresh.exe files/empty1 files/oneline_same files/swap"
let usage_arg = "[FILE1 ... FILEN]"
let init () = ()
let flags = []

let final () = function
  | [] -> None
  | files -> Some files
;;

let main args =
  let default = Common.files in
  let files = Option.value args ~default in
  List.iter files ~f:instance
;;

let cmd = Deprecated_command.create ~summary ~usage_arg ~init ~flags ~final main

let () = Deprecated_command.run cmd ~hash_bang_expand:true
