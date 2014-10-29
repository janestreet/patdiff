(** TO CREATE A NEW TEST:

    Copy the old file into files/, with any desired filename options
    separated by dots.  Copy the new file into files/ with the same
    name, but append '.new' to the end.  Run ./refresh.exe to create
    the .res file.

    Available options:

    same ->    The expected exit code will be 0. Else, 1.
    keep_ws -> Run patdiff with -keep-whitespace.
    latex ->   Run patdiff with -latex.

    Example:

    $ cp ~/old files/newlines.same.keep_ws
    $ cp ~/new files/newlines.same.keep_ws.new
    $ ./refresh.exe
    $g cat files/newlines.same.keep_ws.res
    $

*)


open Core.Std
open OUnit

let test_files ~patdiff_cmd ~result_file ~exit_code =
  let is_ok =
    let tmp () = Filename.temp_file "patdiff_test" ".txt" in
    let unlink x = Exn.handle_uncaught ~exit:false (fun () -> Unix.unlink x) in
    let p_tmp = tmp () in
    let p_cmd = sprintf "%s > %s" patdiff_cmd p_tmp in
    if false then Printf.printf "%s\n%!" patdiff_cmd;
    let p_exit = Unix.system p_cmd in
    let d_tmp = tmp () in
    let d_cmd = sprintf "diff %s %s > %s" result_file p_tmp d_tmp in
    let d_exit = Unix.system d_cmd in
    let is_ok = match p_exit with
      | Error (`Signal _) -> false
      | Ok () | Error #Unix.Exit.error as e ->
          let i = Unix.Exit.code e in
          if not (i = exit_code) then begin
            eprintf "%S exited %i\n%!" p_cmd i;
            false end
          else
            match d_exit with
            | Error (`Signal _ | `Exit_non_zero 1) ->
                let cmd = sprintf "cat %s" d_tmp in
                ignore (Unix.system cmd);
                false
            | Ok () -> true
            | Error (`Exit_non_zero _) -> false                  
    in
    if not is_ok then exit 1;
    unlink p_tmp;
    unlink d_tmp;
    is_ok
  in
  let name = String.drop_suffix result_file 4 in
  (sprintf "%s" name) @? is_ok

let instance name () =
  let exit_code, keep_ws, latex = Common.parse_filename_flags name in
  let old_file = name in
  let new_file = name ^ ".new" in
  let patdiff_cmd = Common.patdiff_cmd ~old_file ~new_file ~keep_ws ~latex in
  let result_file = name ^ ".res" in
  test_files ~patdiff_cmd ~result_file ~exit_code

let all = TestList [
  "all" >:::
    (List.map Common.files ~f:(fun name ->
      name >:: instance name))
  ]
