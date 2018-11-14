open! Core

let exists_shebang_line file_contents ~f =
  String.is_prefix file_contents ~prefix:"#!"
  &&
  match String.lsplit2 file_contents ~on:'\n' with
  | None -> false
  | Some (first_line, _) -> f first_line
;;

let looks_like_python ~file:filename ~contents =
  String.is_suffix filename ~suffix:".py"
  || exists_shebang_line contents ~f:(String.is_substring ~substring:"python")
;;

let for_diff ~file1 ~file2 ~contents1 ~contents2 =
  looks_like_python ~file:file1 ~contents:contents1
  || looks_like_python ~file:file2 ~contents:contents2
;;
