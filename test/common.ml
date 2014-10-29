open Core.Std
open Core_extended.Std

let files =
  let filter = function (name, _stats) ->
    let is_suf s = String.is_suffix name ~suffix:s in
    not (is_suf ".new"
         || is_suf ".res"
         || String.is_prefix (Filename.basename name) ~prefix:".")
  in
  let options = { Find.Options.default with
    Find.Options.max_depth = Some 1;
    Find.Options.filter = Some filter;
  }
  in
  let infos = Find.find_all ~options "files" in
  List.map infos ~f:(fun (name, _stats) -> name)

let patdiff_cmd ~old_file ~new_file ~keep_ws ~latex =
  let flags = "-default -alt-old 'old' -alt-new 'new' -dont-produce-unified-lines" in
  let flags = if keep_ws then flags ^ " -keep-whitespace" else flags in
  let flags = if latex then flags ^ " -latex" else flags in
  sprintf "../bin/patdiff.exe %s %s %s" flags old_file new_file

let parse_filename_flags name =
  let flags = String.split name ~on:'.' in
  let is_flag s = List.exists flags ~f:(String.equal s) in
  let exit_code = if is_flag "same" then 0 else 1 in
  let keep_ws = is_flag "keep_ws" in
  let latex   = is_flag "latex"   in
  (exit_code, keep_ws, latex)
