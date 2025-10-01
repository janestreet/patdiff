open! Core
open! Import
module Unix = Core_unix
include Patdiff_kernel.Compare_core
include Make (Patdiff_core)

(* Returns a Hunk.t list, ready to be printed *)
let compare_files (config : Configuration.t) ~prev_file ~next_file =
  let prev = In_channel.read_all (File_name.real_name_exn prev_file) in
  let next = In_channel.read_all (File_name.real_name_exn next_file) in
  Comparison_result.create
    config
    ~prev:{ name = File_name.display_name prev_file; text = prev }
    ~next:{ name = File_name.display_name next_file; text = next }
    ~compare_assuming_text:(fun config ~prev ~next ->
      let prev_lines, prev_file_newline = File_helpers.lines_of_contents prev.text in
      let next_lines, next_file_newline = File_helpers.lines_of_contents next.text in
      File_helpers.warn_if_no_trailing_newline
        ~warn:(eprintf "No newline at the end of %s\n%!")
        ~prev:(prev_file_newline, prev.name)
        ~next:(next_file_newline, next.name)
        ~warn_if_no_trailing_newline_in_both:config.warn_if_no_trailing_newline_in_both;
      Private.compare_lines config ~prev:prev_lines ~next:next_lines)
;;

(* Print hunks to stdout *)
let print hunks ~file_names ~(config : Configuration.t) =
  let prev_file, next_file = file_names in
  if Comparison_result.has_no_diff hunks
  then (
    if config.double_check
    then (
      match
        Unix.system
          (sprintf
             "cmp -s %s %s"
             (Sys.quote (File_name.real_name_exn prev_file))
             (Sys.quote (File_name.real_name_exn next_file)))
      with
      | Ok () -> ()
      | Error (`Exit_non_zero 1) ->
        printf "There are no differences except those filtered by your settings\n%!"
      | Error _ -> ()))
  else if (* Only print if -quiet is not set *)
          not config.quiet
  then (
    let output = config.output in
    let rules = config.rules in
    match hunks with
    | Binary_same -> assert false
    | Binary_different { prev_is_binary; next_is_binary } ->
      Printf.printf
        "%s\n"
        (File_helpers.binary_different_message
           ~config
           ~prev_file
           ~prev_is_binary
           ~next_file
           ~next_is_binary)
    | Hunks hunks ->
      Patdiff_core.print_unified
        hunks
        ~file_names
        ~output
        ~rules
        ~location_style:config.location_style
    | Structured_hunks hunks ->
      Patdiff_core.print_side_by_side
        ?width_override:config.width_override
        hunks
        ~file_names
        ~rules
        ~wrap_or_truncate:(Option.value config.side_by_side ~default:`wrap)
        ~output)
;;

let diff_files_internal (config : Configuration.t) ~prev_file ~next_file =
  let hunks = compare_files ~prev_file ~next_file config in
  print hunks ~file_names:(prev_file, next_file) ~config;
  if Comparison_result.has_no_diff hunks then `Same else `Different
;;

let with_alt (config : Configuration.t) ~prev ~next : File_name.t * File_name.t =
  ( Real { real_name = prev; alt_name = config.prev_alt }
  , Real { real_name = next; alt_name = config.next_alt } )
;;

let diff_files (config : Configuration.t) ~prev_file ~next_file =
  let prev_file, next_file = with_alt config ~prev:prev_file ~next:next_file in
  diff_files_internal config ~prev_file ~next_file
;;

let is_reg file =
  match Unix.stat (File_name.real_name_exn file) with
  | { st_kind = S_REG; _ } -> true
  | _ -> false
;;

let is_dir file =
  match Unix.stat (File_name.real_name_exn file) with
  | { st_kind = S_DIR; _ } -> true
  | _ -> false
;;

let rec diff_dirs_internal (config : Configuration.t) ~prev_dir ~next_dir ~file_filter =
  assert (is_dir prev_dir);
  assert (is_dir next_dir);
  let set_of_dir dir =
    (* Get a list of files for this directory only; do not descend farther
       (We recursively call diff_dirs later if we need to descend.) *)
    let file_filter =
      match file_filter with
      | None -> Fn.const true
      | Some file_filter -> file_filter
    in
    Sys_unix.ls_dir (File_name.real_name_exn dir)
    |> List.filter ~f:(fun x ->
      let x = File_name.real_name_exn dir ^/ x in
      match Unix.stat x with
      | exception Unix.Unix_error (ENOENT, _, _) ->
        (* If the file disappeared during listing, let's pretend it didn't exist.
           This is important when the file is [-exclude]d because we don't want to create
           noise for excluded files, but it's also not too bad if the file is [-include]d
        *)
        false
      | stats -> file_filter (x, stats))
    |> String.Set.of_list
  in
  let prev_set = set_of_dir prev_dir in
  let next_set = set_of_dir next_dir in
  (* Get unique files *)
  let union = Set.union prev_set next_set in
  let prev_uniques = Set.diff union next_set in
  let next_uniques = Set.diff union prev_set in
  let handle_unique which file ~dir =
    printf !"Only in %{File_name#hum}: %s\n%!" dir file;
    (* Diff unique files against /dev/null, if desired *)
    if not config.mask_uniques
    then (
      let file = File_name.append dir file in
      if is_reg file
      then (
        let diff = diff_files_internal config in
        let null = File_name.dev_null in
        match which with
        | `Prev -> ignore (diff ~prev_file:file ~next_file:null : [ `Different | `Same ])
        | `Next -> ignore (diff ~prev_file:null ~next_file:file : [ `Different | `Same ])))
  in
  Set.iter prev_uniques ~f:(handle_unique `Prev ~dir:prev_dir);
  Set.iter next_uniques ~f:(handle_unique `Next ~dir:next_dir);
  (* Get differences *)
  let inter = Set.inter prev_set next_set in
  let exit_code = ref `Same in
  let diff file =
    let prev_file = File_name.append prev_dir file in
    let next_file = File_name.append next_dir file in
    if is_reg prev_file && is_reg next_file
    then (
      let hunks = compare_files ~prev_file ~next_file config in
      if not (Comparison_result.has_no_diff hunks)
      then (
        exit_code := `Different;
        (* Print the diff if not -quiet *)
        match config.quiet with
        | false -> print hunks ~file_names:(prev_file, next_file) ~config
        | true ->
          printf
            !"Files %{File_name#hum} and %{File_name#hum} differ\n%!"
            prev_file
            next_file))
    else if is_dir prev_file && is_dir next_file
    then
      if not config.shallow
      then (
        match
          diff_dirs_internal ~prev_dir:prev_file ~next_dir:next_file config ~file_filter
        with
        | `Same -> ()
        | `Different -> exit_code := `Different)
      else
        printf
          !"Common subdirectories: %{File_name#hum} and %{File_name#hum}\n%!"
          prev_file
          next_file
    else (
      exit_code := `Different;
      printf
        !"Files %{File_name#hum} and %{File_name#hum} are not the same type\n%!"
        prev_file
        next_file)
  in
  Set.iter inter ~f:diff;
  if Set.is_empty prev_uniques && Set.is_empty next_uniques
  then !exit_code
  else `Different
;;

let diff_dirs (config : Configuration.t) ~prev_dir ~next_dir ~file_filter =
  let prev_dir, next_dir = with_alt config ~prev:prev_dir ~next:next_dir in
  if not (is_dir prev_dir)
  then
    invalid_argf !"diff_dirs: prev_dir '%{File_name#hum}' is not a directory" prev_dir ();
  if not (is_dir next_dir)
  then
    invalid_argf !"diff_dirs: next_dir '%{File_name#hum}' is not a directory" next_dir ();
  diff_dirs_internal config ~prev_dir ~next_dir ~file_filter
;;
