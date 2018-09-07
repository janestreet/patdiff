open Core
open Import

let lines_of_contents contents =
  let lines = Array.of_list (String.split_lines contents) in
  let has_trailing_newline =
    let length = String.length contents in
    if length = 0 || Char.equal contents.[length - 1] '\n'
    then `With_trailing_newline
    else `Missing_trailing_newline
  in
  lines, has_trailing_newline
;;

let%test_unit _ =
  let test contents ~expect =
    [%test_result: string array * [`With_trailing_newline | `Missing_trailing_newline]]
      (lines_of_contents contents)
      ~expect
  in
  test "" ~expect:([||], `With_trailing_newline);
  test "hello" ~expect:([| "hello" |], `Missing_trailing_newline);
  test "hello\nworld" ~expect:([| "hello"; "world" |], `Missing_trailing_newline);
  test "hello\nworld\n" ~expect:([| "hello"; "world" |], `With_trailing_newline)
;;

(* Returns a Hunk.t list, ready to be printed *)
let compare_lines config ~mine ~other =
  let module C = Configuration in
  (* Create the diff *)
  let context = config.C.context in
  let keep_ws = config.C.keep_ws in
  let split_long_lines = config.C.split_long_lines in
  let line_big_enough = config.C.line_big_enough in
  let hunks =
    let transform = if keep_ws then Fn.id else Patdiff_core.remove_ws in
    (* Use external compare program? *)
    match config.C.ext_cmp with
    | None ->
      Patience_diff.String.get_hunks
        ~transform
        ~context
        ~big_enough:line_big_enough
        ~mine
        ~other
    | Some prog ->
      let compare x y =
        let cmd = sprintf "%s %S %S" prog x y in
        match Unix.system cmd with
        | Ok () -> 0
        | Error (`Exit_non_zero 1) -> 1
        | Error _ -> failwithf "External compare %S failed!" prog ()
      in
      let module P = Patience_diff.Make (struct
                       type t = string [@@deriving sexp]

                       let hash = String.hash
                       let compare = compare
                     end)
      in
      P.get_hunks ~transform ~context ~big_enough:line_big_enough ~mine ~other
  in
  let hunks =
    match config.C.float_tolerance with
    | None -> hunks
    | Some tolerance -> Float_tolerance.apply hunks tolerance ~context
  in
  (* Refine if desired *)
  if config.C.unrefined
  then
    (* Turn `Replace ranges into `Old and `New ranges.
       `Replace's would otherwise be later interpreted as refined output *)
    `Hunks (Patience_diff.Hunks.unified hunks)
  else (
    let rules = config.C.rules in
    let output = config.C.output in
    let produce_unified_lines = config.C.produce_unified_lines in
    let interleave = config.C.interleave in
    let word_big_enough = config.C.word_big_enough in
    `Hunks
      (Patdiff_core.refine
         ~rules
         ~output
         ~keep_ws
         ~produce_unified_lines
         ~split_long_lines
         ~interleave
         hunks
         ~word_big_enough))
;;

let warn_if_no_trailing_newline
      ~warn_if_no_trailing_newline_in_both
      (old_file_newline, old_file)
      (new_file_newline, new_file)
  =
  let warn = eprintf "No newline at the end of %s\n%!" in
  match old_file_newline, new_file_newline with
  | `With_trailing_newline, `With_trailing_newline -> ()
  | `With_trailing_newline, `Missing_trailing_newline -> warn new_file
  | `Missing_trailing_newline, `With_trailing_newline -> warn old_file
  | `Missing_trailing_newline, `Missing_trailing_newline ->
    if warn_if_no_trailing_newline_in_both
    then (
      warn old_file;
      warn new_file)
;;

(* Returns a Hunk.t list, ready to be printed *)
let compare_files (config : Configuration.t) ~old_file ~new_file =
  let mine = In_channel.read_all old_file in
  let other = In_channel.read_all new_file in
  let mine_is_binary, other_is_binary =
    match config.assume_text with
    | true -> false, false
    | false -> Is_binary.string mine, Is_binary.string other
  in
  if mine_is_binary || other_is_binary
  then
    if String.( = ) mine other
    then `Binary_same
    else `Binary_different (mine_is_binary, other_is_binary)
  else (
    let mine, old_file_newline = lines_of_contents mine in
    let other, new_file_newline = lines_of_contents other in
    warn_if_no_trailing_newline
      (old_file_newline, old_file)
      (new_file_newline, new_file)
      ~warn_if_no_trailing_newline_in_both:config.warn_if_no_trailing_newline_in_both;
    compare_lines config ~mine ~other)
;;

let has_no_diff hunks =
  match hunks with
  | `Binary_same -> true
  | `Binary_different _ -> false
  | `Hunks hunks -> List.for_all hunks ~f:Patience_diff.Hunk.all_same
;;

let binary_different_message
      ~(config : Configuration.t)
      ~old_file
      ~mine_is_binary
      ~new_file
      ~other_is_binary
  =
  match config.location_style with
  | Diff ->
    sprintf
      "Files %s%s and %s%s differ"
      old_file
      (if mine_is_binary then " (binary)" else "")
      new_file
      (if other_is_binary then " (binary)" else "")
  | Omake ->
    String.concat
      [ error_message_start ~file:old_file ~line:1
      ; "\n"
      ; "  File \""
      ; new_file
      ; "\"\n"
      ; "  binary files differ\n"
      ]
;;

(* Print hunks to stdout *)
let print hunks ~old_file ~new_file ~config =
  let module C = Configuration in
  if has_no_diff hunks
  then (
    if config.C.double_check
    then (
      match Unix.system (sprintf "cmp -s %s %s" old_file new_file) with
      | Ok () -> ()
      | Error (`Exit_non_zero 1) ->
        printf "There are no differences except those filtered by your settings\n%!"
      | Error _ -> ()))
  else if (* Only print if -quiet is not set *)
    not config.C.quiet
  then (
    let output = config.C.output in
    let rules = config.C.rules in
    (* Substitute old/new_alt for the filenames in the final output *)
    let old_file = Option.value ~default:old_file config.C.old_alt in
    let new_file = Option.value ~default:new_file config.C.new_alt in
    match hunks with
    | `Binary_same -> assert false
    | `Binary_different (mine_is_binary, other_is_binary) ->
      Printf.printf
        "%s\n"
        (binary_different_message
           ~config
           ~old_file
           ~mine_is_binary
           ~new_file
           ~other_is_binary)
    | `Hunks hunks ->
      Patdiff_core.print
        hunks
        ~old_file
        ~new_file
        ~output
        ~rules
        ~location_style:config.location_style)
;;

let diff_files config ~old_file ~new_file =
  let hunks = compare_files ~old_file ~new_file config in
  print hunks ~old_file ~new_file ~config;
  if has_no_diff hunks then `Same else `Different
;;

let diff_strings ?print_global_header (config : Configuration.t) ~old ~new_ =
  let lines { Patdiff_core.name = _; text } = String.split_lines text |> Array.of_list in
  let hunks = compare_lines config ~mine:(lines old) ~other:(lines new_) in
  if has_no_diff hunks
  then `Same
  else
    `Different
      (match hunks with
       | `Binary_same -> assert false
       | `Binary_different (mine_is_binary, other_is_binary) ->
         binary_different_message
           ~config
           ~old_file:old.name
           ~mine_is_binary
           ~new_file:new_.name
           ~other_is_binary
       | `Hunks hunks ->
         Patdiff_core.output_to_string
           hunks
           ?print_global_header
           ~file_names:(old.name, new_.name)
           ~output:config.output
           ~rules:config.rules
           ~location_style:config.location_style)
;;

(* True if a file is a regular file *)
let is_reg path = (Unix.stat path).Unix.st_kind = Unix.S_REG
let is_dir path = (Unix.stat path).Unix.st_kind = Unix.S_DIR

let rec diff_dirs config ~old_file ~new_file ~file_filter =
  let module C = Configuration in
  (* Get a list of files for this directory only; do not descend farther
     (We recursively call diff_dirs later if we need to descend.) *)
  let options =
    { Find_files.Options.default with max_depth = Some 1; filter = file_filter }
  in
  let set_of_file file =
    let files = Find_files.find_all ~options file in
    let f (n, _s) = Filename_extended.make_relative ~to_:file n in
    let names = List.map files ~f in
    String.Set.of_list names
  in
  let old_set = set_of_file old_file in
  let new_set = set_of_file new_file in
  (* Get unique files *)
  let union = Set.union old_set new_set in
  let old_uniques = Set.diff union new_set in
  let new_uniques = Set.diff union old_set in
  let handle_unique file ~dir ~is_old =
    printf "Only in %s: %s\n%!" dir file;
    (* Diff unique files against /dev/null, if desired *)
    if not config.C.mask_uniques
    then (
      let path = dir ^/ file in
      if is_reg path
      then (
        let diff = diff_files config in
        let null = "/dev/null" in
        if is_old
        then ignore (diff ~old_file:path ~new_file:null)
        else ignore (diff ~old_file:null ~new_file:path)))
  in
  Set.iter old_uniques ~f:(handle_unique ~dir:old_file ~is_old:true);
  Set.iter new_uniques ~f:(handle_unique ~dir:new_file ~is_old:false);
  (* Get differences *)
  let inter = Set.inter old_set new_set in
  let exit_code = ref `Same in
  let diff file =
    let old_file = old_file ^/ file in
    let new_file = new_file ^/ file in
    if is_reg old_file && is_reg new_file
    then (
      let hunks = compare_files ~old_file ~new_file config in
      if not (has_no_diff hunks)
      then (
        exit_code := `Different;
        (* Print the diff if not -quiet *)
        if config.C.quiet = false
        then print hunks ~old_file ~new_file ~config
        else printf "Files %s and %s differ\n%!" old_file new_file))
    else if is_dir old_file && is_dir new_file
    then
      if not config.C.shallow
      then (
        match diff_dirs ~old_file ~new_file config ~file_filter with
        | `Same -> ()
        | `Different -> exit_code := `Different)
      else printf "Common subdirectories: %s and %s\n%!" old_file new_file
    else (
      exit_code := `Different;
      printf "Files %s and %s are not the same type\n%!" old_file new_file)
  in
  Set.iter inter ~f:diff;
  if Set.is_empty old_uniques && Set.is_empty new_uniques then !exit_code else `Different
;;
