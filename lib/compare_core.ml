open Core.Std
open Core_extended.Std

module Patience_diff = Patience_diff_lib.Std.Patience_diff

TEST_UNIT =
  <:test_result< string list >> (String.split ~on:'\n' "") ~expect:[""]
;;

(* Open file, send lines to an array *)
let array_of_file file =
  In_channel.with_file file ~f:(fun in_channel ->
    let contents = In_channel.input_all in_channel in
    (* [String.split ~on:'\n' contents] returns [""] which is incorrect *)
    if String.is_empty contents
    then [||]
    else
      let ar = Array.of_list (String.split ~on:'\n' contents) in
      (* Strip the trailing line resulting from the last \n in every Unix file *)
      let j = (Array.length ar) - 1 in
      if ar.(j) = "" then
        Array.slice ar 0 j
      else begin
        (* All unix files should have a trailing \n! *)
        eprintf "No newline at the end of %s\n%!" file;
        ar
      end)
;;

(* Returns a Hunk.t list, ready to be printed *)
let compare_lines config ~mine ~other =
  let module C = Configuration in
  (* Create the diff *)
  let context = config.C.context in
  let keep_ws = config.C.keep_ws in
  let split_long_lines = config.C.split_long_lines in
  (* Use external compare program? *)
  let compare = Option.value_map config.C.ext_cmp
    ~default:String.compare
    ~f:(fun prog -> fun x y ->
      let cmd = sprintf "%s %S %S" prog x y in
      match Unix.system cmd with
      | Ok () -> 0
      | Error (`Exit_non_zero 1) -> 1
      | Error _ ->
          failwithf "External compare %S failed!" prog ())
  in
  let hunks = Patdiff_core.diff ~mine ~other ~context ~compare ~keep_ws in
  (* Refine if desired *)
  if config.C.unrefined then
    (* Turn `Replace ranges into `Old and `New ranges.
       `Replace's would otherwise be later interpreted as refined output *)
    Patience_diff.unified hunks
  else
    let rules = config.C.rules in
    let output = config.C.output in
    let produce_unified_lines = config.C.produce_unified_lines in
    Patdiff_core.refine ~rules ~output ~keep_ws
      ~produce_unified_lines ~split_long_lines hunks


(* Returns a Hunk.t list, ready to be printed *)
let compare_files config ~old_file ~new_file =
  let mine = array_of_file old_file in
  let other = array_of_file new_file in
  compare_lines config ~mine ~other

(* Print hunks to stdout *)
let print hunks ~old_file ~new_file ~config =
  let module C = Configuration in
  if Patience_diff.all_same hunks then begin
    if config.C.double_check then
      match Unix.system (sprintf "cmp -s %s %s" old_file new_file) with
      | Ok () -> ()
      | Error (`Exit_non_zero 1) -> printf "\
There are no differences except those filtered by your settings\n%!"
      | Error _ -> ()
  end
  else begin
    (* Only print if -quiet is not set *)
    if not config.C.quiet then
      let output = config.C.output in
      let rules = config.C.rules in
      (* Substitute old/new_alt for the filenames in the final output *)
      let old_file = Option.value ~default:old_file config.C.old_alt in
      let new_file = Option.value ~default:new_file config.C.new_alt in
      Patdiff_core.print hunks ~old_file ~new_file ~output ~rules
  end

let diff_files config ~old_file ~new_file =
  let hunks = compare_files ~old_file ~new_file config in
  print hunks ~old_file ~new_file ~config;
  if Patience_diff.all_same hunks then `Same else `Different

let diff_strings config ~old ~new_ =
  let module C = Configuration in
  let lines str = String.split_lines str |> Array.of_list in
  let hunks =
    compare_lines config ~mine:(lines old) ~other:(lines new_)
  in
  if Patience_diff.all_same hunks then `Same
  else begin
    let output = config.C.output in
    let rules = config.C.rules in
    let diff = Patdiff_core.output_to_string hunks ~output ~rules in
    `Different diff
  end

(* True if a file is a regular file *)
let is_reg path = (Unix.stat path).Unix.st_kind = Unix.S_REG
let is_dir path = (Unix.stat path).Unix.st_kind = Unix.S_DIR

let rec diff_dirs config ~old_file ~new_file ~file_filter =
  let module C = Configuration in
  (* Get a list of files for this directory only; do not descend farther
     (We recursively call diff_dirs later if we need to descend.) *)
  let options = { Find.Options.default with
    Find.Options.max_depth = Some 1;
    filter = file_filter
  } in
  let set_of_file file =
    let files = Find.find_all ~options file in
    let f = fun x -> let (n, _s) = x in Filename.make_relative ~to_:file n in
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
    if not config.C.mask_uniques then
      let path = (dir ^/ file) in
      if is_reg path then
        let diff = diff_files config in
        let null = "/dev/null" in
        if is_old then ignore (diff ~old_file:path ~new_file:null)
        else ignore (diff ~old_file:null ~new_file:path) in
  Set.iter old_uniques ~f:(handle_unique ~dir:old_file ~is_old:true);
  Set.iter new_uniques ~f:(handle_unique ~dir:new_file ~is_old:false);
  (* Get differences *)
  let inter = Set.inter old_set new_set in
  let exit_code = ref `Same in
  let diff file =
    let old_file = old_file ^/ file in
    let new_file = new_file ^/ file in
    if is_reg old_file && is_reg new_file then begin
      let hunks = compare_files ~old_file ~new_file config in
      if not (Patience_diff.all_same hunks) then  begin
        exit_code := `Different;
          (* Print the diff if not -quiet *)
        if config.C.quiet = false then
          print hunks ~old_file ~new_file ~config
        else
          printf "Files %s and %s differ\n%!" old_file new_file
      end
    end
    else if is_dir old_file && is_dir new_file then begin
      if not config.C.shallow then
        match diff_dirs ~old_file ~new_file config ~file_filter with
      | `Same -> ()
      | `Different -> exit_code := `Different
      else printf "Common subdirectories: %s and %s\n%!" old_file new_file
    end
    else begin
      exit_code := `Different;
      printf "Files %s and %s are not the same type\n%!" old_file new_file
    end
  in
  Set.iter inter ~f:(diff);
  if Set.is_empty old_uniques && Set.is_empty new_uniques then
    !exit_code
  else
    `Different
