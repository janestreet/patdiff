open Core
module P = Patdiff_lib.Patdiff_core
module Compare_core = Patdiff_lib.Compare_core
module Configuration = Patdiff_lib.Configuration

let summary =
  {|Compare two files (or process a diff read in on stdin) using the
patience diff algorithm.

If you don't supply any arguments to patdiff, it will read diff-like
text from stdin and color it in the normal patdiff way.

The file ~/.patdiff is used as a config file if it exists.  You can
write a sample config with the -make-config flag.|}
;;

module Args = struct
  type compare_flags =
    { unrefined_opt : bool option
    ; produce_unified_lines_opt : bool option
    ; ext_cmp_opt : string option option
    ; float_tolerance_opt : Percent.t option option
    ; keep_ws_opt : bool option
    ; interleave_opt : bool option
    ; assume_text_opt : bool option
    ; split_long_lines_opt : bool option
    ; shallow_opt : bool option
    ; quiet_opt : bool option
    ; double_check_opt : bool option
    ; mask_uniques_opt : bool option
    ; output : P.Output.t option
    ; context_opt : int option
    ; line_big_enough_opt : int option
    ; word_big_enough_opt : int option
    ; config_opt : string option
    ; old_file : string
    ; new_file : string
    ; old_alt_opt : string option option
    ; new_alt_opt : string option option
    ; include_ : string list
    ; exclude : string list
    ; location_style : P.Format.Location_style.t option
    ; warn_if_no_trailing_newline_in_both : bool option
    }

  type t =
    | Compare of compare_flags
    | Make_config of string
end

let remove_at_exit = ref []

let files_from_anons = function
  | Some (old_file, new_file) -> old_file, new_file
  | None ->
    (* read from stdin *)
    let temp_txt_file prefix =
      let file, oc = Filename.open_temp_file prefix ".txt" in
      remove_at_exit := file :: !remove_at_exit;
      file, oc
    in
    let old_file, old_oc = temp_txt_file "patdiff_old_" in
    let new_file, new_oc = temp_txt_file "patdiff_new_" in
    let add_prefixes = [ "+"; ">" ] in
    let remove_prefixes = [ "-"; "<" ] in
    let begins_with line prefixes =
      List.exists prefixes ~f:(fun prefix -> String.is_prefix line ~prefix)
    in
    let maybe_remove line prefixes =
      let begins_with =
        List.find prefixes ~f:(fun prefix -> String.is_prefix line ~prefix)
      in
      match begins_with with
      | None -> line
      | Some prefix -> " " ^ String.chop_prefix_exn line ~prefix
    in
    let process line begins_with_prefixes maybe_remove_prefixes oc =
      if not (begins_with line begins_with_prefixes)
      then (
        Out_channel.output_string oc (maybe_remove line maybe_remove_prefixes);
        Out_channel.newline oc)
    in
    In_channel.iter_lines In_channel.stdin ~f:(fun line ->
      process line add_prefixes remove_prefixes old_oc;
      process line remove_prefixes add_prefixes new_oc);
    Out_channel.close old_oc;
    Out_channel.close new_oc;
    old_file, new_file
;;

(* Override default/config file options with command line arguments *)
let override config (args : Args.compare_flags) =
  Configuration.override
    config
    ?output:args.output
    ?unrefined:args.unrefined_opt
    ?produce_unified_lines:args.produce_unified_lines_opt
    ?ext_cmp:args.ext_cmp_opt
    ?float_tolerance:args.float_tolerance_opt
    ?keep_ws:args.keep_ws_opt
    ?split_long_lines:args.split_long_lines_opt
    ?interleave:args.interleave_opt
    ?assume_text:args.assume_text_opt
    ?context:args.context_opt
    ?line_big_enough:args.line_big_enough_opt
    ?word_big_enough:args.word_big_enough_opt
    ?shallow:args.shallow_opt
    ?quiet:args.quiet_opt
    ?double_check:args.double_check_opt
    ?mask_uniques:args.mask_uniques_opt
    ?old_alt:args.old_alt_opt
    ?new_alt:args.new_alt_opt
    ?location_style:args.location_style
    ?warn_if_no_trailing_newline_in_both:args.warn_if_no_trailing_newline_in_both
;;

let main' args =
  let module A = Args in
  (* Load config file if it exists, use default if not *)
  let config = Configuration.get_config ?filename:args.A.config_opt () in
  let config = override config args in
  (* 2012-06-28 mbac: /dev/null is used as a placeholder for deleted files. *)
  let file_or_dev_null f = if Sys.file_exists_exn f then f else "/dev/null" in
  let old_file = file_or_dev_null args.A.old_file in
  let new_file = file_or_dev_null args.A.new_file in
  if old_file = "/dev/null" && new_file = "/dev/null"
  then failwithf "Both files, %s and %s, do not exist" args.A.old_file args.A.new_file ();
  let is_dir = Sys.is_directory_exn in
  let if_not_diffing_two_dirs () =
    if args.A.include_ <> [] || args.A.exclude <> []
    then failwith "Can only specify -include or -exclude when diffing two dirs"
  in
  match is_dir old_file, is_dir new_file with
  | true, false
  | false, true ->
    if_not_diffing_two_dirs ();
    (* One is a directory, the other is a file *)
    let dir, file = if is_dir old_file then old_file, new_file else new_file, old_file in
    (* Match file with its twin file in dir *)
    let module FO = Core_extended.Find.Options in
    let filter (filename, _stats) = filename = file in
    let options =
      { FO.min_depth = 1
      ; max_depth = Some 1
      ; follow_links = FO.default.FO.follow_links
      ; on_open_errors = FO.default.FO.on_open_errors
      ; on_stat_errors = FO.default.FO.on_stat_errors
      ; filter = Some filter
      ; skip_dir = FO.default.FO.skip_dir
      ; relative_paths = false
      }
    in
    let matches = Core_extended.Find.find_all ~options dir in
    (match matches with
     | [ (matched_filename, _stats) ] ->
       let old_file, new_file =
         if is_dir old_file then matched_filename, file else file, matched_filename
       in
       Compare_core.diff_files ~old_file ~new_file config
     | [] -> failwithf "File not found in %s: %s" dir file ()
     | _ ->
       (* This is impossible because max_depth is 1 *)
       failwithf "Directory contains clones: %s" dir ())
  | true, true ->
    (* Both are directories *)
    let file_filter =
      if args.A.include_ = [] && args.A.exclude = []
      then None
      else
        Some
          (fun (s, stat) ->
             match stat.Unix.st_kind with
             | Unix.S_REG ->
               List.for_all args.A.exclude ~f:(fun pat -> not (Pcre.pmatch s ~pat))
               && (args.A.include_ = []
                   || List.exists args.A.include_ ~f:(fun pat -> Pcre.pmatch s ~pat))
             | _ -> true)
    in
    Compare_core.diff_dirs ~old_file ~new_file config ~file_filter
  | false, false ->
    (* Both are files *)
    if_not_diffing_two_dirs ();
    Compare_core.diff_files ~old_file ~new_file config
;;

let main arg =
  match arg with
  | Args.Make_config file -> Make_config.main file
  | Args.Compare compare_args ->
    let res = main' compare_args in
    List.iter !remove_at_exit ~f:(fun file ->
      try Unix.unlink file with
      | _ -> ());
    (match res with
     | `Same -> exit 0
     | `Different -> exit 1)
;;

let command =
  let flag_no_arg ?(inverted = false) name ~doc =
    let open Command.Param in
    map ~f:(fun b -> if b then Some (not inverted) else None) (flag name no_arg ~doc)
  in
  let specified_more_than_once s = failwithf "%s specified more than once" s () in
  let open Command.Let_syntax in
  Command.basic
    ~summary
    ~readme:(fun () -> Readme.doc)
    (let%map_open config_opt =
       let%map default =
         flag
           "default"
           no_arg
           ~doc:" Use the default configuration instead of ~/.patdiff"
       and file =
         flag
           "file"
           (optional Arg_type.Export.file)
           ~doc:"FILE Use FILE as configuration file instead of ~/.patdiff"
       in
       match file, default with
       | Some _, true -> specified_more_than_once "config"
       | None, true -> Some ""
       | _, false -> file
     and context_opt =
       flag
         "context"
         (optional int)
         ~doc:"NUM Show lines of unchanged context before and after changes"
     and line_big_enough_opt, word_big_enough_opt =
       let%map line_big_enough =
         flag
           "line-big-enough"
           (optional int)
           ~doc:
             "NUM Limit line-level semantic cleanup to the matches of length less than \
              NUM lines"
       and no_semantic_cleanup =
         flag
           "no-semantic-cleanup"
           no_arg
           ~doc:" Don't do any semantic cleanup; let small, spurious matches survive"
       and word_big_enough =
         flag
           "word-big-enough"
           (optional int)
           ~doc:
             "NUM Limit word-level semantic cleanup to the matches of length less than \
              NUM words"
       in
       let f name value =
         match value, no_semantic_cleanup with
         | Some _, true -> specified_more_than_once name
         | None, true -> Some 1
         | _, false -> value
       in
       f "line-big-enough" line_big_enough, f "word-big-enough" word_big_enough
     and ext_cmp_opt, unrefined_opt =
       let%map ext_cmp =
         flag
           "ext-cmp"
           (optional Arg_type.Export.file)
           ~doc:"FILE Use external string comparison program (implies -unrefined)"
       and unrefined =
         flag "unrefined" no_arg ~doc:" Don't highlight word differences between lines"
       in
       let unrefined_opt =
         match ext_cmp, unrefined with
         | Some _, true ->
           (* unrefined is set more than once, but both values agree, so it's fine. *)
           Some true
         | Some _, false
         | None, true -> (* only set once *)
           Some true
         | None, false -> (* never set. *)
           None
       in
       let ext_cmp_opt = Option.map ~f:Option.some ext_cmp in
       ext_cmp_opt, unrefined_opt
     and keep_ws_opt =
       flag_no_arg "keep-whitespace" ~doc:" Consider whitespace when comparing lines"
     and split_long_lines_opt =
       flag_no_arg
         "split-long-lines"
         ~doc:" Split long lines into multiple displayed lines"
     and interleave_opt =
       flag_no_arg
         ~inverted:true
         "no-interleave"
         ~doc:" Don't attempt to split up large hunks near equalities"
     and assume_text_opt =
       flag_no_arg
         "text"
         ~doc:" Treat all files as text (i.e. display diffs of binary file contents)"
     and output =
       choose_one
         [ map
             ~f:(function
               | true -> Some (Some P.Output.Html)
               | _ -> None)
             (flag
                "html"
                no_arg
                ~doc:
                  " Output in HTML format instead of default (ASCII with ANSI escape \
                   codes)")
         ; map
             ~f:(function
               | true -> Some (Some P.Output.Ascii)
               | _ -> None)
             (flag
                "ascii"
                no_arg
                ~doc:" Output in ASCII with no ANSI escape codes (implies -unrefined)")
         ; map
             ~f:(function
               | true -> Some (Some P.Output.Ansi)
               | _ -> None)
             (flag "ansi" no_arg ~doc:" Output in ASCII with ANSI escape codes")
         ]
         ~if_nothing_chosen:(`Default_to None)
     and produce_unified_lines_opt =
       flag_no_arg
         ~inverted:true
         "dont-produce-unified-lines"
         ~doc:" Don't produce unified lines"
     and quiet_opt =
       flag_no_arg "quiet" ~doc:" Report only whether files differ, not the details"
     and shallow_opt =
       flag_no_arg
         "shallow"
         ~doc:" When comparing directories, don't recurse into subdirs"
     and double_check_opt =
       flag_no_arg "double-check" ~doc:" If files seem identical, double check with cmp"
     and mask_uniques_opt =
       flag_no_arg
         "mask-uniques"
         ~doc:" When comparing directories, don't compare against /dev/null"
     and float_tolerance_opt =
       flag
         "float-tolerance"
         (optional (Arg_type.map percent ~f:Option.some))
         ~doc:
           "PERCENT Consider strings equal if only difference is floats changing within \
            PERCENT"
     and old_alt_opt =
       flag
         "alt-old"
         (optional (Arg_type.map Arg_type.Export.file ~f:Option.some))
         ~doc:"NAME Mask old filename with NAME"
     and new_alt_opt =
       flag
         "alt-new"
         (optional (Arg_type.map Arg_type.Export.file ~f:Option.some))
         ~doc:"NAME Mask new filename with NAME"
     and make_config =
       flag "make-config" (optional Arg_type.Export.file) ~doc:Make_config.doc
     and include_ =
       flag
         "include"
         (listed string)
         ~doc:"REGEXP include files matching this pattern when comparing two directories"
     and exclude =
       flag
         "exclude"
         (listed string)
         ~doc:
           "REGEXP exclude files matching this pattern when comparing two directories \
            (overrides include patterns)"
     and reverse = flag "reverse" no_arg ~doc:" produce a diff that undoes the changes"
     and location_style =
       flag
         "location-style"
         (optional (Arg_type.create P.Format.Location_style.of_string))
         ~doc:
           P.Format.Location_style.(
             sprintf
               "<%s> how to format location information in hunk headers"
               (String.concat ~sep:"|" (List.map all ~f:to_string)))
     and warn_if_no_trailing_newline_in_both =
       flag
         "warn-if-no-trailing-newline-in-both"
         (optional bool)
         ~doc:
           (sprintf
              "BOOL warn when neither file ends in a newline, even though this does not \
               constitute a diff (default: read from config, or %b)"
              Configuration.warn_if_no_trailing_newline_in_both_default)
     and files =
       anon (maybe (t2 ("FILE1" %: Arg_type.Export.file) ("FILE2" %: Arg_type.Export.file)))
     in
     fun () ->
       let args =
         match make_config with
         | Some file -> Args.Make_config file
         | None ->
           let old_file, new_file =
             let pair = files_from_anons files in
             if reverse then Tuple.T2.swap pair else pair
           in
           Args.Compare
             { config_opt
             ; context_opt
             ; line_big_enough_opt
             ; word_big_enough_opt
             ; unrefined_opt
             ; keep_ws_opt
             ; split_long_lines_opt
             ; interleave_opt
             ; assume_text_opt
             ; output
             ; produce_unified_lines_opt
             ; quiet_opt
             ; shallow_opt
             ; double_check_opt
             ; mask_uniques_opt
             ; ext_cmp_opt
             ; float_tolerance_opt
             ; old_alt_opt
             ; new_alt_opt
             ; include_
             ; exclude
             ; location_style
             ; warn_if_no_trailing_newline_in_both
             ; old_file
             ; new_file
             }
       in
       main args)
;;

