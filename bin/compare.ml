open Core.Std
open Core_extended.Std
module D = Patience_diff_lib.Std.Patience_diff
module P = Patdiff_lib.Patdiff_core
module Compare_core = Patdiff_lib.Compare_core
module Configuration = Patdiff_lib.Configuration

let summary = "\
Compare two files (or process a diff read in on stdin) using the
patience diff algorithm.

If you don't supply any arguments to patdiff, it will read diff-like
text from stdin and color it in the normal patdiff way.

The file ~/.patdiff is used as a config file if it exists.  You can
write a sample config with the -make-config flag."

let usage_arg = "[FILE1 FILE2] [OPTIONS]"


module Accum = struct

  (* These are all options because they override the config file if specified,
     so we want to know if they were actually given on the command line. *)
  type t = {
    config_opt : string option ref;
    context_opt : int option ref;
    unrefined_opt : bool option ref;
    keep_ws_opt : bool option ref;
    split_long_lines_opt : bool option ref;
    html_opt : bool option ref;
    produce_unified_lines_opt : bool option ref;
    quiet_opt : bool option ref;
    shallow_opt : bool option ref;
    double_check_opt : bool option ref;
    mask_uniques_opt : bool option ref;
    ext_cmp_opt : string option option ref;
    old_alt_opt : string option option ref;
    new_alt_opt : string option option ref;
    make_config_file : string option ref;
    do_readme : bool option ref;
    include_ : string list ref;
    exclude : string list ref;
    reverse : bool option ref;
  }

  let empty = {
    config_opt = ref None;
    context_opt = ref None;
    unrefined_opt = ref None;
    keep_ws_opt = ref None;
    split_long_lines_opt = ref None;
    html_opt = ref None;
    produce_unified_lines_opt = ref None;
    quiet_opt = ref None;
    shallow_opt = ref None;
    double_check_opt = ref None;
    mask_uniques_opt = ref None;
    ext_cmp_opt = ref None;
    old_alt_opt = ref None;
    new_alt_opt = ref None;
    make_config_file = ref None;
    do_readme = ref None;
    include_ = ref [];
    exclude = ref [];
    reverse = ref None;
  }

end

let init () = Accum.empty

(* Helper for one-shot field updation used by command dispatches *)
let set_once name r v =
  match !r with
  | Some _ -> failwithf "%s specified more than once" name ()
  | None -> r := Some v
;;

let flags =
  let module Cf = Deprecated_command.Flag in
  [
    Cf.arg_mut "-file"
      (fun t s -> set_once "config" t.Accum.config_opt s)
      ~doc: "FILE Use FILE as configuration file instead of ~/.patdiff";
    Cf.noarg_mut "-default"
      (fun t -> set_once "config" t.Accum.config_opt "")
      ~doc: " Use the default configuration instead of ~/.patdiff";
    Cf.arg_mut "-context"
      (fun t i -> set_once "context" t.Accum.context_opt (int_of_string i))
      ~doc: "NUM Show lines of unchanged context before and after changes";
    Cf.noarg_mut "-unrefined"
      (fun t -> set_once "unrefined" t.Accum.unrefined_opt true)
      ~doc: " Don't highlight word differences between lines";
    Cf.noarg_mut "-keep-whitespace"
      (fun t -> set_once "keep-whitespace" t.Accum.keep_ws_opt true)
          ~doc: " Consider whitespace when comparing lines";
    Cf.noarg_mut "-split-long-lines"
      (fun t -> set_once "split-long-lines" t.Accum.split_long_lines_opt true)
      ~doc: " Split long lines into multiple displayed lines";
    Cf.noarg_mut "-html"
      (fun t -> set_once "html" t.Accum.html_opt true)
      ~doc: " Output in HTML format instead of default";
    Cf.noarg_mut "-dont-produce-unified-lines"
      (fun t -> set_once "word unify" t.Accum.produce_unified_lines_opt false)
      ~doc: " Don't produce unified lines";
    Cf.noarg_mut "-quiet"
      (fun t -> set_once "quiet" t.Accum.quiet_opt true)
      ~doc: " Report only whether files differ, not the details";
    Cf.noarg_mut "-shallow"
      (fun t -> set_once "shallow" t.Accum.shallow_opt true)
      ~doc: " When comparing directories, don't recurse into subdirs";
    Cf.noarg_mut "-double-check"
      (fun t -> set_once "double_check" t.Accum.double_check_opt true)
      ~doc: " If files seem identical, double check with cmp";
    Cf.noarg_mut "-mask-uniques"
      (fun t -> set_once "mask_uniques" t.Accum.mask_uniques_opt true)
      ~doc: " When comparing directories, don't compare against /dev/null";
    Cf.arg_mut "-ext-cmp"
      (fun t s ->
        set_once "external compare" t.Accum.ext_cmp_opt (Some s);
        set_once "unrefined" t.Accum.unrefined_opt true)
      ~doc: "FILE Use external string comparison program (implies -unrefined)";
    Cf.arg_mut "-alt-old"
      (fun t s -> set_once "alternate old" t.Accum.old_alt_opt (Some s))
      ~doc: "NAME Mask old filename with NAME";
    Cf.arg_mut "-alt-new"
      (fun t s -> set_once "alternate new" t.Accum.new_alt_opt (Some s))
      ~doc: "NAME Mask new filename with NAME";
    Cf.arg_mut "-make-config"
      (fun t s -> set_once "config" t.Accum.make_config_file s)
      ~doc:Make_config.doc;
    Cf.noarg_mut "-readme"
      (fun t -> set_once "readme" t.Accum.do_readme true)
      ~doc:Readme.doc;
    Cf.arg_mut "-include"
      (fun t s -> t.Accum.include_ := s :: !(t.Accum.include_))
      ~doc:"REGEXP include files matching this pattern when comparing two directories";
    Cf.arg_mut "-exclude"
      (fun t s -> t.Accum.exclude := s :: !(t.Accum.exclude))
      ~doc:"REGEXP exclude files matching this pattern when comparing two directories \
            (overrides include patterns)";
    Cf.noarg_mut "-reverse"
      (fun t -> set_once "reverse" t.Accum.reverse true)
      ~doc:" produce a diff that undoes the changes";
  ]
;;

module Args = struct
  type compare_flags = {
    unrefined_opt : bool option;
    produce_unified_lines_opt : bool option;
    ext_cmp_opt : string option option;
    keep_ws_opt : bool option;
    split_long_lines_opt: bool option;
    shallow_opt : bool option;
    quiet_opt : bool option;
    double_check_opt : bool option;
    mask_uniques_opt : bool option;
    html_opt : bool option;
    context_opt : int option;
    config_opt : string  option;
    old_file : string;
    new_file : string;
    old_alt_opt : string option option;
    new_alt_opt : string option option;
    include_ : string list;
    exclude : string list;
  }

  type t =
    | Compare of compare_flags
    | Make_config of string
    | Print_readme

end

let remove_at_exit = ref []

(* XXX : These need to work with the new flags *)
let final t anon =
  match !(t.Accum.make_config_file) with
  | Some config_file -> Args.Make_config config_file
  | None ->
    match !(t.Accum.do_readme) with
    | Some true -> Args.Print_readme
    | _ ->
      let args ~old_file ~new_file =
        let old_file, new_file =
          match !(t.Accum.reverse) with
          | Some true -> new_file, old_file
          | None | Some false -> old_file, new_file
        in
        Args.Compare
          { Args.
            old_file = old_file;
            new_file = new_file;
            unrefined_opt = !(t.Accum.unrefined_opt);
            produce_unified_lines_opt = !(t.Accum.produce_unified_lines_opt);
            ext_cmp_opt = !(t.Accum.ext_cmp_opt);
            keep_ws_opt = !(t.Accum.keep_ws_opt);
            split_long_lines_opt = !(t.Accum.split_long_lines_opt);
            shallow_opt = !(t.Accum.shallow_opt);
            quiet_opt = !(t.Accum.quiet_opt);
            double_check_opt = !(t.Accum.double_check_opt);
            mask_uniques_opt = !(t.Accum.mask_uniques_opt);
            html_opt = !(t.Accum.html_opt);
            context_opt = !(t.Accum.context_opt);
            config_opt = !(t.Accum.config_opt);
            old_alt_opt = !(t.Accum.old_alt_opt);
            new_alt_opt = !(t.Accum.new_alt_opt);
            include_ = !(t.Accum.include_);
            exclude = !(t.Accum.exclude);
          }
      in
      match anon with
      | [old_file; new_file] -> args ~old_file ~new_file
      | [] ->
        (* read from stdin *)
        let temp_txt_file prefix =
          let file, oc = Filename.open_temp_file prefix ".txt" in
          remove_at_exit := file :: !remove_at_exit;
          file, oc
        in
        let old_file, old_oc = temp_txt_file "patdiff_old_" in
        let new_file, new_oc = temp_txt_file "patdiff_new_" in
        let add_prefixes = ["+";">"] in
        let remove_prefixes = ["-";"<"] in
        let begins_with line prefixes = List.exists prefixes ~f:(fun prefix ->
          String.is_prefix line ~prefix
        )
        in
        let maybe_remove line prefixes =
          let begins_with = List.find prefixes ~f:(fun prefix ->
            String.is_prefix line ~prefix
          )
          in
          match begins_with with
          | None -> line
          | Some prefix -> " " ^ String.chop_prefix_exn line ~prefix
        in
        let process line begins_with_prefixes maybe_remove_prefixes oc =
          if not (begins_with line begins_with_prefixes)
          then begin
            Out_channel.output_string oc (maybe_remove line maybe_remove_prefixes);
            Out_channel.newline oc
          end
        in
        In_channel.iter_lines In_channel.stdin ~f:(fun line ->
          process line add_prefixes remove_prefixes old_oc;
          process line remove_prefixes add_prefixes new_oc
        );
        Out_channel.close old_oc;
        Out_channel.close new_oc;
        args ~old_file ~new_file
      | _ ->
          failwithf "Please provide zero or two files to compare Usage: %s" usage_arg ()
;;

(* Override default/config file options with command line arguments *)
let override config args =
  let module A = Args in
  let module C = Configuration in
  let output =
    match args.A.html_opt with
    | None -> config.C.output
    | Some true  -> P.Output.Html
    | Some false -> P.Output.Ansi
  in
  let value default option = Option.value ~default option in
  {
    C.rules = config.C.rules;
    output = output;
    unrefined = value config.C.unrefined args.A.unrefined_opt;
    produce_unified_lines = value config.C.produce_unified_lines
      args.A.produce_unified_lines_opt;
    ext_cmp = value config.C.ext_cmp args.A.ext_cmp_opt;
    keep_ws = value config.C.keep_ws args.A.keep_ws_opt;
    split_long_lines = value config.C.split_long_lines args.A.split_long_lines_opt;
    context = value config.C.context args.A.context_opt;
    shallow = value config.C.shallow args.A.shallow_opt;
    quiet = value config.C.quiet args.A.quiet_opt;
    double_check = value config.C.double_check args.A.double_check_opt;
    mask_uniques = value config.C.mask_uniques args.A.mask_uniques_opt;
    old_alt = value config.C.old_alt args.A.old_alt_opt;
    new_alt = value config.C.new_alt args.A.new_alt_opt;
  }
;;

let main' args =
  let module A = Args in
  let module C = Configuration in
  (* Load config file if it exists, use default if not *)
  let config =
    let file =
      match args.A.config_opt with
      | Some f -> Some f (* specified file *)
      | None ->
        (* ~/.patdiff exists *)
        Option.bind (Sys.getenv "HOME") (fun home ->
          let f = home ^/ ".patdiff" in
          match Sys.file_exists f with
          | `Yes -> Some f
          | `No | `Unknown -> None)
    in
    (* C.load prints warnings to stderr. This is desired because [file] is only Some if it
       was manually specified or if ~/.patdiff exists. The user should be notified of
       errors if the file fails in both cases. *)
    match Option.bind file C.load with
    | Some c -> c
    | None -> C.parse (C.Config.t_of_sexp (Sexp.of_string Text.Configuration.ansi_config))
  in
  let config = override config args in
  (* 2012-06-28 mbac: /dev/null is used as a placeholder for deleted files. *)
  let file_or_dev_null f = if Sys.file_exists_exn f then f else "/dev/null" in
  let old_file = file_or_dev_null args.A.old_file in
  let new_file = file_or_dev_null args.A.new_file in
  if old_file = "/dev/null" && new_file = "/dev/null" then
    failwithf "Both files, %s and %s, do not exist" args.A.old_file args.A.new_file ();

  let is_dir = Sys.is_directory_exn in
  let if_not_diffing_two_dirs () =
    if args.A.include_ <> [] || args.A.exclude <> []
    then failwith "Can only specify -include or -exclude when diffing two dirs"
  in
  match (is_dir old_file, is_dir new_file) with
  | (true, false) | (false, true) ->
    if_not_diffing_two_dirs ();
    (* One is a directory, the other is a file *)
    let dir, file =
      if is_dir old_file then
        old_file, new_file
      else
        new_file, old_file
    in
    (* Match file with its twin file in dir *)
    let module FO = Find.Options in
    let filter = fun (filename, _stats) -> filename = file in
    let options = {
      FO.
      min_depth = 1;
      max_depth = Some 1;
      follow_links = FO.default.FO.follow_links;
      on_open_errors = FO.default.FO.on_open_errors;
      on_stat_errors = FO.default.FO.on_stat_errors;
      filter = Some filter;
      skip_dir = FO.default.FO.skip_dir;
      relative_paths = false;
    } in
    let matches = Find.find_all ~options dir in
    begin match matches with
    | [(matched_filename, _stats)] ->
      let old_file, new_file =
        if is_dir old_file then
          matched_filename, file
        else
          file, matched_filename
      in
      Compare_core.diff_files ~old_file ~new_file config
    | [] -> failwithf "File not found in %s: %s" dir file ()
    | _ -> (* This is impossible because max_depth is 1 *)
      failwithf "Directory contains clones: %s" dir ()
    end
  | (true, true) ->
    (* Both are directories *)
    let file_filter =
      if args.A.include_ = [] && args.A.exclude = []
      then None
      else Some (fun (s,stat) ->
        match stat.Unix.st_kind with
        | Unix.S_REG ->
          List.for_all args.A.exclude
            ~f:(fun pat -> not (Pcre.pmatch s ~pat))
          && (
            args.A.include_ = [] ||
              List.exists args.A.include_ ~f:(fun pat ->
                Pcre.pmatch s ~pat
              ))
        | _ -> true
      )
    in
    Compare_core.diff_dirs ~old_file ~new_file config ~file_filter
  | (false, false) ->
    (* Both are files *)
    if_not_diffing_two_dirs ();
    Compare_core.diff_files ~old_file ~new_file config
;;

let main arg =
  match arg with
  | Args.Make_config file -> Make_config.main file
  | Args.Print_readme -> Readme.main ()
  | Args.Compare compare_args ->
    let res = main' compare_args in
    List.iter !remove_at_exit ~f:(fun file ->
      try Unix.unlink file with _ -> ()
    );
    match res with
    | `Same -> exit 0
    | `Different -> exit 1
;;

let command =
  Deprecated_command.create
    ~summary
    ~usage_arg
    ~init
    ~flags
    ~final
    main


