open Core.Std
module P = Patdiff_core

type t = {
  output: P.Output.t;
  rules: P.Format.Rules.t;
  ext_cmp: string option;
  produce_unified_lines : bool;
  unrefined: bool;
  keep_ws: bool;
  split_long_lines: bool;
  context: int;
  shallow: bool;
  quiet: bool;
  double_check: bool;
  mask_uniques: bool;
  old_alt: string option;
  new_alt: string option;
}

module Color = P.Format.Color
module Style = P.Format.Style

module Annex = struct

    (* Simply a prefix or a suffix.  Might come with an associated style. *)

  type t = {
    text: string sexp_option;
    style: Style.t list sexp_option;
  } with sexp

  let blank = {
    text = None;
    style = None;
  }

  let get_text t =
    Option.value t.text ~default:""

  let length t_opt =
    let f = fun t -> String.length (get_text t) in
    Option.value_map t_opt ~default:0 ~f

  let to_internal t ~min_width =
    let text = sprintf "%*s" min_width (get_text t) in
    let styles = Option.value ~default:[] t.style in
    P.Format.Rule.Annex.create ~styles text

end

module Rule = struct

  type t = {
    prefix: Annex.t sexp_option;
    suffix: Annex.t sexp_option;
    style: Style.t list sexp_option;
  } with sexp

  let blank = {
    prefix = None;
    suffix = None;
    style = None;
  }

  let to_internal t =
    let module R = P.Format.Rule in
    let f = Annex.to_internal ~min_width:0 in
    let default = R.Annex.blank in
    let annex opt = Option.value_map ~default ~f opt in
    let pre = annex t.prefix in
    let suf = annex t.suffix in
    let style = Option.value ~default:[] t.style in
    R.create ~pre ~suf style

end

module Hunk = struct

  type t = Rule.t with sexp

  let to_internal t =
    let get_annex a = Option.value a ~default:Annex.blank in
    let prefix = get_annex t.Rule.prefix in
    let suffix = get_annex t.Rule.suffix in
    let prefix_text = Option.value prefix.Annex.text ~default:"@@ " in
    let suffix_text = Option.value suffix.Annex.text ~default:" @@" in
    let t = { t with
      Rule.prefix = Some { prefix with
        Annex.text = Some prefix_text;
      };
      Rule.suffix = Some { suffix with
        Annex.text = Some suffix_text;
      }
    } in
    Rule.to_internal t

end

module Header = struct

  type t = Rule.t with sexp

  let to_internal t ~default =
    let get_annex a = Option.value a ~default:Annex.blank in
    let prefix = get_annex t.Rule.prefix in
    let prefix_text = Option.value prefix.Annex.text ~default in
    let t = { t with
      Rule.prefix = Some { prefix with
        Annex.text = Some prefix_text;
      };
    } in
    Rule.to_internal t

end

module Line_rule = struct

  type t = {
    prefix: Annex.t sexp_option;
    suffix: Annex.t sexp_option;
    style: Style.t list sexp_option;
    word_same: Style.t list sexp_option;
  } with sexp

  let default = {
    prefix = None;
    suffix = None;
    style = None;
    word_same = None;
  }

end

module Output = struct
  type t = P.Output.t with sexp
end

module Config = struct

  type t = {
    dont_produce_unified_lines : bool sexp_option;
    dont_overwrite_word_old_word_new : bool sexp_option;
    config_path: string sexp_option;
    context: int sexp_option;
    unrefined: bool sexp_option;

    keep_whitespace: bool sexp_option;
    split_long_lines: bool sexp_option;
    quiet: bool sexp_option;
    shallow: bool sexp_option;
    double_check: bool sexp_option;
    mask_uniques: bool sexp_option;
    html: bool sexp_option;
    alt_old: string sexp_option;
    alt_new: string sexp_option;
    ext_cmp: string sexp_option;
    header_old: Header.t sexp_option;
    header_new: Header.t sexp_option;
    hunk: Hunk.t sexp_option;
    line_same: Line_rule.t sexp_option;
    line_old: Line_rule.t sexp_option;
    line_new: Line_rule.t sexp_option;
    line_unified: Line_rule.t sexp_option;
    word_old: Rule.t sexp_option;
    word_new: Rule.t sexp_option;
  } with sexp

end

module Old_config = struct

  module Line_changed = struct
    type t = {
      prefix_old: Annex.t;
      prefix_new: Annex.t;
    } with sexp
  end

  module Word_same = struct
    type t = {
      style_old: Style.t list;
      style_new: Style.t list;
    } with sexp
  end

  module Word_changed = struct
    type t = {
      style_old: Style.t list;
      style_new: Style.t list;
      prefix_old: Annex.t sexp_option;
      suffix_old: Annex.t sexp_option;
      prefix_new: Annex.t sexp_option;
      suffix_new: Annex.t sexp_option;
    } with sexp
  end

  module Old_header = struct
    type t = {
      style_old: Style.t list sexp_option;
      style_new: Style.t list sexp_option;
      prefix_old: Annex.t sexp_option;
      suffix_old: Annex.t sexp_option;
      prefix_new: Annex.t sexp_option;
      suffix_new: Annex.t sexp_option;
    } with sexp
  end

  type t = {
    config_path : string sexp_option;
    context : int sexp_option;
    unrefined : bool sexp_option;
    external_compare : string sexp_option;
    keep_whitespace : bool sexp_option;
    split_long_lines : bool sexp_option;
    shallow : bool sexp_option;
    quiet : bool sexp_option;
    double_check : bool sexp_option;
    hide_uniques : bool sexp_option;
    header: Old_header.t sexp_option;
    line_same: Style.t list sexp_option;
    line_same_prefix: Annex.t sexp_option;
    line_changed: Line_changed.t sexp_option;
    word_same: Word_same.t sexp_option;
    word_changed: Word_changed.t sexp_option;
    chunk: Hunk.t sexp_option;
  } with sexp

  let to_new_config t =
    {Config.config_path = t.config_path;
     context = t.context;
     unrefined = t.unrefined;
     dont_produce_unified_lines = None;
     dont_overwrite_word_old_word_new = None;
     keep_whitespace = t.keep_whitespace;
     split_long_lines = t.split_long_lines;
     quiet = t.quiet;
     shallow = t.shallow;
     double_check = t.double_check;
     mask_uniques = t.hide_uniques;
     html = None;
     alt_old = None;
     alt_new = None;
     ext_cmp = t.external_compare;
     header_old = Option.map t.header ~f:(fun header ->
       {Rule.style = header.Old_header.style_old;
       prefix = header.Old_header.prefix_old;
       suffix = header.Old_header.suffix_old;});
     header_new = Option.map t.header ~f:(fun header ->
       {Rule.style = header.Old_header.style_new;
       prefix = header.Old_header.prefix_new;
       suffix = header.Old_header.suffix_new;});
     hunk = t.chunk;
     line_same =
        Some
          {Line_rule.default with
            Line_rule.style = t.line_same;
            prefix = t.line_same_prefix;
            word_same =
              Option.map t.word_same ~f:(fun word_same ->
                word_same.Word_same.style_old);
          };
     line_old = Option.map t.line_changed ~f:(fun line_changed ->
       {Line_rule.default with
         Line_rule.style = Option.map t.word_changed ~f:(
           fun word_changed -> word_changed.Word_changed.style_old
         );
         Line_rule.prefix = Some line_changed.Line_changed.prefix_old;
         word_same =
           Option.map t.word_same ~f:(fun word_same ->
             word_same.Word_same.style_old);
       });
     line_new = Option.map t.line_changed ~f:(fun line_changed ->
       {Line_rule.default with
         Line_rule.style = Option.map t.word_changed ~f:(
           fun word_changed -> word_changed.Word_changed.style_new
         );
         Line_rule.prefix = Some line_changed.Line_changed.prefix_new;
         word_same =
           Option.map t.word_same ~f:(fun word_same ->
             word_same.Word_same.style_new);
       });
     line_unified = None;
     word_old = None;
     word_new = None;
    }
end


let parse config =
  let module C = Config in
  let module A = Annex in
  let module R = Rule in
  let module L = Line_rule in
  let c = config in
  let value = Option.value in
  (**** Command Line Arguments ****)
  let context = value ~default:(-1) c.C.context in
  let unrefined = value ~default:false c.C.unrefined in
  let produce_unified_lines =
    not (value ~default:false c.C.dont_produce_unified_lines)
  in
  let overwrite_word_old_word_new =
    not (value ~default:false c.C.dont_overwrite_word_old_word_new)
  in
  let keep_ws = value ~default:false c.C.keep_whitespace in
  let split_long_lines = value ~default:false c.C.split_long_lines in
  let shallow = value ~default:false c.C.shallow in
  let quiet = value ~default:false c.C.quiet in
  let double_check = value ~default:false c.C.double_check in
  let mask_uniques = value ~default:false c.C.mask_uniques in
  let ext_cmp = c.C.ext_cmp in
  let alt_old = c.C.alt_old in
  let alt_new = c.C.alt_new in
  (**** Output Type ****)
  let output =
    let html = Option.value c.C.html ~default:false in
    if html then P.Output.Html else P.Output.Ansi
  in
  (**** Styling Rules ****)
  (* Words *)
  let create_word_same line_opt =
    let line = value line_opt ~default:L.default in
    P.Format.Rule.create (value ~default:[] line.L.word_same) in
  let word_same_old = create_word_same c.C.line_old in
  let word_same_new = create_word_same c.C.line_new in
  let word_same_unified = create_word_same c.C.line_unified in
  (* Lines *)
  let default = Line_rule.default in
  let line_same = value ~default c.C.line_same in
  let line_old = value ~default c.C.line_old in
  let line_new = value ~default c.C.line_new in
  let line_unified = value ~default c.C.line_unified in
  (* Words *)
  let create_word ~line_rule opt =
    if overwrite_word_old_word_new
    then R.to_internal {R.blank with R.style=line_rule.Line_rule.style}
    else R.to_internal (value opt ~default:R.blank)
  in
  let word_old = create_word ~line_rule:line_old c.C.word_old in
  let word_new = create_word ~line_rule:line_new c.C.word_new in
  (* Padding for prefixes: They should all be the same length. *)
  let len = Annex.length in
  let sam_len = len line_same.L.prefix in
  let old_len = len line_old.L.prefix in
  let new_len = len line_new.L.prefix in
  let uni_len = len line_unified.L.prefix in
  let min_width = max (max (max old_len new_len) sam_len) uni_len in
  let pad a = A.to_internal ~min_width (value ~default:A.blank a) in
  let same_prefix = pad line_same.L.prefix in
  let old_prefix = pad line_old.L.prefix in
  let new_prefix = pad line_new.L.prefix in
  let unified_prefix = pad line_unified.L.prefix in
  (* Construct the rules *)
  let create style ~pre = P.Format.Rule.create (value ~default:[] style) ~pre in
  let line_same = create line_same.L.style ~pre:same_prefix in
  let line_old = create line_old.L.style ~pre:old_prefix in
  let line_new = create line_new.L.style ~pre:new_prefix in
  let line_unified = create line_unified.L.style  ~pre:unified_prefix in
  (* Header *)
  let get_header h_opt = value h_opt ~default:R.blank in
  let create h_opt default = Header.to_internal (get_header h_opt) ~default in
  let header_old = create c.C.header_old "---" in
  let header_new = create c.C.header_new "+++" in
  (* Chunks *)
  let hunk = Hunk.to_internal (Option.value c.C.hunk ~default:R.blank) in
  (* Final *)
  let rules = {
    P.Format.Rules.line_same = line_same ~name:"linesame";
    line_old = line_old ~name:"lineold";
    line_new = line_new ~name:"linenew";
    line_unified = line_unified ~name:"lineunified";
    word_same_old = word_same_old ~name:"wordsameold";
    word_same_new = word_same_new ~name:"wordsamenew";
    word_same_unified = word_same_unified ~name:"wordsameunified";
    word_old = word_old ~name:"wordold";
    word_new = word_new ~name:"wordnew";
    hunk = hunk ~name:"hunk";
    header_old = header_old ~name:"headerold";
    header_new = header_new ~name:"headernew";
  } in
  {
    rules;
    output;
    context;
    unrefined;
    produce_unified_lines;
    ext_cmp;
    keep_ws;
    split_long_lines;
    shallow;
    quiet;
    double_check;
    mask_uniques;
    old_alt = alt_old;
    new_alt = alt_new;
  }

let load_sexp_conv f conv = Result.try_with (fun () -> Sexp.load_sexp_conv_exn f conv)

let rec load_exn' ~set config_file =
  let config =
    match load_sexp_conv config_file Config.t_of_sexp with
    | Ok c -> c
    | Error exn ->
      let as_old_config =
        Result.map (load_sexp_conv config_file Old_config.t_of_sexp)
          ~f:Old_config.to_new_config
      in
      match as_old_config with
      | Error _another_exn -> raise exn
      | Ok c ->
        begin
          let new_file = config_file ^ ".new" in
          match Sys.file_exists new_file with
          | `Yes | `Unknown -> ()
          | `No -> try Sexp.save_hum new_file (Config.sexp_of_t c) with _ -> ()
        end;
        c
  in
  match config.Config.config_path with
  | Some config_path ->
    if Set.mem set config_path
    then failwith "Cycle detected! file redirects to itself"
    else load_exn' ~set:(Set.add set config_path) config_path
  | None -> parse config

let load_exn config_file =
  load_exn' ~set:String.Set.empty config_file

(* prints errors to stderr *)
let load ?(quiet_errors=false) config_file =
  try Some (load_exn config_file) with
  | e ->
    if not quiet_errors then
      eprintf "Note: error loading %S: %s\n%!" config_file (Exn.to_string e);
    None
