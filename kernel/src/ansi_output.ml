open! Core
open! Import

let apply_styles (styles : Format.Style.t list) str = Ansi_text.apply styles str

module Rule = struct
  let apply text ~(rule : Format.Rule.t) ~refined =
    let only_whitespace =
      (not (String.is_empty text)) && String.for_all text ~f:Char.is_whitespace
    in
    let text_style : Format.Style.t list =
      if List.is_empty rule.styles
      then []
      else (
        match refined, only_whitespace with
        | true, _ -> [ Reset ]
        | false, true -> Invert :: rule.styles
        | false, false -> rule.styles)
    in
    sprintf
      "%s%s%s"
      (apply_styles rule.pre.styles rule.pre.text)
      (apply_styles text_style text)
      (apply_styles rule.suf.styles rule.suf.text)
  ;;
end

let print_header ~(rules : Format.Rules.t) ~file_names:(prev_file, next_file) ~print =
  let print_line (file : File_name.t) rule =
    print (Rule.apply (File_name.display_name file) ~rule ~refined:false)
  in
  print_line prev_file rules.header_prev;
  print_line next_file rules.header_next
;;

let print
  ~print_global_header
  ~file_names:((prev_file, _) as file_names)
  ~(rules : Format.Rules.t)
  ~print
  ~location_style
  hunks
  =
  let f_hunk_break hunk =
    Format.Location_style.sprint
      location_style
      hunk
      ~prev_filename:(File_name.display_name prev_file)
      ~rule:(Rule.apply ~rule:rules.hunk ~refined:false)
    |> print
  in
  if print_global_header then print_header ~rules ~file_names ~print;
  Hunks.iter' ~f_hunk_break ~f_line:print hunks
;;
