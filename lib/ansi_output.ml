open! Core.Std
open! Import

module A = ANSITerminal

module Color = struct

  let of_internal =
    let module C = Patdiff_format.Color in
    function
    | C.Black -> A.Black | C.Red -> A.Red | C.Green -> A.Green | C.Yellow -> A.Yellow
    | C.Blue -> A.Blue | C.Magenta -> A.Magenta | C.Cyan -> A.Cyan | C.White -> A.White
    | C.Gray -> A.Bright_black
    | C.Bright_black -> A.Bright_black | C.Bright_red -> A.Bright_red
    | C.Bright_green -> A.Bright_green | C.Bright_yellow -> A.Bright_yellow
    | C.Bright_blue -> A.Bright_blue | C.Bright_magenta -> A.Bright_magenta
    | C.Bright_cyan -> A.Bright_cyan | C.Bright_white -> A.Bright_white
    | C.Default -> A.Default
    | C.Gray24 grey24 -> A.Gray24 grey24
    | C.RGB6 rgb6 -> A.RGB6 rgb6
  ;;
end

module Style = struct

  let of_internal =
    let module S = Patdiff_format.Style in
    function
    | S.Bold -> A.Bold | S.Underline -> A.Underlined | S.Emph -> A.Underlined
    | S.Blink -> A.Blink | S.Inverse -> A.Inverse | S.Hide -> A.Hidden
    | S.Dim -> A.Dim
    | S.Reset -> A.Reset
    | S.Foreground color | S.Fg color -> A.Foreground (Color.of_internal color)
    | S.Background color | S.Bg color -> A.Background (Color.of_internal color)
  ;;

  let drop_reset_prefix = List.drop_while ~f:(function A.Reset -> true | _ -> false)
  ;;

  let _apply text ~styles =
    match drop_reset_prefix (List.map styles ~f:of_internal) with
    | [] -> text
    | styles -> A.apply_string styles text
  ;;

  let apply text ~styles =
    match List.map styles ~f:(of_internal) with
    | [] -> text
    | styles -> A.apply_string (A.Reset :: styles) text
  ;;
end

module Rule = struct

  let apply text ~rule ~refined =
    let module R = Patdiff_format.Rule in
    let module A = Patdiff_format.Rule.Annex in
    let apply styles text = Style.apply text ~styles in
    sprintf "%s%s%s"
      (apply rule.R.pre.A.styles rule.R.pre.A.text)
      (if refined then apply [Patdiff_format.Style.Reset] text else apply rule.R.styles text)
      (apply rule.R.suf.A.styles rule.R.suf.A.text)
  ;;
end

let print_header ~rules ~file_names:(old_file, new_file) ~print =
  let print_line file rule =
    print (Rule.apply file ~rule ~refined:false)
  in
  let module Rz = Patdiff_format.Rules in
  print_line old_file rules.Rz.header_old;
  print_line new_file rules.Rz.header_new
;;

let print ~print_global_header ~file_names ~rules ~print ~location_style hunks =
  let module Rz = Patdiff_format.Rules in
  let f_hunk_break hunk =
    Patdiff_format.Location_style.sprint
      location_style
      hunk
      ~mine_filename:(fst file_names)
      ~rule:(Rule.apply ~rule:rules.Rz.hunk ~refined:false)
    |> print
  in
  if print_global_header
  then print_header ~rules ~file_names ~print;
  Patdiff_hunks.iter' ~f_hunk_break ~f_line:print hunks;
;;
