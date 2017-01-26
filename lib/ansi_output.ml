open! Core
open! Import

module A = struct

  (* From https://en.wikipedia.org/wiki/ANSI_escape_code last accessed 2016-05-26 *)

  module RGB6   = struct
    include Patdiff_format.Color.RGB6

    let escape_code { r; g; b; } = 16 + 36 * r + 6 * g + b

  end

  module Gray24 = struct
    include Patdiff_format.Color.Gray24

    let escape_code { level } = 232 + level

  end

  let codes_of_foreground_color : Patdiff_format.Color.t -> string = function
    | Black              -> "30"
    | Red                -> "31"
    | Green              -> "32"
    | Yellow             -> "33"
    | Blue               -> "34"
    | Magenta            -> "35"
    | Cyan               -> "36"
    | White              -> "37"
    | Default            -> "39"
    | Gray
    | Bright_black       -> "90"
    | Bright_red         -> "91"
    | Bright_green       -> "92"
    | Bright_yellow      -> "93"
    | Bright_blue        -> "94"
    | Bright_magenta     -> "95"
    | Bright_cyan        -> "96"
    | Bright_white       -> "97"
    | RGB6   x           -> sprintf "38;5;%d" (RGB6.escape_code x)
    | Gray24 x           -> sprintf "38;5;%d" (Gray24.escape_code x)
  ;;

  let codes_of_background_color : Patdiff_format.Color.t -> string = function
    | Black          -> "40"
    | Red            -> "41"
    | Green          -> "42"
    | Yellow         -> "43"
    | Blue           -> "44"
    | Magenta        -> "45"
    | Cyan           -> "46"
    | White          -> "47"
    | Default        -> "49"
    | Gray
    | Bright_black   -> "100"
    | Bright_red     -> "101"
    | Bright_green   -> "102"
    | Bright_yellow  -> "103"
    | Bright_blue    -> "104"
    | Bright_magenta -> "105"
    | Bright_cyan    -> "106"
    | Bright_white   -> "107"
    | RGB6   x       -> sprintf "48;5;%d" (RGB6.escape_code x)
    | Gray24 x       -> sprintf "48;5;%d" (Gray24.escape_code x)
  ;;

  let codes_of_style : Patdiff_format.Style.t -> string = function
    | Reset      -> "0"
    | Bold       -> "1"
    | Dim        -> "2"
    | Underline
    | Emph       -> "4"
    | Blink      -> "5"
    | Inverse    -> "7"
    | Hide       -> "8"
    | Fg c
    | Foreground c -> codes_of_foreground_color c
    | Bg c
    | Background c -> codes_of_background_color c
  ;;

  let apply_string styles str =
    match styles with
    | [] -> str
    | _ ->
      sprintf "\027[%sm%s\027[0m"
        (List.map styles ~f:codes_of_style |> String.concat ~sep:";")
        str
  ;;
end

module Style = struct

  let drop_reset_prefix =
    List.drop_while ~f:(function Patdiff_format.Style.Reset -> true | _ -> false)
  ;;

  let _apply text ~styles =
    match drop_reset_prefix styles with
    | [] -> text
    | styles -> A.apply_string styles text
  ;;

  let apply text ~styles =
    match styles with
    | [] -> text
    | styles -> A.apply_string (Reset :: styles) text
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
