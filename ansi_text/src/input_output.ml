open! Core

type t = Text_with_styles.t [@@deriving compare, equal, quickcheck, sexp]

(* If the string starts with an ANSI escape sequence, split it off. *)
let split_leading_style str =
  if String.is_prefix str ~prefix:"\027["
  then (
    let str = String.drop_prefix str 2 in
    let escaped = String.take_while str ~f:Char.(fun c -> is_digit c || c = ';') in
    let m = String.length escaped in
    if m < String.length str && Char.(str.[m] = 'm')
    then Some (escaped, String.drop_prefix str (m + 1))
    else None)
  else None
;;

(* Split a string at the start of each ["\027["] substring. *)
let split_on_escapes str =
  let esc = String.Search_pattern.create "\027[" in
  String.Search_pattern.split_on esc str
  |> List.filter_mapi ~f:(fun i s ->
    match i, s with
    | 0, "" -> None
    | 0, s -> Some s
    | _, s -> Some ("\027[" ^ s))
;;

let parse str =
  let open Text_with_styles in
  split_on_escapes str
  |> List.map ~f:(fun substr ->
    match split_leading_style substr with
    | None -> [ Text (Text.of_string substr) ]
    | Some (style, text) ->
      (try [ Style (Style.of_string_exn style); Text (Text.of_string text) ] with
       | _ -> [ Text (Text.of_string substr) ]))
  |> List.concat
;;

let pad ?(char = ' ') ?(style = []) ~width:total_width t =
  let pad_len = max 0 (total_width - Text_with_styles.width t) in
  let pad = String.init pad_len ~f:(fun _ -> char) |> Text.of_string in
  if List.is_empty style
  then t @ [ Text_with_styles.Text pad ]
  else t @ Text_with_styles.[ Style style; Text pad; Style (Style.turn_off style) ]
;;

let center ?(char = ' ') ?(style = []) ~width:total_width t =
  let text_width = Text_with_styles.width t in
  let pad_len = max 0 (total_width - text_width) in
  let left_pad = String.init (pad_len / 2) ~f:(fun _ -> char) |> Text.of_string in
  let right_pad =
    String.init (pad_len - (pad_len / 2)) ~f:(fun _ -> char) |> Text.of_string
  in
  let left_pad = Text_with_styles.Text left_pad in
  let right_pad = Text_with_styles.Text right_pad in
  match style with
  | [] -> (left_pad :: t) @ [ right_pad ]
  | _ ->
    let turn_on = Text_with_styles.Style style in
    let turn_off = Text_with_styles.Style (Style.turn_off style) in
    (turn_on :: left_pad :: turn_off :: t) @ [ turn_on; right_pad; turn_off ]
;;

let truncate ~width:pos t = Text_with_styles.split ~pos t |> fst

let wrap ~width:pos t =
  let rec helper acc t =
    if Text_with_styles.is_empty t
    then List.rev acc
    else (
      let before, after = Text_with_styles.split ~pos t in
      helper (before :: acc) after)
  in
  helper [] t
;;

let apply style str = Style.to_string style ^ str ^ Style.to_string (Style.turn_off style)
let visualize str = parse str |> Text_with_styles.to_string_hum

let minimize str =
  parse str |> Text_with_styles.simplify_styles |> Text_with_styles.to_string
;;

let strip str = parse str |> Text_with_styles.to_unstyled
