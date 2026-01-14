open! Core

let pad ?(char = ' ') ?(style = []) ~width:total_width t =
  let pad_len = max 0 (total_width - Text_with_ansi.width t) in
  let pad = String.init pad_len ~f:(fun _ -> char) |> Text.of_string in
  if List.is_empty style
  then t @ [ `Text pad ]
  else t @ [ `Style style; `Text pad; `Style (Style.turn_off style) ]
;;

let center ?(char = ' ') ?(style = []) ~width:total_width t =
  let text_width = Text_with_ansi.width t in
  let pad_len = max 0 (total_width - text_width) in
  let left_pad = String.init (pad_len / 2) ~f:(fun _ -> char) |> Text.of_string in
  let right_pad =
    String.init (pad_len - (pad_len / 2)) ~f:(fun _ -> char) |> Text.of_string
  in
  let left_pad = `Text left_pad in
  let right_pad = `Text right_pad in
  match style with
  | [] -> (left_pad :: t) @ [ right_pad ]
  | _ ->
    let turn_on = `Style style in
    let turn_off = `Style (Style.turn_off style) in
    (turn_on :: left_pad :: turn_off :: t) @ [ turn_on; right_pad; turn_off ]
;;

let truncate ~width:pos t = Text_with_ansi.split ~pos t |> fst

let wrap ~width:pos t =
  let rec helper acc t =
    if Text_with_ansi.is_empty t
    then List.rev acc
    else (
      let before, after = Text_with_ansi.split ~pos t in
      helper (before :: acc) after)
  in
  helper [] t
;;

let apply style str = Style.to_string style ^ str ^ Style.to_string (Style.turn_off style)
let visualize str = Parser.parse str |> Text_with_ansi.to_string_hum

let minimize str =
  Parser.parse str |> Text_with_ansi.simplify_styles |> Text_with_ansi.to_string
;;

let strip str = Parser.parse str |> Text_with_ansi.to_unstyled

let to_double_column ~width ~left ~right =
  let left =
    Parser.parse left
    |> wrap ~width
    |> List.map ~f:(fun l -> pad ~width l |> Text_with_ansi.to_string)
  in
  let right = Parser.parse right |> wrap ~width |> List.map ~f:Text_with_ansi.to_string in
  let left_length = List.length left in
  let right_length = List.length right in
  let lines_to_add_left = max 0 (right_length - left_length) in
  let lines_to_add_right = max 0 (left_length - right_length) in
  let left = left @ List.init lines_to_add_left ~f:(fun _ -> String.make width ' ') in
  let right = right @ List.init lines_to_add_right ~f:(fun _ -> "") in
  List.zip_exn left right
;;
