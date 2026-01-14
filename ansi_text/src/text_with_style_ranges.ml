open! Core

type t =
  { text : Text.t
  ; ranges : Style_ranges.t
  }
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

let width t = Text.width t.text
let is_empty t = Text.is_empty t.text

let to_text_with_ansi t =
  let compare (s1, _) (s2, _) = s1 - s2 in
  let start_styles =
    List.map t.ranges ~f:(fun r -> r.start, r.style) |> List.sort ~compare
  in
  let end_styles =
    List.map t.ranges ~f:(fun r -> r.end_, Style.turn_off r.style) |> List.sort ~compare
  in
  let styles = List.merge end_styles start_styles ~compare in
  let text_with_styles_rev, _, _ =
    List.fold
      styles
      ~init:(([] : Text_with_ansi.t), t.text, 0)
      ~f:(fun (text_with_styles_rev, text_remaining, last_pos) (pos, style) ->
        let text, text_remaining = Text.split text_remaining ~pos:(pos - last_pos) in
        let text_with_styles_rev =
          if Text.is_empty text
          then `Style style :: text_with_styles_rev
          else `Style style :: `Text text :: text_with_styles_rev
        in
        text_with_styles_rev, text_remaining, pos)
  in
  List.rev text_with_styles_rev
;;

let of_text_with_ansi (text_with_ansi : Text_with_ansi.t) =
  let ranges, unmatched_styles = Style_ranges.identify text_with_ansi in
  match unmatched_styles with
  | [] ->
    let text =
      List.filter_map text_with_ansi ~f:(function
        | `Text txt -> Some txt
        | `Style _ | `Control _ | `Hyperlink _ | `Unknown _ -> None)
      |> Text.concat
    in
    Some { text; ranges }
  | _ -> None
;;

let to_string t = to_text_with_ansi t |> Text_with_ansi.to_string
let to_string_hum t = to_text_with_ansi t |> Text_with_ansi.to_string_hum
let to_unstyled t = Text.to_string t.text

let unstyle_between ~start ~end_ t =
  let ranges = Style_ranges.exclude ~start ~end_ t.ranges in
  { text = t.text; ranges }
;;

let split ~pos t =
  let text_before, text_after = Text.split t.text ~pos in
  let ranges_before, ranges_after = Style_ranges.split ~pos t.ranges in
  ( { text = text_before; ranges = ranges_before }
  , { text = text_after; ranges = ranges_after } )
;;
