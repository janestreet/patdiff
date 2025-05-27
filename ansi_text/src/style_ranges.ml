open! Core

type range =
  { start : int
  ; end_ : int
  ; style : Style.t
  }
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

type t = range list [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

let identify text_with_styles =
  let unmatched_opens, ranges, _ =
    List.fold
      text_with_styles
      ~init:([], [], 0)
      ~f:(fun (open_ranges, closed_ranges, width) element ->
        match element with
        | Text_with_styles.Text txt -> open_ranges, closed_ranges, width + Text.width txt
        | Style new_style ->
          (match
             List.find_mapi open_ranges ~f:(fun i (start, old_style) ->
               if Style.closes ~new_style ~old_style
               then Some (i, start, old_style)
               else None)
           with
           | None ->
             let open_ranges = (width, new_style) :: open_ranges in
             open_ranges, closed_ranges, width
           | Some (i, start, old_style) ->
             let open_ranges = List.take open_ranges i @ List.drop open_ranges (i + 1) in
             let closed_ranges =
               { start; end_ = width; style = old_style } :: closed_ranges
             in
             open_ranges, closed_ranges, width))
  in
  let unmatched_styles = List.rev_map unmatched_opens ~f:snd in
  List.sort ~compare:compare_range ranges, unmatched_styles
;;

let apply ~text t =
  let styles =
    List.map t ~f:(fun r -> r.start, r.style)
    @ List.map t ~f:(fun r -> r.end_, Style.turn_off r.style)
    |> List.sort ~compare:(fun (pos1, _) (pos2, _) -> Int.compare pos1 pos2)
  in
  let acc, text_remaining, _ =
    List.fold
      styles
      ~init:([], text, 0)
      ~f:(fun (acc, text_remaining, last_pos) (pos, style) ->
        if pos = last_pos
        then Text_with_styles.Style style :: acc, text_remaining, pos
        else (
          let text_before, text_remaining =
            Text.split text_remaining ~pos:(pos - last_pos)
          in
          let acc =
            Text_with_styles.Style style :: Text_with_styles.Text text_before :: acc
          in
          acc, text_remaining, pos))
  in
  List.rev (Text_with_styles.Text text_remaining :: acc)
;;

let adjust_by ?(start = 0) ?(end_ = 0) t =
  List.map t ~f:(fun r -> { r with start = r.start + start; end_ = r.end_ + end_ })
;;

let exclude ~start ~end_ t =
  List.concat_map t ~f:(fun r ->
    match r.start, r.end_ with
    | s, e when start <= s && e <= end_ -> []
    | s, e when s <= start && end_ <= e ->
      [ { r with end_ = start }; { r with start = end_ } ]
    | s, e when s <= start && start <= e -> [ { r with end_ = start } ]
    | s, e when s <= end_ && end_ <= e -> [ { r with start = end_ } ]
    | _ -> [ r ])
;;

let split ~pos t =
  let before = List.filter t ~f:(fun r -> r.end_ <= pos) in
  let after =
    List.filter_map t ~f:(fun r ->
      if r.start >= pos
      then Some { r with start = r.start - pos; end_ = r.end_ - pos }
      else None)
  in
  let across = List.filter t ~f:(fun r -> r.start < pos && r.end_ > pos) in
  let before = before @ List.map across ~f:(fun r -> { r with end_ = pos }) in
  let after =
    List.map across ~f:(fun r -> { r with start = 0; end_ = r.end_ - pos }) @ after
  in
  before, after
;;
