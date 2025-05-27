open! Core

type element =
  | Style of Style.t
  | Text of Text.t
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

type t = element list [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

let width t =
  List.sum
    (module Int)
    t
    ~f:(function
      | Style _ -> 0
      | Text txt -> Text.width txt)
;;

let is_empty t =
  List.for_all t ~f:(function
    | Style _ -> true
    | Text txt -> Text.is_empty txt)
;;

let to_string t =
  List.map t ~f:(fun text ->
    match text with
    | Style sty -> Style.to_string sty
    | Text txt -> Text.to_string txt)
  |> String.concat
;;

let to_string_hum t =
  List.map t ~f:(fun text ->
    match text with
    | Style sty -> Style.to_string_hum sty
    | Text txt -> Text.to_string txt)
  |> String.concat
;;

let to_unstyled t =
  List.filter_map t ~f:(function
    | Style _ -> None
    | Text txt -> Some (Text.to_string txt))
  |> String.concat
;;

let map ?(f_style = Fn.id) ?(f_text = Fn.id) t =
  List.map t ~f:(function
    | Style sty -> Style (f_style sty)
    | Text txt -> Text (f_text txt))
;;

(* Combine adjacent [Text] elements or adjacent [Style] elements; compress all styles. *)
let compress t =
  let rec scan_to_combine = function
    | [] -> []
    | Style prev :: Style next :: rest -> scan_to_combine (Style (prev @ next) :: rest)
    | Text prev :: Text next :: rest -> scan_to_combine (Text Text.(prev ^ next) :: rest)
    | text_or_style :: rest -> text_or_style :: scan_to_combine rest
  in
  List.filter t ~f:(function
    | Style _ -> true
    | Text txt -> not Text.(is_empty txt))
  |> scan_to_combine
  |> List.filter_map ~f:(function
    | Style sty ->
      let sty = Style.compress sty in
      if List.is_empty sty then None else Some (Style sty)
    | Text _ as txt -> Some txt)
;;

let simplify_styles t =
  let _, deltas =
    List.fold_map (compress t) ~init:[] ~f:(fun old_style style_or_text ->
      match style_or_text with
      | Style added_style ->
        let delta = Style.delta ~old_style ~added_style in
        Style.update ~old_style ~added_style, Style delta
      | Text _ as text -> old_style, text)
  in
  compress deltas
;;

let style_at_end t =
  List.fold t ~init:[] ~f:(fun old_style style_or_text ->
    match style_or_text with
    | Style added_style -> Style.update ~old_style ~added_style
    | Text _ -> old_style)
;;

let split ~pos t =
  match
    List.fold_until
      t
      ~init:(0, 0)
      ~f:(fun (idx, len) style_or_text ->
        match style_or_text with
        | Style _ -> Continue (idx + 1, len)
        | Text txt ->
          let w = Text.width txt in
          if len + w >= pos then Stop (Some (idx, len)) else Continue (idx + 1, len + w))
      ~finish:(fun _ -> None)
  with
  | None -> t, []
  | Some (idx, len) ->
    let at_boundary =
      match List.nth_exn t idx with
      | Style _ -> Text.of_string "" (* should be impossible *)
      | Text txt -> txt
    in
    let before, after =
      if len + Text.width at_boundary = pos
      then List.take t (idx + 1), List.drop t (idx + 1)
      else (
        let prefix, suffix = Text.split at_boundary ~pos:(pos - len) in
        List.take t idx @ [ Text prefix ], Text suffix :: List.drop t (idx + 1))
    in
    let middle_state = style_at_end before in
    let to_turn_on = Style.delta ~old_style:[] ~added_style:middle_state in
    let before = before @ [ Style (Style.turn_off to_turn_on) ] in
    let after =
      match after, to_turn_on with
      | Style sty :: rest, _ :: _ ->
        (* If we're putting one non-trivial style in front of another, compress them. *)
        Style (List.concat [ to_turn_on; sty ] |> Style.compress) :: rest
      | after, [] -> after
      | after, _ -> Style to_turn_on :: after
    in
    before, after
;;
