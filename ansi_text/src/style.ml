open! Core

type t = Attr.t list [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

let of_sgr ~params =
  let codes =
    String.split params ~on:';'
    |> List.filter_map ~f:(function
      | "" -> None
      | i -> Int.of_string_opt i)
  in
  (* Per ECMA-48, [{ESC [ m}] (no parameters) is equivalent to [{ESC [ 0 m}] (reset) *)
  let codes = if List.is_empty codes then [ 0 ] else codes in
  let rec parse_several_codes acc codes =
    match codes with
    | [] -> List.rev acc
    | 38 :: 2 :: _ :: _ :: _ :: rest
    | 48 :: 2 :: _ :: _ :: _ :: rest
    | 58 :: 2 :: _ :: _ :: _ :: rest ->
      let first = Attr.of_codes (List.take codes 5) in
      parse_several_codes (first :: acc) rest
    | 38 :: 5 :: _ :: rest | 48 :: 5 :: _ :: rest | 58 :: 5 :: _ :: rest ->
      let first = Attr.of_codes (List.take codes 3) in
      parse_several_codes (first :: acc) rest
    | c :: rest ->
      let first = Attr.of_codes [ c ] in
      parse_several_codes (first :: acc) rest
  in
  parse_several_codes [] codes
;;

let to_string t =
  match t with
  | [] -> ""
  | _ ->
    let esc_codes = List.map t ~f:Attr.to_string |> String.concat ~sep:";" in
    sprintf "\027[%sm" esc_codes
;;

let includes_reset (t : t) =
  List.exists
    t
    ~f:
      Attr.(
        function
        | Reset -> true
        | _ -> false)
;;

let is_reset t =
  match List.last t with
  | Some Attr.Reset -> true
  | _ -> false
;;

(* If a reset is present, drop it and everything before. *)
let after_reset (t : t) =
  let rec helper acc = function
    | [] -> acc
    | hd :: tl -> if Attr.equal hd Reset then helper tl tl else helper acc tl
  in
  helper t t
;;

let compress (t : t) =
  let rec helper acc t =
    match t with
    | [] -> acc
    | attr :: rest ->
      let rest =
        List.filter rest ~f:(fun a -> not (Attr.overrides ~new_attr:attr ~old_attr:a))
      in
      helper (attr :: acc) rest
  in
  helper [] (List.rev t)
;;

let equal s1 s2 = equal s1 s2 || equal (compress s1) (compress s2)
let update ~old_style ~added_style = old_style @ added_style |> compress

let delta ~old_style ~added_style =
  let old_style = compress old_style in
  let added_style = compress added_style in
  match `Old (includes_reset old_style), `New_ (includes_reset added_style) with
  | `Old true, `New_ true ->
    (* Check whether the added style is shorter with or without the reset. *)
    let without_reset =
      let old_attrs = after_reset old_style in
      let new_attrs = after_reset added_style in
      let to_drop =
        List.filter old_attrs ~f:(fun old_attr ->
          not
            (List.mem new_attrs old_attr ~equal:(fun new_attr old_attr ->
               Attr.overrides ~new_attr ~old_attr)))
      in
      let to_add =
        List.filter new_attrs ~f:(fun new_attr ->
          not (List.mem old_attrs new_attr ~equal:Attr.equal))
      in
      List.filter_map to_drop ~f:Attr.turn_off @ to_add
    in
    if List.length without_reset < List.length added_style
    then without_reset
    else added_style
  | `Old false, `New_ true -> Reset :: after_reset added_style
  | `Old _, `New_ false ->
    List.filter added_style ~f:(fun new_attr ->
      let last_overridden =
        List.fold old_style ~init:None ~f:(fun found old_attr ->
          if Attr.overrides ~new_attr ~old_attr then Some old_attr else found)
      in
      match last_overridden with
      | Some a when Attr.equal a new_attr -> false
      | None | Some _ -> true)
;;

let turn_off t = List.filter_map t ~f:Attr.turn_off |> compress

let closes ~new_style ~old_style =
  List.for_all old_style ~f:(fun old_attr ->
    List.exists new_style ~f:(fun new_attr -> Attr.overrides ~new_attr ~old_attr))
;;

let to_string_hum t =
  match t with
  | [] -> ""
  | _ ->
    let inner = String.concat (List.map t ~f:Attr.to_string_hum) ~sep:" " in
    [%string "(%{inner})"]
;;
