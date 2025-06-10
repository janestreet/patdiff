open! Core
module Utf8 = String.Utf8

type t =
  { str : string
  ; char_widths : (Uchar.t * int) list
  ; width : int
  }
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

let width t = t.width
let is_empty t = String.is_empty t.str

let of_string str =
  let chars = Utf8.sanitize str |> Utf8.to_list in
  let widths = List.map chars ~f:(fun c -> Int.max 0 (Uucp.Break.tty_width_hint c)) in
  let width = List.sum (module Int) widths ~f:Fn.id in
  { str; char_widths = List.zip_exn chars widths; width }
;;

let to_string t = t.str

let split t ~pos =
  let prefix_width, n =
    List.fold_until
      t.char_widths
      ~init:(0, 0)
      ~f:(fun (width_so_far, idx) (_, this_char_width) ->
        let new_width_so_far = width_so_far + this_char_width in
        if new_width_so_far > pos
        then Stop (width_so_far, idx)
        else Continue (new_width_so_far, idx + 1))
      ~finish:Fn.id
  in
  let prefix_char_widths, suffix_char_widths = List.split_n t.char_widths n in
  let prefix_byte_length =
    List.sum (module Int) prefix_char_widths ~f:(fun (char, _) ->
      Uchar.Utf8.byte_length char)
  in
  let prefix = String.prefix t.str prefix_byte_length in
  let suffix = String.drop_prefix t.str prefix_byte_length in
  ( { str = prefix; char_widths = prefix_char_widths; width = prefix_width }
  , { str = suffix; char_widths = suffix_char_widths; width = t.width - prefix_width } )
;;

let ( ^ ) t1 t2 =
  { str = t1.str ^ t2.str
  ; char_widths = t1.char_widths @ t2.char_widths
  ; width = t1.width + t2.width
  }
;;

let concat t_list =
  List.fold t_list ~init:{ str = ""; char_widths = []; width = 0 } ~f:( ^ )
;;
