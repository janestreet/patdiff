open! Core

module T = struct
  type t =
    | Reset
    | Bold
    | Faint
    | Italic
    | Underline
    | Blink
    | Fast_blink
    | Double_ul
    | Invert
    | Hide
    | Strike
    | Overline
    | Not_bold_or_faint
    | Not_italic
    | Not_underline
    | Not_blink
    | Not_invert
    | Not_hide
    | Not_strike
    | Not_overline
    | Fg of Color.t
    | Bg of Color.t
    | Ul_color of Color.t
    | Other of int list [@quickcheck.do_not_generate]
  [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]
end

include T
include Comparable.Make (T)

let turn_off = function
  | Reset -> None
  | Fg _ -> Fg Default |> Some
  | Bg _ -> Bg Default |> Some
  | Bold -> Some Not_bold_or_faint
  | Faint -> Some Not_bold_or_faint
  | Italic -> Some Not_italic
  | Underline -> Some Not_underline
  | Blink -> Some Not_blink
  | Fast_blink -> Some Not_blink
  | Double_ul -> Some Not_underline
  | Invert -> Some Not_invert
  | Hide -> Some Not_hide
  | Strike -> Some Not_strike
  | Overline -> Some Not_overline
  | Not_bold_or_faint -> None
  | Not_italic -> None
  | Not_underline -> None
  | Not_blink -> None
  | Not_invert -> None
  | Not_hide -> None
  | Not_strike -> None
  | Not_overline -> None
  | Ul_color _ -> Ul_color Default |> Some
  | Other codes ->
    (match codes with
     | [ c ] when Int.(11 <= c && c <= 20) -> Other [ 10 ] |> Some (* fonts *)
     | [ c ] when Int.(c = 26) -> Other [ 50 ] |> Some (* proportional spacing *)
     | [ c ] when Int.(c = 51 || c = 52) -> Other [ 54 ] |> Some (* framed, encircled *)
     | [ c ] when Int.(60 <= c && c <= 64) -> Other [ 65 ] |> Some (* ideograms *)
     | [ c ] when Int.(73 <= c && c <= 74) -> Other [ 75 ] |> Some (* sub/superscript *)
     | _ -> None)
;;

(** Whether the new attribute would completely override the effects of the old one. For
    example, in the code
    {v "\027[1;2m" v}
    we have [Bold] followed by [Faint]. After this sequence, the text will be faint, and
    this is indicated by [overrides ~new_attr:Faint ~old_attr:Bold] returning true.
    [Reset] overrides everything; other attributes mostly come in pairs or groups of three
    that mutually override. *)
let overrides ~new_attr ~old_attr =
  match new_attr, old_attr with
  | Reset, _ -> true
  | (Bold | Faint | Not_bold_or_faint), (Bold | Faint | Not_bold_or_faint) -> true
  | (Italic | Not_italic), (Italic | Not_italic) -> true
  | (Underline | Double_ul | Not_underline), (Underline | Double_ul | Not_underline) ->
    true
  | (Blink | Fast_blink | Not_blink), (Blink | Fast_blink | Not_blink) -> true
  | (Invert | Not_invert), (Invert | Not_invert) -> true
  | (Hide | Not_hide), (Hide | Not_hide) -> true
  | (Strike | Not_strike), (Strike | Not_strike) -> true
  | (Overline | Not_overline), (Overline | Not_overline) -> true
  | Fg _, Fg _ -> true
  | Bg _, Bg _ -> true
  | Ul_color _, Ul_color _ -> true
  | Other [ o1 ], Other [ o2 ] when Int.(10 <= o1 && o1 <= 20 && 10 <= o2 && o2 <= 20) ->
    true
  | Other [ o1 ], Other [ o2 ] when Int.((o1 = 26 || o1 = 50) && (o2 = 26 || o2 = 50)) ->
    true
  | Other [ o1 ], Other [ o2 ]
    when Int.((o1 = 51 || o1 = 52 || o1 = 54) && (o2 = 51 || o2 = 52 || o2 = 54)) -> true
  | Other [ o1 ], Other [ o2 ] when Int.(60 <= o1 && o1 <= 65 && 60 <= o2 && o2 <= 65) ->
    true
  | Other [ o1 ], Other [ o2 ] when Int.(73 <= o1 && o1 <= 75 && 73 <= o2 && o2 <= 75) ->
    true
  | _ -> false
;;

let of_color_code code =
  let between n (low, high) = Int.between n ~low ~high in
  match code with
  | [ 5; c ] when between c (0, 7) -> Some (Color.sgr8_exn c)
  | [ 5; c ] when between c (8, 15) -> Some (Color.sgr8_exn ~bright:true (c - 8))
  | [ 5; c ] when between c (16, 231) -> Some (Color.Rgb6 (Color.Rgb6.of_code_exn c))
  | [ 5; c ] when between c (232, 255) -> Some (Color.gray24_exn c)
  | [ 2; r; g; b ] when between r (0, 255) && between g (0, 255) && between b (0, 255) ->
    Some (Color.rgb256_exn (r, g, b))
  | _ -> None
;;

let of_codes codes =
  let between n (low, high) = Int.between n ~low ~high in
  match codes with
  | [ 0 ] -> Reset
  | [ 1 ] -> Bold
  | [ 2 ] -> Faint
  | [ 3 ] -> Italic
  | [ 4 ] -> Underline
  | [ 5 ] -> Blink
  | [ 6 ] -> Fast_blink
  | [ 7 ] -> Invert
  | [ 8 ] -> Hide
  | [ 9 ] -> Strike
  | [ 21 ] -> Double_ul
  | [ 53 ] -> Overline
  | [ 22 ] -> Not_bold_or_faint
  | [ 23 ] -> Not_italic
  | [ 24 ] -> Not_underline
  | [ 25 ] -> Not_blink
  | [ 27 ] -> Not_invert
  | [ 28 ] -> Not_hide
  | [ 29 ] -> Not_strike
  | [ 55 ] -> Not_overline
  | [ 39 ] -> Fg Color.Default
  | [ c ] when between c (30, 37) -> Fg (Color.sgr8_exn (c - 30))
  | [ c ] when between c (90, 97) -> Fg (Color.sgr8_exn ~bright:true (c - 90))
  | 38 :: rest ->
    (match of_color_code rest with
     | Some color -> Fg color
     | None -> Other codes)
  | [ 49 ] -> Bg Color.Default
  | [ c ] when between c (40, 47) -> Bg (Color.sgr8_exn (c - 40))
  | [ c ] when between c (100, 107) -> Bg (Color.sgr8_exn ~bright:true (c - 100))
  | 48 :: rest ->
    (match of_color_code rest with
     | Some color -> Bg color
     | None -> Other codes)
  | 58 :: rest ->
    (match of_color_code rest with
     | Some color -> Ul_color color
     | None -> Other codes)
  | [ 59 ] -> Ul_color Color.Default
  | _ -> Other codes
;;

let to_code = function
  | Reset -> [ 0 ]
  | Fg c -> Color.to_fg_code c
  | Bg c -> Color.to_bg_code c
  | Bold -> [ 1 ]
  | Faint -> [ 2 ]
  | Italic -> [ 3 ]
  | Underline -> [ 4 ]
  | Blink -> [ 5 ]
  | Fast_blink -> [ 6 ]
  | Invert -> [ 7 ]
  | Hide -> [ 8 ]
  | Strike -> [ 9 ]
  | Double_ul -> [ 21 ]
  | Overline -> [ 53 ]
  | Not_bold_or_faint -> [ 22 ]
  | Not_italic -> [ 23 ]
  | Not_underline -> [ 24 ]
  | Not_blink -> [ 25 ]
  | Not_invert -> [ 27 ]
  | Not_hide -> [ 28 ]
  | Not_strike -> [ 29 ]
  | Not_overline -> [ 55 ]
  | Ul_color c -> Color.to_ul_code c
  | Other codes -> codes
;;

let to_string_hum = function
  | Reset -> "off"
  | Bold -> "+bold"
  | Faint -> "+faint"
  | Italic -> "+italic"
  | Underline -> "+uline"
  | Blink -> "+blink"
  | Fast_blink -> "+fastblink"
  | Invert -> "+invert"
  | Hide -> "+hide"
  | Strike -> "+strike"
  | Double_ul -> "+2uline"
  | Overline -> "+overline"
  | Not_bold_or_faint -> "-weight"
  | Not_italic -> "-italic"
  | Not_underline -> "-uline"
  | Not_blink -> "-blink"
  | Not_invert -> "-invert"
  | Not_hide -> "-hide"
  | Not_strike -> "-strike"
  | Not_overline -> "-overline"
  | Fg c -> "fg:" ^ Color.to_string_hum c
  | Bg c -> "bg:" ^ Color.to_string_hum c
  | Ul_color c -> "ul:" ^ Color.to_string_hum c
  | Other codes ->
    "ANSI-SGR:" ^ (List.map codes ~f:Int.to_string |> String.concat ~sep:";")
;;

let to_string = function
  | Reset -> "0"
  | Fg Default -> "39"
  | Bg Default -> "49"
  | Fg (Standard c) -> 30 + Color.Sgr8.to_code c |> Int.to_string
  | Bg (Standard c) -> 40 + Color.Sgr8.to_code c |> Int.to_string
  | Bold -> "1"
  | Faint -> "2"
  | Italic -> "3"
  | Underline -> "4"
  | Blink -> "5"
  | Fast_blink -> "6"
  | Invert -> "7"
  | Hide -> "8"
  | Strike -> "9"
  | Double_ul -> "21"
  | Overline -> "53"
  | Not_bold_or_faint -> "22"
  | Not_italic -> "23"
  | Not_underline -> "24"
  | Not_blink -> "25"
  | Not_invert -> "27"
  | Not_hide -> "28"
  | Not_strike -> "29"
  | Not_overline -> "55"
  | Fg c -> Color.to_fg_code c |> List.map ~f:Int.to_string |> String.concat ~sep:";"
  | Bg c -> Color.to_bg_code c |> List.map ~f:Int.to_string |> String.concat ~sep:";"
  | Ul_color c ->
    Color.to_ul_code c |> List.map ~f:Int.to_string |> String.concat ~sep:";"
  | Other codes -> List.map codes ~f:Int.to_string |> String.concat ~sep:";"
;;

(* For compatibility with legacy patdiff configs, which used the much-less-expressive
   [Patdiff_kernel.Format.Style.t] type. *)
let t_of_sexp sexp =
  match sexp with
  | Sexp.Atom "dim" -> Faint
  | Atom "inverse" -> Invert
  | Atom "emph" -> Italic
  | _ as a -> t_of_sexp a
;;
