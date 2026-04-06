open! Core

module T = struct
  type t =
    | Reset
    | Bold
    | Faint
    | Normal_weight
    | Italic
    | Fraktur
    | Not_emphasis
    | Underline
    | Double_ul
    | Not_underline
    | Blink
    | Fast_blink
    | Not_blink
    | Framed
    | Encircled
    | Not_framed
    | Superscript
    | Subscript
    | Not_script
    | Invert
    | Not_invert
    | Hide
    | Not_hide
    | Strike
    | Not_strike
    | Overline
    | Not_overline
    | Variable_width
    | Fixed_width
    | Fg of Color.t
    | Bg of Color.t
    | Ul_color of Color.t
    | Font of int
    | Ideogram of int
    | Other of int list [@quickcheck.do_not_generate]
  [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]
end

include T
include Comparable.Make (T)

let turn_off = function
  | Reset -> None
  | Fg Default | Bg Default | Ul_color Default | Font 0 | Ideogram 5 -> None
  | Fg _ -> Fg Default |> Some
  | Bg _ -> Bg Default |> Some
  | Ul_color _ -> Ul_color Default |> Some
  | Font _ -> Font 0 |> Some
  | Ideogram _ -> Ideogram 5 |> Some
  | Bold | Faint -> Some Normal_weight
  | Italic | Fraktur -> Some Not_emphasis
  | Underline | Double_ul -> Some Not_underline
  | Blink | Fast_blink -> Some Not_blink
  | Framed | Encircled -> Some Not_framed
  | Superscript | Subscript -> Some Not_script
  | Invert -> Some Not_invert
  | Hide -> Some Not_hide
  | Strike -> Some Not_strike
  | Overline -> Some Not_overline
  | Variable_width -> Some Fixed_width
  | Normal_weight
  | Not_emphasis
  | Not_underline
  | Not_blink
  | Not_invert
  | Not_hide
  | Not_strike
  | Not_overline
  | Not_framed
  | Not_script
  | Fixed_width
  | Other _ -> None
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
  | (Bold | Faint | Normal_weight), (Bold | Faint | Normal_weight) -> true
  | (Italic | Not_emphasis | Fraktur), (Italic | Not_emphasis | Fraktur) -> true
  | (Underline | Double_ul | Not_underline), (Underline | Double_ul | Not_underline) ->
    true
  | (Blink | Fast_blink | Not_blink), (Blink | Fast_blink | Not_blink) -> true
  | (Invert | Not_invert), (Invert | Not_invert) -> true
  | (Hide | Not_hide), (Hide | Not_hide) -> true
  | (Strike | Not_strike), (Strike | Not_strike) -> true
  | (Overline | Not_overline), (Overline | Not_overline) -> true
  | (Framed | Encircled | Not_framed), (Framed | Encircled | Not_framed) -> true
  | (Superscript | Subscript | Not_script), (Superscript | Subscript | Not_script) -> true
  | (Variable_width | Fixed_width), (Variable_width | Fixed_width) -> true
  | Fg _, Fg _ -> true
  | Bg _, Bg _ -> true
  | Ul_color _, Ul_color _ -> true
  | Font _, Font _ -> true
  | Ideogram _, Ideogram _ -> true
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
  | [ f ] when between f (10, 19) -> Font (f - 10)
  | [ 20 ] -> Fraktur
  | [ 21 ] -> Double_ul
  | [ 22 ] -> Normal_weight
  | [ 23 ] -> Not_emphasis
  | [ 24 ] -> Not_underline
  | [ 25 ] -> Not_blink
  | [ 26 ] -> Variable_width
  | [ 27 ] -> Not_invert
  | [ 28 ] -> Not_hide
  | [ 29 ] -> Not_strike
  | [ c ] when between c (30, 37) -> Fg (Color.sgr8_exn (c - 30))
  | 38 :: rest ->
    (match of_color_code rest with
     | Some color -> Fg color
     | None -> Other codes)
  | [ 39 ] -> Fg Color.Default
  | [ c ] when between c (40, 47) -> Bg (Color.sgr8_exn (c - 40))
  | 48 :: rest ->
    (match of_color_code rest with
     | Some color -> Bg color
     | None -> Other codes)
  | [ 49 ] -> Bg Color.Default
  | [ 50 ] -> Fixed_width
  | [ 51 ] -> Framed
  | [ 52 ] -> Encircled
  | [ 53 ] -> Overline
  | [ 54 ] -> Not_framed
  | [ 55 ] -> Not_overline
  | 58 :: rest ->
    (match of_color_code rest with
     | Some color -> Ul_color color
     | None -> Other codes)
  | [ 59 ] -> Ul_color Color.Default
  | [ i ] when between i (60, 65) -> Ideogram (i - 60)
  | [ 73 ] -> Superscript
  | [ 74 ] -> Subscript
  | [ 75 ] -> Not_script
  | [ c ] when between c (90, 97) -> Fg (Color.sgr8_exn ~bright:true (c - 90))
  | [ c ] when between c (100, 107) -> Bg (Color.sgr8_exn ~bright:true (c - 100))
  | _ -> Other codes
;;

let to_code = function
  | Reset -> [ 0 ]
  | Bold -> [ 1 ]
  | Faint -> [ 2 ]
  | Italic -> [ 3 ]
  | Underline -> [ 4 ]
  | Blink -> [ 5 ]
  | Fast_blink -> [ 6 ]
  | Invert -> [ 7 ]
  | Hide -> [ 8 ]
  | Strike -> [ 9 ]
  | Font i -> [ 10 + i ]
  | Fraktur -> [ 20 ]
  | Double_ul -> [ 21 ]
  | Normal_weight -> [ 22 ]
  | Not_emphasis -> [ 23 ]
  | Not_underline -> [ 24 ]
  | Not_blink -> [ 25 ]
  | Variable_width -> [ 26 ]
  | Not_invert -> [ 27 ]
  | Not_hide -> [ 28 ]
  | Not_strike -> [ 29 ]
  | Fg c -> Color.to_fg_code c
  | Bg c -> Color.to_bg_code c
  | Fixed_width -> [ 50 ]
  | Framed -> [ 51 ]
  | Encircled -> [ 52 ]
  | Overline -> [ 53 ]
  | Not_framed -> [ 54 ]
  | Not_overline -> [ 55 ]
  | Ul_color c -> Color.to_ul_code c
  | Ideogram i -> [ 60 + i ]
  | Superscript -> [ 73 ]
  | Subscript -> [ 74 ]
  | Not_script -> [ 75 ]
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
  | Normal_weight -> "-weight"
  | Not_emphasis ->
    "-italic" (* Since [Fraktur] isn't widely supported, this is clearer *)
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
  | Framed -> "+framed"
  | Encircled -> "+encircled"
  | Superscript -> "+superscript"
  | Subscript -> "+subscript"
  | Variable_width -> "+proportional_spacing"
  | Fraktur -> "+fraktur"
  | Not_framed -> "-framed"
  | Not_script -> "-script"
  | Fixed_width -> "-proportional_spacing"
  | Font 0 -> "-font"
  | Font f -> "+font:" ^ Int.to_string f
  | Ideogram 5 -> "-ideogram"
  | Ideogram i -> "+ideogram:" ^ Int.to_string i
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
  | Normal_weight -> "22"
  | Not_emphasis -> "23"
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
  | Framed -> "51"
  | Encircled -> "52"
  | Superscript -> "73"
  | Subscript -> "74"
  | Variable_width -> "26"
  | Fraktur -> "20"
  | Not_framed -> "54"
  | Not_script -> "75"
  | Fixed_width -> "50"
  | Font f -> Int.to_string (f + 10)
  | Ideogram i -> Int.to_string (i + 60)
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
