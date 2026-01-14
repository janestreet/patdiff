open! Core

module Sgr8 = struct
  type t =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
  [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

  let of_code_exn code =
    match code with
    | 0 -> Black
    | 1 -> Red
    | 2 -> Green
    | 3 -> Yellow
    | 4 -> Blue
    | 5 -> Magenta
    | 6 -> Cyan
    | 7 -> White
    | _ -> invalid_arg "Sgr8 code -- expected (0 <= code < 8)"
  ;;

  let to_code = function
    | Black -> 0
    | Red -> 1
    | Green -> 2
    | Yellow -> 3
    | Blue -> 4
    | Magenta -> 5
    | Cyan -> 6
    | White -> 7
  ;;

  let to_string_hum = function
    | Black -> "black"
    | Red -> "red"
    | Green -> "green"
    | Yellow -> "yellow"
    | Blue -> "blue"
    | Magenta -> "magenta"
    | Cyan -> "cyan"
    | White -> "white"
  ;;
end

module Rgb6 = struct
  type t =
    { r : int
    ; g : int
    ; b : int
    }
  [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

  let of_rgb_exn (r, g, b) =
    let check c = 0 <= c && c < 6 in
    if not (check r && check g && check b)
    then invalid_arg "Rgb6 (r, g, b) -- expected (0 <= r, g, b < 6)";
    { r; g; b }
  ;;

  let of_code_exn code =
    if not (16 <= code && code < 232)
    then invalid_arg "Rgb6 code -- expected (16 <= code < 232)";
    let rgb = code - 16 in
    { r = rgb / 36; g = rgb / 6 mod 6; b = rgb mod 6 }
  ;;

  let to_rgb t = t.r, t.g, t.b
  let to_code t = 16 + (36 * t.r) + (6 * t.g) + t.b
  let to_string_hum t = sprintf "rgb6-%d-%d-%d" t.r t.g t.b

  (* Custom quickcheck generator that only produces valid Rgb6 values (0-5) *)
  let quickcheck_generator =
    let open Base_quickcheck.Generator.Let_syntax in
    let component = Base_quickcheck.Generator.int_inclusive 0 5 in
    let%map r = component
    and g = component
    and b = component in
    { r; g; b }
  ;;
end

module Gray24 = struct
  type t = { level : int }
  [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

  let of_level_exn level =
    if not (0 <= level && level < 24)
    then invalid_arg "Gray24 level -- expected (0 <= level < 24)";
    { level }
  ;;

  let of_code_exn code =
    if not (232 <= code && code < 256)
    then invalid_arg "Gray24 code -- expected (232 <= code < 256)";
    { level = code - 232 }
  ;;

  let to_level t = t.level
  let to_code t = 232 + t.level
  let to_string_hum t = sprintf "gray-%d" t.level

  (* Custom quickcheck generator that only produces valid Gray24 values (0-23) *)
  let quickcheck_generator =
    Base_quickcheck.Generator.map
      (Base_quickcheck.Generator.int_inclusive 0 23)
      ~f:(fun level -> { level })
  ;;
end

module Rgb256 = struct
  type t =
    { r : int
    ; g : int
    ; b : int
    }
  [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

  let of_rgb_exn (r, g, b) =
    let check c = 0 <= c && c < 256 in
    if not (check r && check g && check b)
    then invalid_arg "Rgb6 (r, g, b) -- expected (0 <= r, g, b < 256)";
    { r; g; b }
  ;;

  let to_rgb t = t.r, t.g, t.b
  let to_string_hum t = sprintf "rgb256-%d-%d-%d" t.r t.g t.b

  (* Custom quickcheck generator that only produces valid Rgb256 values (0-255) *)
  let quickcheck_generator =
    let open Base_quickcheck.Generator.Let_syntax in
    let component = Base_quickcheck.Generator.int_inclusive 0 255 in
    let%map r = component
    and g = component
    and b = component in
    { r; g; b }
  ;;
end

module T = struct
  type t =
    | Default
    | Standard of Sgr8.t
    | Bright of Sgr8.t
    | Rgb6 of Rgb6.t
    | Gray24 of Gray24.t
    | Rgb256 of Rgb256.t
  [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]
end

include T
include Comparable.Make (T)

let sgr8_exn ?(bright = false) code =
  match bright with
  | false -> Standard (Sgr8.of_code_exn code)
  | true -> Bright (Sgr8.of_code_exn code)
;;

let rgb6_exn rgb = Rgb6 (Rgb6.of_rgb_exn rgb)
let gray24_exn code = Gray24 (Gray24.of_code_exn code)
let rgb256_exn rgb = Rgb256 (Rgb256.of_rgb_exn rgb)

let to_string_hum = function
  | Default -> "default"
  | Standard sgr8 -> Sgr8.to_string_hum sgr8
  | Bright Black -> "gray"
  | Bright sgr8 -> sprintf "bright-%s" (Sgr8.to_string_hum sgr8)
  | Rgb6 rgb6 -> Rgb6.to_string_hum rgb6
  | Gray24 gray24 -> Gray24.to_string_hum gray24
  | Rgb256 rgb256 -> Rgb256.to_string_hum rgb256
;;

let to_fg_code = function
  | Default -> [ 39 ]
  | Standard c -> [ 30 + Sgr8.to_code c ]
  | Bright c -> [ 90 + Sgr8.to_code c ]
  | Rgb6 c -> [ 38; 5; Rgb6.to_code c ]
  | Gray24 c -> [ 38; 5; Gray24.to_code c ]
  | Rgb256 c ->
    let r, g, b = Rgb256.to_rgb c in
    [ 38; 2; r; g; b ]
;;

let to_bg_code = function
  | Default -> [ 49 ]
  | Standard c -> [ 40 + Sgr8.to_code c ]
  | Bright c -> [ 100 + Sgr8.to_code c ]
  | Rgb6 c -> [ 48; 5; Rgb6.to_code c ]
  | Gray24 c -> [ 48; 5; Gray24.to_code c ]
  | Rgb256 c ->
    let r, g, b = Rgb256.to_rgb c in
    [ 48; 2; r; g; b ]
;;

let to_ul_code = function
  | Default -> [ 59 ]
  | Standard c -> [ 58; 5; Sgr8.to_code c ]
  | Bright c -> [ 58; 5; 8 + Sgr8.to_code c ]
  | Rgb6 c -> [ 58; 5; Rgb6.to_code c ]
  | Gray24 c -> [ 58; 5; Gray24.to_code c ]
  | Rgb256 c ->
    let r, g, b = Rgb256.to_rgb c in
    [ 58; 2; r; g; b ]
;;

(* For compatibility with legacy patdiff configs, which used the much-less-expressive
   [Patdiff_kernel.Format.Color.t] type. *)
let t_of_sexp sexp =
  match sexp with
  | Sexp.Atom "black" | Atom "Black" -> Standard Black
  | Atom "red" | Atom "Red" -> Standard Red
  | Atom "green" | Atom "Green" -> Standard Green
  | Atom "yellow" | Atom "Yellow" -> Standard Yellow
  | Atom "blue" | Atom "Blue" -> Standard Blue
  | Atom "magenta" | Atom "Magenta" -> Standard Magenta
  | Atom "cyan" | Atom "Cyan" -> Standard Cyan
  | Atom "white" | Atom "White" -> Standard White
  | Atom "gray" | Atom "Gray" -> Bright Black
  | Atom "bright_black" | Atom "Bright_black" -> Bright Black
  | Atom "bright_red" | Atom "Bright_red" -> Bright Red
  | Atom "bright_green" | Atom "Bright_green" -> Bright Green
  | Atom "bright_yellow" | Atom "Bright_yellow" -> Bright Yellow
  | Atom "bright_blue" | Atom "Bright_blue" -> Bright Blue
  | Atom "bright_magenta" | Atom "Bright_magenta" -> Bright Magenta
  | Atom "bright_cyan" | Atom "Bright_cyan" -> Bright Cyan
  | Atom "bright_white" | Atom "Bright_white" -> Bright White
  | List (Atom "GRAY24" :: [ level ]) -> Gray24 (Gray24.t_of_sexp level)
  | List (Atom "RGB6" :: [ rgb ]) -> Rgb6 (Rgb6.t_of_sexp rgb)
  | List (Atom "RGB256" :: [ rgb ]) -> Rgb256 (Rgb256.t_of_sexp rgb)
  | _ as c -> t_of_sexp c
;;

let black = Standard Sgr8.Black
let red = Standard Sgr8.Red
let green = Standard Sgr8.Green
let yellow = Standard Sgr8.Yellow
let blue = Standard Sgr8.Blue
let magenta = Standard Sgr8.Magenta
let cyan = Standard Sgr8.Cyan
let white = Standard Sgr8.White
let gray = Bright Sgr8.Black

let default_text_color_for ~bg =
  match bg with
  | Standard srg8 ->
    (match srg8 with
     | White | Yellow -> black
     | _ -> white)
  | Bright srg8 ->
    (match srg8 with
     | White | Yellow | Green | Cyan -> black
     | Black | Red | Blue | Magenta -> white)
  | Rgb6 { r; g; b } ->
    if Int.(r + g + b < 6) then white else if Int.(r + g + b < 12) then Default else black
  | Gray24 { level } ->
    if Int.(level < 8) then white else if Int.(level < 16) then Default else black
  | Rgb256 { r; g; b } ->
    if Int.(r + g + b < 256)
    then white
    else if Int.(r + g + b < 512)
    then Default
    else black
  | Default -> Default
;;
