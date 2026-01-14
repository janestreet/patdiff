open! Core

type t =
  | Start of string
  | End
[@@deriving compare ~localize, equal ~localize, sexp]

(* Custom quickcheck generator that only produces valid OSC 8 hyperlink URLs. URLs must
   not contain control characters (0x00-0x1F), DEL (0x7F), or the ST terminator ESC \ *)
let quickcheck_generator =
  let open Base_quickcheck.Generator in
  (* Generate printable ASCII characters excluding ESC (0x1B) *)
  let safe_char =
    Char.quickcheck_generator
    |> filter ~f:(fun c ->
      let code = Char.to_int c in
      code >= 0x20 && code <= 0x7E && not (Char.equal c '\027'))
  in
  let safe_url = string_of safe_char in
  union [ return End; map safe_url ~f:(fun url -> Start url) ]
;;

let quickcheck_observer = Base_quickcheck.Observer.opaque
let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

let to_string t =
  match t with
  | End -> "\027]8;;\027\\"
  | Start url -> "\027]8;;" ^ url ^ "\027\\"
;;

let to_string_hum = function
  | Start url -> "(HREF:" ^ url ^ ")"
  | End -> "(/HREF)"
;;
