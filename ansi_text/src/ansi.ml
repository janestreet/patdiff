open! Core

type t =
  [ `Control of Control.t
  | `Style of Style.t
  | `Hyperlink of Hyperlink.t
  | `Unknown of Unknown_esc.t
  ]
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(* Final bytes that correspond to Control sequences we understand *)
let is_control_final_byte = function
  | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'J' | 'K' | 'S' | 'T' -> true
  | _ -> false
;;

let of_csi ~params ~terminal =
  match terminal with
  | 'm' -> `Style (Style.of_sgr ~params)
  | c when is_control_final_byte c -> `Control (Control.of_csi ~params ~terminal)
  | _ -> `Unknown (Unknown_esc.Csi (params ^ String.make 1 terminal))
;;

let to_string = function
  | `Control c -> Control.to_string c
  | `Style s -> Style.to_string s
  | `Hyperlink h -> Hyperlink.to_string h
  | `Unknown u -> Unknown_esc.to_string u
;;

let to_string_hum = function
  | `Control c -> Control.to_string_hum c
  | `Style s -> Style.to_string_hum s
  | `Hyperlink h -> Hyperlink.to_string_hum h
  | `Unknown u -> Unknown_esc.to_string_hum u
;;
