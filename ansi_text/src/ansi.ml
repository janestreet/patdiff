open! Core

type formatting =
  [ `Style of Style.t
  | `Hyperlink of Hyperlink.t
  ]
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

type emulation =
  [ `Private_mode of Private_mode.t
  | `Osc of Osc.t
  | `Dsr of Dsr.t
  ]
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

type t =
  [ formatting
  | emulation
  | `Control of Control.t
  | `Unknown of Unknown_esc.t
  ]
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(* Final bytes that correspond to [Control] sequences we understand *)
let is_control_final_byte = function
  | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'J' | 'K' | 'S' | 'T' -> true
  | _ -> false
;;

let parse_params params =
  if String.is_empty params
  then []
  else
    String.split params ~on:';'
    |> List.map ~f:(fun s -> if String.is_empty s then None else Int.of_string_opt s)
;;

let of_csi ?private_prefix param_string ~terminal =
  let unknown () = `Unknown (Unknown_esc.of_csi ?private_prefix param_string ~terminal) in
  match private_prefix with
  | Some '?' ->
    (match Private_mode.of_csi_params (parse_params param_string) ~terminal with
     | Some pm -> `Private_mode pm
     | None -> unknown ())
  | Some _ -> unknown ()
  | None ->
    (match terminal with
     | 'm' -> `Style (Style.of_sgr_params (parse_params param_string))
     | 'n' ->
       (match Dsr.of_params (parse_params param_string) with
        | Some dsr -> `Dsr dsr
        | None -> unknown ())
     | c when is_control_final_byte c ->
       `Control (Control.of_csi_params (parse_params param_string) ~terminal)
     | _ -> unknown ())
;;

let of_osc_payload payload = `Osc (Osc.of_payload payload)

let to_string = function
  | `Control c -> Control.to_string c
  | `Style s -> Style.to_string s
  | `Hyperlink h -> Hyperlink.to_string h
  | `Private_mode p -> Private_mode.to_string p
  | `Osc o -> Osc.to_string o
  | `Dsr d -> Dsr.to_string d
  | `Unknown u -> Unknown_esc.to_string u
;;

let to_string_hum = function
  | `Control c -> Control.to_string_hum c
  | `Style s -> Style.to_string_hum s
  | `Hyperlink h -> Hyperlink.to_string_hum h
  | `Private_mode p -> Private_mode.to_string_hum p
  | `Osc o -> Osc.to_string_hum o
  | `Dsr d -> Dsr.to_string_hum d
  | `Unknown u -> Unknown_esc.to_string_hum u
;;
