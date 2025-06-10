open Core

type t =
  [ `Control of Control.t
  | `Style of Style.t
  ]
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

let of_string_exn str =
  match String.suffix str 1 with
  | "m" -> `Style (Style.of_string_exn str)
  | _ -> `Control (Control.of_string_exn str)
;;

let of_string_opt str = Option.try_with (fun () -> of_string_exn str)
