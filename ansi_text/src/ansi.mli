type t =
  [ `Control of Control.t
  | `Style of Style.t
  | `Hyperlink of Hyperlink.t
  | `Unknown of Unknown_esc.t
  ]
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(** Dispatches to [Style], [Control], or [Unknown] depending on the final char. *)
val of_csi : params:string -> terminal:char -> t

val to_string : t -> string
val to_string_hum : t -> string
