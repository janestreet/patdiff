type formatting =
  [ `Style of Style.t
  | `Hyperlink of Hyperlink.t
  ]

type emulation =
  [ `Private_mode of Private_mode.t
  | `Osc of Osc.t
  | `Dsr of Dsr.t
  ]

type t =
  [ formatting
  | emulation
  | `Control of Control.t
  | `Unknown of Unknown_esc.t
  ]
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(** Dispatches to [Style], [Control], [Private_mode], [Dsr], or [Unknown] depending on the
    parameters string and the initial and final characters. *)
val of_csi : ?private_prefix:char -> string -> terminal:char -> t

val of_osc_payload : string -> t
val to_string : t -> string
val to_string_hum : t -> string
