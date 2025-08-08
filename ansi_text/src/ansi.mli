type t =
  [ `Control of Control.t
  | `Style of Style.t
  ]
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

val of_string_exn : string -> t
val of_string_opt : string -> t option
