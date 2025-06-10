(** Alternative representation that's only valid if all styles have clear start and end
    positions. This type is far less general than [Text_with_ansi.t], and somewhat less
    thoroughly tested. Its intended use case is re-rendering patdiff output for the
    web-ui. *)
type t =
  { text : Text.t
  ; ranges : Style_ranges.t
  }
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

val width : t -> int
val is_empty : t -> bool
val to_string : t -> string
val to_string_hum : t -> string
val to_unstyled : t -> string
val to_text_with_ansi : t -> Text_with_ansi.t
val of_text_with_ansi : Text_with_ansi.t -> t option

(** Removes styles between [start] and [end_], shortening ranges as necessary. *)
val unstyle_between : start:int -> end_:int -> t -> t

val split : pos:int -> t -> t * t
