(** Alternative representation that's only valid if all styles have clear start and end
    positions. This type is far less general than [Text_with_styles.t], and somewhat less
    thoroughly tested. Its potential use is for better splitting of ansi text and for
    translating to html. *)
type t =
  { text : Text.t
  ; ranges : Style_ranges.t
  }
[@@deriving compare, equal, quickcheck, sexp]

val width : t -> int
val is_empty : t -> bool
val to_string : t -> string
val to_string_hum : t -> string
val to_unstyled : t -> string
val to_text_with_styles : t -> Text_with_styles.t
val of_text_with_styles : Text_with_styles.t -> t option

(** Removes styles between [start] and [end_], shortening ranges as necessary. *)
val unstyle_between : start:int -> end_:int -> t -> t

val split : pos:int -> t -> t * t
