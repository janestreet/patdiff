open! Core

type element =
  [ Ansi.t
  | `Text of Text.t
  ]
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(** The primary representation of text with ANSI codes. We should be able to parse any
    string into this type. *)
type t = element list [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(** The total length of all [Text] elements in an [Ansi_text.t]. *)
val width : t -> int

(** Whether the [Ansi_text.t] has width 0. *)
val is_empty : t -> bool

(** A string that includes all text and all ANSI codes. *)
val to_string : t -> string

(** A string where ANSI codes have been replaced by more-human-readable names. *)
val to_string_hum : t -> string

(** A string where all ANSI codes have been removed. *)
val to_unstyled : t -> string

(** Map over the elements; if [f] outputs [None] the element is unchanged. This makes it
    easy to write an [f] that applies only to [`Style] elements, for example. *)
val map : f:(element -> element option) -> t -> t

(** Best-effort to reduce the ansi-codes needed to express the same styles. *)
val simplify_styles : t -> t

(** Determines what styles are active at the end of the string. Assumes there are no
    styles active at the start of the text. *)
val style_at_end : t -> Style.t

(** Split at an index in the printable text, terminating and restoring active styles. *)
val split : pos:int -> t -> t * t
