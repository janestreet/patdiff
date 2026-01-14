open! Core

(** OSC-8 hyperlink representation. Supporting terminals render text between a [Start] and
    an [End] as a clickable link. *)
type t =
  | Start of string (** The string encodes the url. *)
  | End
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

val to_string : t -> string
val to_string_hum : t -> string
