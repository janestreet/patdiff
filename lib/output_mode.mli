open! Core
open! Import

(** Ascii is Ansi with no styles. *)
type t =
  | Ansi
  | Ascii
  | Html
[@@deriving compare, sexp]

val implies_unrefined : t -> bool
