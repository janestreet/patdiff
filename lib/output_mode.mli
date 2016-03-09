open! Core.Std
open! Import

(** Ascii is Ansi with no styles. *)
type t =
  | Ansi
  | Ascii
[@@deriving sexp]

val implies_unrefined : t -> bool
