open! Core
open! Import

(** Heuristics for detecting files that are clearly not UTF-8 compliant. *)

val clearly_not_utf8 : lines : string array -> bool
