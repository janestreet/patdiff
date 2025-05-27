open! Core
open! Import

(** Heuristics for detecting files that are clearly binary. *)

val string : string -> bool
val array : string array -> bool
