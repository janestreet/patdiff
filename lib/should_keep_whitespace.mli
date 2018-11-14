open! Core

(** Heuristics for whitespace-sensitive inputs (e.g., if it looks like Python). *)

val for_diff
  :  file1:string
  -> file2:string
  -> contents1:string
  -> contents2:string
  -> bool
