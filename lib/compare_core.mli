open! Core
open Core_extended.Std

val diff_files
  :  Configuration.t
  -> old_file : string
  -> new_file : string
  -> [ `Different | `Same ]

val diff_dirs
  : Configuration.t
  -> old_file : string
  -> new_file : string
  -> file_filter:(Find.file_info -> bool) option
  -> [ `Different | `Same ]

(* diff strings and output to strings, supposed to be used by ocaml code *)
val diff_strings
  :  ?print_global_header:bool
  -> Configuration.t
  -> old  : Patdiff_core.diff_input
  -> new_ : Patdiff_core.diff_input
  -> [ `Different of string | `Same ]
