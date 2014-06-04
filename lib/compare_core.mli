open Core.Std
open Core_extended.Std

val array_of_file :
  string ->
  string array

val diff_files :
  Configuration.t ->
  old_file: string ->
  new_file: string ->
  [ `Different | `Same ]

val diff_dirs :
  Configuration.t ->
  old_file: string ->
  new_file: string ->
  file_filter:(Find.file_info -> bool) option ->
  [ `Different | `Same ]

(* diff strings and output to strings, supposed to be used by ocaml code *)
val diff_strings :
  Configuration.t ->
  old: string ->
  new_: string ->
  [ `Different of string | `Same ]
