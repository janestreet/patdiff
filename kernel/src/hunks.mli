open! Core
open! Import

type t = string Patience_diff.Hunk.t list [@@deriving sexp_of]

val iter'
  :  f_hunk_break:(string Patience_diff.Hunk.t -> unit)
  -> f_line:(string -> unit)
  -> t
  -> unit

(** Iterates over output rows, reporting only side/line-number metadata. *)
val iter_sides
  :  f_hunk_break:('a Patience_diff.Hunk.t -> unit)
  -> f_line:(Format.Annotation_gutter.side -> unit)
  -> 'a Patience_diff.Hunk.t list
  -> unit

(** Like [iter'], but [f_line] is also given the [side] (kind plus prev/next line numbers
    within the hunk) of the line being emitted. *)
val iter_with_line_info
  :  f_hunk_break:(string Patience_diff.Hunk.t -> unit)
  -> f_line:(Format.Annotation_gutter.side -> string -> unit)
  -> t
  -> unit
