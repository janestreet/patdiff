type range =
  { start : int
  ; end_ : int
  ; style : Style.t
  }
[@@deriving compare, equal, quickcheck, sexp]

(*** An ordered collection of styles that each apply to a specific range of text. *)
type t = range list [@@deriving compare, equal, quickcheck, sexp]

(** Identify ranges for as many styles as possible. Also returns a list of the remaining
    unmatched styles. *)
val identify : Text_with_styles.t -> t * Style.t list

(** Make an [Ansi_text.t] with the given style-ranges *)
val apply : text:Text.t -> t -> Text_with_styles.t

(** Updates all ranges, moving their start and end by the given amounts (default = 0). *)
val adjust_by : ?start:int -> ?end_:int -> t -> t

(** Adjusts style-ranges to remove all styling between [start] and [end_]. *)
val exclude : start:int -> end_:int -> t -> t

(** Splits the ranges appropriately for splitting the text at a given position, producing
    two lists of ranges: those that end before and those that start after [pos], with
    starts/ends in the after-list adjusted accordingly. Ranges straddling [pos] are
    included in both lists, modified to start/end at the split. *)
val split : pos:int -> t -> t * t
