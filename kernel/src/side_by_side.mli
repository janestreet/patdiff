open! Core
open Patience_diff_lib.Patience_diff

module Line : sig
  (** A line in the previous file will only ever have [`Prev] and [`Same] while lines in
      the next file will only have [`Same] and [`Next].

      This means all deletions will be denoted on the left (previous) side and all
      additions will be denoted on the right (next) side.

      [line_number] is the line-number in the original file. *)
  type t =
    { line_number : int option
    ; contents : ([ `Next | `Prev | `Same ] * Ansi_text.t) list
    }
  [@@deriving sexp_of]

  val to_string : t -> string

  (** [~style] gets applied to each phrase, then phrases are concatenated. *)
  val styled_string
    :  ?output:Output.t
    -> style:([ `Next | `Prev | `Same ] -> string -> string)
    -> t
    -> string

  (** Line-number in the original file. Should only be [None] for an empty line produced
      when padding to match alignment. *)
  val line_number : t -> int option

  (** The width of the line if it were printed in the terminal. *)
  val width : t -> int

  val wrap : t -> width:int -> t list
  val truncate : t -> width:int -> t
  val any_non_same : t -> bool
end

module Line_info : sig
  type t =
    | Same of Line.t * Line.t
    | Prev of Line.t * Move_id.t option
    | Next of Line.t * Move_id.t option
  [@@deriving sexp_of]

  (** In the case of a [Same] line, it may be necessary to pad one side with empty lines. *)
  val wrap : width:int -> t -> t list

  val truncate : width:int -> t -> t

  (** Produces an empty line on one side if necessary. [Move_id.t]s are dropped. *)
  val lines : t -> Line.t * Line.t

  val numbers : t -> int option * int option
end

(** Take structured hunks and produce hunks of [Line_info]. The elements of outer array
    represent the original hunks passed in and the inner elements are the lines within the
    hunk. *)
val hunks_to_lines
  :  ([ `Next | `Prev | `Same ] * string) list Hunk.t list
  -> Line_info.t array array
