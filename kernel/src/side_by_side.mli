open! Core
open Patience_diff_lib.Patience_diff

module Line : sig
  (** A line in the previous file will only ever have [`Prev] and [`Same] while lines in
      the next file will only have [`Same] and [`Next].

      This means all deletions will be denoted on the left (previous) side and all
      additions will be denoted on the right (next) side.

      [line_number] is the number of the line in the original file.
  *)
  type t =
    { line_number : int
    ; contents : ([ `Next | `Prev | `Same ] * string) list
    }
  [@@deriving sexp_of]

  val unstyled_string : t -> string
end

module Line_info : sig
  type t =
    | Same of Line.t * Line.t
    | Prev of Line.t * Move_id.t option
    | Next of Line.t * Move_id.t option
  [@@deriving sexp_of]
end

(** Take structured hunks and produce hunks of [Line_info]. The elements of outer array
    represent the original hunks passed in and the inner elements are the lines within the
    hunk. *)
val hunks_to_lines
  :  ([ `Next | `Prev | `Same ] * string) list Hunk.t list
  -> Line_info.t array array
