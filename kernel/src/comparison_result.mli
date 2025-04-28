open! Core
open! Import

type t =
  | Binary_same
  | Binary_different of
      { prev_is_binary : bool
      ; next_is_binary : bool
      }
  | Hunks of Hunks.t
  | Structured_hunks of
      ([ `Next | `Prev | `Same ] * string) list Patience_diff.Hunk.t list

val create
  :  Configuration.t
  -> prev:Diff_input.t
  -> next:Diff_input.t
       (** This configuration may differ from what was passed into [create], depending on
           heuristics that consider [prev] and [next]. *)
  -> compare_assuming_text:
       (Configuration.t
        -> prev:Diff_input.t
        -> next:Diff_input.t
        -> [ `Hunks of Hunks.t
           | `Structured_hunks of
             ([ `Next | `Prev | `Same ] * string) list Patience_diff.Hunk.t list
           ])
  -> t

val has_no_diff : t -> bool
