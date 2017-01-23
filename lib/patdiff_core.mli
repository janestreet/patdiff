open! Core.Std
open! Import

module Format : module type of (struct include Patdiff_format end)
module Output : module type of (struct include Output_mode end)

val default_context : int

(** [diff ~context ~keep_ws ~mine ~other] uses [Patience_diff] to get a list of hunks
    describing the comparison between [mine] and [other]. *)
val diff
  :  context : int
  -> keep_ws : bool
  -> mine:string array
  -> other:string array
  -> string Patience_diff.Hunk.t list

(* [remove_ws] calls String.strip and replaces whitespace with " " *)
val remove_ws : string -> string

(** [refine ~rules ~producte_unified_lines ~output ~keep_ws ~split_long_lines hunks] takes
    the Replace ranges from [hunks], splits them into smaller arrays, diffs those arrays,
    formats them according to [rules], and recomposes the Replace range of [hunks]. *)
val refine
  :  rules:Format.Rules.t
  -> produce_unified_lines:bool
  -> output:Output.t
  -> keep_ws:bool
  -> split_long_lines:bool
  -> string Patience_diff.Hunk.t list
  -> string Patience_diff.Hunk.t list

(** Print a hunk list, usually from [diff] or [refine] *)
val print
  :  old_file:string
  -> new_file:string
  -> rules:Format.Rules.t
  -> output:Output.t
  -> location_style:Format.Location_style.t
  -> string Patience_diff.Hunk.t list
  -> unit

(** Output a hunk list, usually from [diff] or [refine], to a string *)
val output_to_string
  :  ?print_global_header:bool
  -> file_names:(string * string)
  -> rules:Format.Rules.t
  -> output:Output.t
  -> location_style:Format.Location_style.t
  -> string Patience_diff.Hunk.t list
  -> string

(** Iter along the lines of the diff and the breaks between hunks. Offers more flexibility
    regarding what the caller wants to do with the lines *)
val iter_ansi
  :  rules:Format.Rules.t
  -> f_hunk_break:((int*int) -> (int*int) -> unit)
  -> f_line:(string -> unit)
  -> string Patience_diff.Hunk.t list
  -> unit

type diff_input =
  { name : string
  ; text : string
  }

(** Runs the equivalent of the command line version of patdiff on two given contents
    [from_] and [to_]. *)
val patdiff
  :  ?context               : int
  -> ?keep_ws               : bool
  -> ?rules                 : Format.Rules.t
  -> ?output                : Output.t
  -> ?produce_unified_lines : bool
  -> ?split_long_lines      : bool
  -> ?print_global_header   : bool
  -> ?location_style        : Format.Location_style.t
  -> from_                  : diff_input
  -> to_                    : diff_input
  -> unit
  -> string
