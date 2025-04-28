open! Core
open! Import

module type S = sig
  (** [diff ~context ~keep_ws ~prev ~next] uses [Patience_diff.String] to get a list of
      hunks describing the comparison between [prev] and [next]. *)
  val diff
    :  context:int
    -> line_big_enough:int
    -> keep_ws:bool
    -> find_moves:bool
    -> prev:string array
    -> next:string array
    -> Hunks.t

  val find_moves : line_big_enough:int -> keep_ws:bool -> Hunks.t -> Hunks.t

  (** [refine hunks] maps each [Range.Replace (prev, next)] in [hunks] to a diff of [prev]
      against [next]. *)
  val refine
    :  rules:Format.Rules.t
    -> produce_unified_lines:bool
    -> output:Output.t
    -> keep_ws:bool
    -> split_long_lines:bool
    -> interleave:bool
    -> word_big_enough:int
    -> Hunks.t
    -> Hunks.t

  (** The same as [refine] except returns structured data. *)
  val refine_structured
    :  produce_unified_lines:bool
    -> keep_ws:bool
    -> split_long_lines:bool
    -> interleave:bool
    -> word_big_enough:int
    -> string Patience_diff.Hunk.t list
    -> ([ `Next | `Prev | `Same ] * string) list Patience_diff.Hunk.t list

  val explode
    :  string array
    -> keep_ws:bool
    -> [ `Newline of int * string option | `Word of string ] array

  (** Print a hunk list, usually from [diff] or [refine] *)
  val print
    :  file_names:File_name.t * File_name.t
    -> rules:Format.Rules.t
    -> output:Output.t
    -> location_style:Format.Location_style.t
    -> Hunks.t
    -> unit

  (** Builds the sides of the diff, returning a list of lines. Each line is a pair of
      strings representing the contents of the left and right columns. *)
  val build_side_by_side
    :  ?width_override:int
    -> ?f_hunk_break:(width:int -> int * int -> int * int -> string list * string list)
    -> ?file_names:File_name.t * File_name.t
    -> ?include_line_numbers:bool
    -> rules:Format.Rules.t
    -> wrap_or_truncate:[ `wrap | `truncate | `neither ]
    -> output:Output.t
    -> ([ `Next | `Prev | `Same ] * string) list Patience_diff.Hunk.t list
    -> (string * string) list list

  val print_side_by_side
    :  ?width_override:int
    -> file_names:File_name.t * File_name.t
    -> rules:Format.Rules.t
    -> wrap_or_truncate:[ `wrap | `truncate ]
    -> output:Output.t
    -> ([ `Next | `Prev | `Same ] * string) list Patience_diff.Hunk.t list
    -> unit

  (** Output a hunk list, usually from [diff] or [refine], to a string *)
  val output_to_string
    :  ?print_global_header:bool
    -> file_names:File_name.t * File_name.t
    -> rules:Format.Rules.t
    -> output:Output.t
    -> location_style:Format.Location_style.t
    -> Hunks.t
    -> string

  val output_to_string_side_by_side
    :  ?width_override:int
    -> file_names:File_name.t * File_name.t
    -> rules:Format.Rules.t
    -> wrap_or_truncate:[ `truncate | `wrap ]
    -> output:Output.t
    -> ([ `Next | `Prev | `Same ] * string) list Patience_diff.Hunk.t list
    -> string

  (** Iter along the lines of the diff and the breaks between hunks. Offers more
      flexibility regarding what the caller wants to do with the lines *)
  val iter_output
    :  rules:Format.Rules.t
    -> f_hunk_break:(int * int -> int * int -> unit)
    -> f_line:(string -> unit)
    -> ?output:Output.t
    -> Hunks.t
    -> unit

  (** Same as [iter_output] but for side-by-side diffs *)
  val iter_side_by_side_ansi
    :  ?width_override:int
    -> rules:Format.Rules.t
    -> f_hunk_break:(width:int -> int * int -> int * int -> string list * string list)
    -> f_line:(string -> unit)
    -> wrap_or_truncate:[ `truncate | `wrap ]
    -> ([ `Next | `Prev | `Same ] * string) list Patience_diff.Hunk.t list
    -> unit

  val output_width : ?width_override:int -> unit -> int

  (** Runs the equivalent of the command line version of patdiff on two given contents
      [prev] and [next]. Uses [Patience_diff.String]. *)
  val patdiff
    :  ?context:int
    -> ?keep_ws:bool
    -> ?find_moves:bool
    -> ?rules:Format.Rules.t
    -> ?output:Output.t
    -> ?produce_unified_lines:bool
    -> ?split_long_lines:bool
    -> ?print_global_header:bool
    -> ?location_style:Format.Location_style.t
    -> ?interleave:bool
    -> ?float_tolerance:Percent.t
    -> ?line_big_enough:int
    -> ?word_big_enough:int
    -> prev:Diff_input.t
    -> next:Diff_input.t
    -> unit
    -> string
end

module type Output_impls = sig
  val implementation : Output.t -> (module Output.S)
  val console_width : unit -> int Or_error.t
end

module type Patdiff_core = sig
  module type S = S

  val default_context : int
  val default_line_big_enough : int
  val default_word_big_enough : int

  (** [remove_ws] calls String.strip and replaces whitespace with " " *)
  val remove_ws : string -> string

  module Private : sig
    module Make (Output_impls : Output_impls) : S
  end

  module Without_unix : S
end
