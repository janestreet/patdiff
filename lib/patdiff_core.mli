open Core.Std
open Patience_diff_lib.Std

module Format : sig

  module Color : sig

    type t =
        | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default
        | Gray
        | Bright_black | Bright_red | Bright_green | Bright_yellow | Bright_blue
        | Bright_magenta | Bright_cyan | Bright_white
        | Cmyk of float * float * float * float
    with sexp

  end

  module Style : sig

    type t =
        | Bold | Underline | Emph
        | Blink | Dim | Inverse | Hide
        | Reset
        | Foreground of Color.t | Fg of Color.t
        | Background of Color.t | Bg of Color.t
    with sexp

  end

  module Rule : sig

    module Annex : sig
      type t
      val create : ?styles: Style.t list -> string -> t
      val blank : t
    end

    type t
    val create : ?pre: Annex.t -> ?suf: Annex.t -> Style.t list -> name:string -> t
    val blank : name: string -> t
    val unstyled_prefix : string -> name:string -> t

  end

  module Rules : sig

    type t = {
      line_same: Rule.t;
      line_old: Rule.t;
      line_new: Rule.t;
      line_unified: Rule.t;
      word_same_old: Rule.t;
      word_same_new: Rule.t;
      word_same_unified: Rule.t;
      word_old: Rule.t;
      word_new: Rule.t;
      hunk: Rule.t;
      header_old: Rule.t;
      header_new: Rule.t;
    }

  end

end

module Output : sig
  type t = Ansi | Html with sexp
end

(** [diff ~context ~compare ~keep_ws a b] Use Patience_diff to get a list of
    hunks describing the comparison between [a] and [b] *)
val diff
  :  context : int
  -> compare : (string -> string -> int)
  -> keep_ws : bool
  -> mine:string array
  -> other:string array
  -> string Patience_diff.Hunk.t list

(** [refine diff format] takes the Replace ranges from the hunk list, splits
    them into smaller arrays, diffs those arrays, formats them according to
    the provided format, and recomposes the Replace range of the original
    hunk list. *)
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
  -> string Patience_diff.Hunk.t list
  -> unit

(** Output a hunk list, usually from [diff] or [refine], to a string *)
val output_to_string
  :  ?file_names:(string * string)
  -> rules:Format.Rules.t
  -> output:Output.t
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
