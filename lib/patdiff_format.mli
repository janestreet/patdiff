open! Core.Std
open! Import

(** Patdiff_format is the home of all the internal representations of the formatting
    that will be applied to the diff. ie. prefixes, suffixes, & valid styles. *)

module Color : sig

  type t =
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default
    | Gray
    | Bright_black | Bright_red | Bright_green | Bright_yellow | Bright_blue
    | Bright_magenta | Bright_cyan | Bright_white
    | RGB6 of ANSITerminal.RGB6.t
    | Gray24 of ANSITerminal.Gray24.t
  [@@deriving compare, sexp]

  include Comparable.S with type t := t

  (** From the documentation of Ansi_terminal.ANSITerminal:

      RGB(r,g,b):    expected (0 ≤ r, g, b < 6)
      GRAY24(level): expected (0 ≤ level < 24)

      [rgb6_exn r g b] and [gray24_exn level] raise if the values are out of bound.
  *)
  val rgb6_exn   : int * int * int -> t
  val gray24_exn : int -> t
end

module Style : sig

  type t =
    | Bold | Underline | Emph
    | Blink | Dim | Inverse | Hide
    | Reset
    | Foreground of Color.t | Fg of Color.t
    | Background of Color.t | Bg of Color.t
  [@@deriving compare, sexp]

  include Comparable.S with type t := t
end

module Rule : sig

  module Annex : sig
    type t = private
      { text   : string
      ; styles : Style.t list
      }

    val create : ?styles: Style.t list -> string -> t
    val blank : t
  end

  type t = private
    { pre    : Annex.t
    ; suf    : Annex.t
    ; styles : Style.t list
    ; name   : string
    }
  [@@deriving sexp_of]

  val create : ?pre: Annex.t -> ?suf: Annex.t -> Style.t list -> name:string -> t
  val blank : name: string -> t
  val unstyled_prefix : string -> name:string -> t
  val strip_styles : t -> t
end

module Rules : sig

  type t =
    { line_same         : Rule.t
    ; line_old          : Rule.t
    ; line_new          : Rule.t
    ; line_unified      : Rule.t
    ; word_same_old     : Rule.t
    ; word_same_new     : Rule.t
    ; word_same_unified : Rule.t
    ; word_old          : Rule.t
    ; word_new          : Rule.t
    ; hunk              : Rule.t
    ; header_old        : Rule.t
    ; header_new        : Rule.t
    }
  [@@deriving sexp_of]

  val default : t
  val strip_styles : t -> t
end

module Location_style : sig
  type t =
    | Diff
    | Omake
  [@@deriving bin_io, compare, enumerate, sexp]

  include Stringable.S with type t := t

  val sprint
    : t
    -> string Patience_diff.Hunk.t
    -> mine_filename:string
    -> rule:(string -> string)
    -> string
end
