open! Core
open! Import

(** Patdiff_format is the home of all the internal representations of the formatting that
    will be applied to the diff. ie. prefixes, suffixes, & valid styles. *)

module Color = Ansi_text.Color
module Style = Ansi_text.Attr

(** A rule consists of a styled prefix, a styled suffix, and a style. Rules are applied to
    strings using functions defined in Output_ops. *)
module Rule : sig
  (** An affix is either a prefix or a suffix. *)
  module Affix : sig
    type t =
      { text : string
      ; styles : Style.t list
      }

    val create : ?styles:Style.t list -> string -> t
    val blank : t
  end

  type t =
    { pre : Affix.t
    ; suf : Affix.t
    ; styles : Style.t list
    }
  [@@deriving sexp_of]

  (** Rule creation: Most rules have a style, and maybe a prefix. For instance, a
      line_next rule might have a bold "+" prefix and a green style. *)
  val create : ?pre:Affix.t -> ?suf:Affix.t -> Style.t list -> t

  val blank : t
  val unstyled_prefix : string -> t
  val strip_styles : t -> t
  val strip_prefix : t -> t
  val use_prefix_text_from : t -> this_prefix:t -> t
end

(** Rules are configured in the configuration file. Default values are provided in
    Configuration. *)
module Rules : sig
  type t =
    { line_same : Rule.t
    ; line_prev : Rule.t
    ; line_next : Rule.t
    ; line_unified : Rule.t
    ; word_same_prev : Rule.t
    ; word_same_next : Rule.t
    ; word_same_unified : Rule.t
    ; word_same_unified_in_move : Rule.t
    ; word_prev : Rule.t
    ; word_next : Rule.t
    ; hunk : Rule.t
    ; header_prev : Rule.t
    ; header_next : Rule.t
    ; moved_from_prev : Rule.t
    ; moved_to_next : Rule.t
    ; removed_in_move : Rule.t
    ; added_in_move : Rule.t
    ; line_unified_in_move : Rule.t
    }
  [@@deriving compare ~localize, sexp_of, fields ~iterators:map]

  module Color_palette : sig
    type t =
      { added : Color.t
      ; removed : Color.t
      ; moved_from_prev : Color.t
      ; moved_to_next : Color.t
      }

    val default : t
  end

  val default : t
  val default_with_color_palette : Color_palette.t -> t
  val strip_styles : t -> t
end

module Location_style : sig
  type t =
    | Diff
    | Omake
    | None
    | Separator
  [@@deriving bin_io, compare ~localize, quickcheck, enumerate, equal ~localize, sexp]

  include Stringable.S with type t := t

  val omake_style_error_message_start : file:string -> line:int -> string

  val sprint
    :  t
    -> string Patience_diff.Hunk.t
    -> prev_filename:string
    -> rule:(string -> string)
    -> string
end
