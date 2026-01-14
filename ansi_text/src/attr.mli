open! Core

(** Models a subset of the ANSI select-graphic-rendition attributes. The subset
    corresponds to attributes described in
    https://en.wikipedia.org/wiki/ANSI_escape_code#Select_Graphic_Rendition_parameters
    that work in utop in a linux terminal. *)

type t =
  | Reset
  | Bold
  | Faint
  | Italic
  | Underline
  | Blink
  | Fast_blink
  | Double_ul
  | Invert
  | Hide
  | Strike
  | Overline
  | Not_bold_or_faint
  | Not_italic
  | Not_underline
  | Not_blink
  | Not_invert
  | Not_hide
  | Not_strike
  | Not_overline
  | Fg of Color.t
  | Bg of Color.t
  | Ul_color of Color.t
  | Other of int list
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(** Gives an attribute that turns off the given atribute (if applicable). *)
val turn_off : t -> t option

(** Whether [new_attr] overrides the style effects of [old_attr]. *)
val overrides : new_attr:t -> old_attr:t -> bool

(** Creates an [Attr.t] from a list of SGR parameter codes. Known codes (like [1] for bold
    or [38;5;196] for 256-color red foreground) are parsed into specific variants.
    Unrecognized [codes] are returned as [Other codes]. *)
val of_codes : int list -> t

(** Converts an [Attr.t] to an ANSI code. *)
val to_code : t -> int list

(** Converts an [Attr.t] to a string. For example [Fg Standard Red] -> "31". *)
val to_string : t -> string

(** A somewhat human-readable name for what the attribute is changing. *)
val to_string_hum : t -> string
