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
  | Other of int
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(** Gives an attribute that turns off the given atribute (if applicable). *)
val turn_off : t -> t option

(** Whether [new_attr] overrides the style effects of [old_attr]. *)
val overrides : new_attr:t -> old_attr:t -> bool

(** Creates an [Attr.t] from integers 0-255. Integers outside 0-255 produce an exception,
    as do lists of length > 1 that don't correspond to a color code. Valid color codes
    start with 38, 48, or 58, followed by either [5;n] or [2;r;g;b]. *)
val of_code_exn : int list -> t

(** Converts an [Attr.t] to an ANSI code. *)
val to_code : t -> int list

(** Converts an [Attr.t] to a string. For example [Fg Standard Red] -> "31". *)
val to_string : t -> string

(** A somewhat human-readable name for what the attribute is changing. *)
val to_string_hum : t -> string
