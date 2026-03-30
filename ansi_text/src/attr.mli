open! Core

(** Models a subset of the ANSI select-graphic-rendition attributes. The subset
    corresponds to attributes described in
    https://en.wikipedia.org/wiki/ANSI_escape_code#Select_Graphic_Rendition_parameters. *)

(** Grouped into related attributes that override one another, e.g. intensity, emphasis. *)
type t =
  | Reset
  | Bold (** intensity *)
  | Faint (** intensity *)
  | Normal_weight (** intensity *)
  | Italic (** emphasis *)
  | Fraktur (** emphasis (rarely supported) *)
  | Not_emphasis (** emphasis *)
  | Underline (** underline *)
  | Double_ul (** underline *)
  | Not_underline (** underline *)
  | Blink (** blink *)
  | Fast_blink (** blink (rarely supported) *)
  | Not_blink (** blink *)
  | Framed (** framing (rarely supported) *)
  | Encircled (** framing (rarely supported) *)
  | Not_framed (** framing (also cancels [Encircled]) *)
  | Superscript (** sub/super script (rarely supported) *)
  | Subscript (** sub/super script (rarely supported) *)
  | Not_script (** sub/super script *)
  | Invert (** invert fg/bg colors *)
  | Not_invert (** invert fg/bg colors *)
  | Hide (** hide text *)
  | Not_hide (** hide text *)
  | Strike (** strikethrough *)
  | Not_strike (** strikethrough *)
  | Overline (** overline *)
  | Not_overline (** overline *)
  | Variable_width (** proportional spacing (rarely supported) *)
  | Fixed_width (** proportional spacing *)
  | Fg of Color.t
  | Bg of Color.t
  | Ul_color of Color.t
  | Font of int (** 0-10; default font is 0 (rarely supported) *)
  | Ideogram of int (** 0-5; no ideograms is 5 (rarely supported) *)
  | Other of int list
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(** Gives an attribute that turns off the given atribute (if applicable). *)
val turn_off : t -> t option

(** Whether [new_attr] overrides the style effects of [old_attr]. For example, [Bold],
    [Faint], and [Normal_weight] all override one another. *)
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
