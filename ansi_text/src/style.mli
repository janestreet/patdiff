open! Core

(** Models an ANSI escape sequence that can modify several attributes. *)
type t = Attr.t list [@@deriving compare, equal, quickcheck, sexp]

(** Gives a style that turns off all attributes that are turned on by the given style. *)
val turn_off : t -> t

(** Parses an escape string into a [Style.t]. Raises on various malformed strings. For
    example
    {v "\027[1;38;5;1m" -> [Bold; Fg Red] v} *)
val of_string_exn : string -> t

(** Converts a [Style.t] to a string. For example
    {v [Bold; Fg Standard Red] -> "\027[1;31m" v} *)
val to_string : t -> string

(** Simplifies a [Style.t] by removing redundant attributes. For example
    [Bold; Bg Blue; Reset; Fg Red; Fg Green] -> [Reset; Fg Green]. This runs in O(n^2)
    time, but should be ok since [Style] lists are generally short. *)
val compress : t -> t

(** Style that results from applying [added_style] when [old_style] is already active. *)
val update : old_style:t -> added_style:t -> t

(** A minimal style that would transition the active state from [old_style] to
    [old_style] + [additional_style]. *)
val delta : old_style:t -> added_style:t -> t

(** Lists the attributes in a somewhat human-readable style. *)
val to_string_hum : t -> string

(** Whether any of the attributes are [Reset]. *)
val includes_reset : t -> bool

(** Whether final attribute is [Reset]. *)
val is_reset : t -> bool

(** Whether [new_style] overrides all attributes that [old_style] set. *)
val closes : new_style:t -> old_style:t -> bool
