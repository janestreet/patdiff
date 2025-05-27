(** Represents a string that doesn't contain ANSI codes, but may contain unicode chars. *)
type t [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(** Printable width, as estimated using [Uucp_break.tty_width_hint]. *)
val width : t -> int

val is_empty : t -> bool
val of_string : string -> t
val to_string : t -> string

(** If [pos] falls within a multi-column char, that char will go after the split. *)
val split : t -> pos:int -> t * t

val ( ^ ) : t -> t -> t
val concat : t list -> t
