(** Variant for EraseDisplay options (ANSI-CSI 0-3 J) *)
type clear_screen

(** Variant for EraseLine options (ANSI-CSI 0-2 K) *)
type clear_line

(** Represents a subset of the ANSI-CSI spec:
    https://en.wikipedia.org/wiki/ANSI_escape_code#Control_Sequence_Introducer_commands *)
type t =
  | CursorUp of int option (* A *)
  | CursorDown of int option (* B *)
  | CursorForward of int option (* C *)
  | CursorBackward of int option (* D *)
  | CursorNextLine of int option (* E *)
  | CursorPrevLine of int option (* F *)
  | CursorToCol of int option (* G *)
  | CursorToPos of int option * int option (* H *)
  | EraseDisplay of clear_screen option (* J *)
  | EraseLine of clear_line option (* K *)
  | ScrollUp of int option (* S *)
  | ScrollDown of int option (* T *)
  | Unknown of string
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(** Make a [Control.t] from bytes extracted from a parsed string. For example the parser,
    when handling:
    {v "\027[;5H" v}
    would invoke:
    {v of_csi ~params:";5" ~terminal:'H' v}
    yielding:
    {v CursorToPos (None, Some 5) v} *)
val of_csi
  :  params:string (** CSI parameter bytes in 0x30-0x3F. Generaly digits and semicolons. *)
  -> terminal:char (** CSI final byte in 0x40-0x7E *)
  -> t

(** Converts a [Control.t] back to an ANSI-CSI string. For example
    {[
      to_string (CursorToCol (Some 12)) = "\027[12G"
    ]} *)
val to_string : t -> string

(** A somewhat human-readable name for what the control sequence does. *)
val to_string_hum : t -> string
