(** Pad the text to the given length with spaces or another character. *)
val pad
  :  ?char:char
  -> ?style:Style.t
  -> width:int
  -> Text_with_ansi.t
  -> Text_with_ansi.t

(** Center the text within the given length, using the given character (default=' '). The
    left side can end up with one fewer padding character than the right. *)
val center
  :  ?char:char
  -> ?style:Style.t
  -> width:int
  -> Text_with_ansi.t
  -> Text_with_ansi.t

(** Truncate the text at the given length. *)
val truncate : width:int -> Text_with_ansi.t -> Text_with_ansi.t

(** Wrap the text at the given length. *)
val wrap : width:int -> Text_with_ansi.t -> Text_with_ansi.t list

(** A string where the given styles are turned on at the start and off at the end. Note
    that this does not parse or minimize the string. *)
val apply : Style.t -> string -> string

(** Print a string with ANSI style codes converted into a more human-readable format. *)
val visualize : string -> string

(** Remove redundant ANSI style codes from a string. Note that this is best-effort, and
    the resulting string is not guaranteed to be globally minimal. *)
val minimize : string -> string

(** Remove all ANSI style codes from a string. *)
val strip : string -> string

(** Prepares a pair of strings for printing as two columns in a single buffer. Wraps both
    strings to the given width, then pads the shorter list to the same length with empty
    strings, then pads each left-side line to the width with spaces. *)
val to_double_column : width:int -> left:string -> right:string -> (string * string) list
