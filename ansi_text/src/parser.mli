open! Core

(** Parse a string (possibly) containing ANSI escape sequences into structured form.
    Malformed sequences are captured as [Unknown] or [Other] in the result. The resulting
    [Text_with_ansi.t] can always be converted back to a semantically equivalent string
    via [Text_with_ansi.to_string]. *)
val parse : string -> Text_with_ansi.t
