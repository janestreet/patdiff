(** Operating System Command (OSC) sequences.

    OSC sequences are commands sent to the terminal emulator (not the terminal screen).
    They start with
    {v ESC ] v}
    and are terminated by BEL (0x07) or ST (
    {v ESC \ v}
    ).

    Common OSC sequences:
    - {v ESC]0;title ST v}
      - Set window title and icon name
    - {v ESC]1;name ST v}
      - Set icon name
    - {v ESC]2;title ST v}
      - Set window title
    - {v ESC]7;file://host/path ST v}
    - Set current working directory

    We don't support all sequences, as most of them are rarely used. (for instance
    {v ESC]4;index;color ST v}
    , which changes color palette entry).

    Note: ANSI-encoded hyperlinks use an OSC, but we represent them with a completely
    separate type, since their function more-closely resembles text formatting than
    terminal emulation. *)

type command =
  | Set_title of string (** OSC 0, 2: Set window title *)
  | Set_icon_name of string (** OSC 1: Set icon name *)
  | Set_working_directory of string (** OSC 7: Set current working directory *)
  | Other of string (** Other OSC commands we don't specifically handle *)
[@@deriving compare ~localize, equal ~localize, sexp]

type t = { command : command }
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

val of_payload : string -> t
val to_string : t -> string
val to_string_hum : t -> string
