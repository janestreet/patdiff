(** DEC Private Mode sequences (ESC \[ ? ... h/l).

    Private mode sequences control terminal emulator features that don't affect visible
    text content, but affect certain other aspects of a terminal emulator. They start with
    {v ESC[? v}
    and end with 'h' (set/enable) or 'l' (reset/disable).

    Common examples:
    - {v ESC[?25h / ESC[?25l v}
      - Show/hide cursor
    - {v ESC[?1049h / ESC[?1049l v}
      - Alternate screen buffer (XTerm)
    - {v ESC[?47h / ESC[?47l v}
      - Alternate screen buffer (older terminals)
    - {v ESC[?2004h / ESC[?2004l v}
      - Bracketed paste mode
    - {v ESC[?1h / ESC[?1l v}
      - Application cursor keys mode
    - {v ESC[?7h / ESC[?7l v}
      - Auto-wrap mode

    See: https://invisible-island.net/xterm/ctlseqs/ctlseqs.html *)

type action =
  | Set (** Enable the mode (h) *)
  | Reset (** Disable the mode (l) *)
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

type t =
  { modes : int list (** The mode number(s) being set or reset *)
  ; action : action
  }
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(** Make a [Private_mode.t] from parsed CSI parameters if possible. For example the
    parser, when handling:
    {v "\027[?25h" v}
    would invoke:
    {v of_csi_params [Some 25] ~terminal:'h' v}
    yielding:
    {v { modes = [ 25 ] ; action = Set } v}
    Returns [None] if the terminal is not 'h' or 'l', or if no valid mode numbers are
    found in [params]. *)
val of_csi_params : int option list -> terminal:char -> t option

(** Whether this is an alternate screen buffer sequence (modes 47 or 1049). *)
val is_alternate_screen : t -> bool

val to_string : t -> string
val to_string_hum : t -> string
