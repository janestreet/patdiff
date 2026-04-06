(** Device Status Report (DSR) sequences.

    DSR sequences are queries sent to the terminal to request information. They are CSI
    sequences ending with 'n'.

    Common DSR sequences:
    - {v ESC[5n v}
      - Device status query (terminal responds with
        {v ESC[0n v}
        if OK)
    - {v ESC[6n v}
      - Cursor position query (terminal responds with
        {v ESC[row;colR v}
        )

    These are query sequences that expect a response from the terminal emulator. *)

type query =
  | Device_status (** {v ESC[5n v}
                      - Query device status *)
  | Cursor_position (** {v ESC[6n v}
                        - Query cursor position *)
  | Other of int (** Other DSR query with numeric parameter *)
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

type t = { query : query }
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

val of_params : int option list -> t option
val to_string : t -> string
val to_string_hum : t -> string
