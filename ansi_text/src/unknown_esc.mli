(** Escape sequences we recognize structurally but don't interpret as [Ansi_text]. *)

type t =
  | Csi of string
  (** CSI sequence that we don't parse as [Style], [Control], [Private_mode], or [Dsr].
      https://en.wikipedia.org/wiki/ANSI_escape_code#Control_Sequence_Introducer_commands *)
  | Fe of char
  (** Fe sequence (ESC + 0x40-0x5F) that we don't parse as CSI or OSC.
      https://en.wikipedia.org/wiki/ANSI_escape_code#Fe_Escape_sequences *)
  | Fp of char
  (** Fp sequence: ESC + 0x60-0x7E.
      https://en.wikipedia.org/wiki/ANSI_escape_code#Fp_Escape_sequences *)
  | Nf of string
  (** nF sequence: ESC + intermediates (0x20-0x2F) + final (0x30-0x7E).
      https://en.wikipedia.org/wiki/ANSI_escape_code#nF_Escape_sequences *)
  | Incomplete (** We parsed an ESC not followed by any of the above. *)
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(** Construct a [Csi] by recombining the components the parser split apart. *)
val of_csi : ?private_prefix:char -> string -> terminal:char -> t

val to_string : t -> string
val to_string_hum : t -> string
