open! Core

(** Models the color-related ANSI codes. *)

module Sgr8 : sig
  (** Models the basic codes that appear in the following SGR ranges

      30-37 - foreground color 40-47 - background color 90-97 - bright foreground color
      100-107 - bright background color *)
  type t =
    | Black (* 0 *)
    | Red (* 1 *)
    | Green (* 2 *)
    | Yellow (* 3 *)
    | Blue (* 4 *)
    | Magenta (* 5 *)
    | Cyan (* 6 *)
    | White (* 7 *)
  [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

  (** code in 0-7 *)
  val of_code_exn : int -> t

  val to_code : t -> int

  (** human-readable name *)
  val to_string_hum : t -> string
end

(** Models the rgb subset of the codes under "38;5" and "48;5". *)
module Rgb6 : sig
  type t [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

  (** r,g,b in 0-5 *)
  val of_rgb_exn : int * int * int -> t

  (** code in 16-231 *)
  val of_code_exn : int -> t

  val to_rgb : t -> int * int * int
  val to_code : t -> int

  (** somewhat human-readable name *)
  val to_string_hum : t -> string
end

(** Models the grayscale subset of the codes under "38;5" and "48;5". *)
module Gray24 : sig
  type t [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

  (** level in 0-23 *)
  val of_level_exn : int -> t

  (** code in 232-255 *)
  val of_code_exn : int -> t

  val to_level : t -> int
  val to_code : t -> int

  (** somewhat human-readable name *)
  val to_string_hum : t -> string
end

(** Models the rgb codes under "38;2" and "48;2". *)
module Rgb256 : sig
  type t [@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

  (** r,g,b in 0-255 *)
  val of_rgb_exn : int * int * int -> t

  val to_rgb : t -> int * int * int

  (** somewhat human-readable name *)
  val to_string_hum : t -> string
end

(** Unifies all the ANSI color types. *)
type t =
  | Default
  | Standard of Sgr8.t
  | Bright of Sgr8.t
  | Rgb6 of Rgb6.t
  | Gray24 of Gray24.t
  | Rgb256 of Rgb256.t
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

val to_fg_code : t -> int list
val to_bg_code : t -> int list
val to_ul_code : t -> int list

(** Generates a [Standard] or [Bright] color based on an integer 0-15. *)
val sgr8_exn : ?bright:bool -> int -> t

(** Generates an [Rgb6] color based on r, g, and b integers 0-5. *)
val rgb6_exn : int * int * int -> t

(** Generates a [Gray24] color based on an integer 232-255. *)
val gray24_exn : int -> t

(** Generates an [Rgb256] color based on r, g, and b integers 0-255. *)
val rgb256_exn : int * int * int -> t

(** A somewhat human-readable name for a color. *)
val to_string_hum : t -> string

val black : t
val red : t
val green : t
val yellow : t
val blue : t
val magenta : t
val cyan : t
val white : t
val gray : t

(** Returns [white], [black] or [default] to be used as the fg color for the given bg.
    Attempts to use [white] for dark backgrounds, [black] for light backgrounds, and
    [default] in between. *)
val default_text_color_for : bg:t -> t
