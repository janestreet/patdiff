(*********************************************************
   File: ANSITerminal.mli

   Copyright 2004 Troestler Christophe
   Christophe.Troestler(at)umh.ac.be

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*********************************************************)
(** This module offers basic control of ANSI compliant terminals.

  @author Christophe Troestler
  @version 0.3
*)

(** {2 Color} *)

type color =
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  | Default (** Default color of the terminal *)
  | Bright_black | Bright_red | Bright_green | Bright_yellow
  | Bright_blue | Bright_magenta | Bright_cyan | Bright_white

(** Various styles for the text.  [Blink] and [Hidden] may not work on
    every terminal. *)
type style =
  | Reset
  | Bold | Underlined | Dim | Blink | Inverse | Hidden
  | Bold_off | Underlined_off | Dim_off | Blink_off | Inverse_off | Hidden_off
  | Foreground of color
  | Background of color

val black             : style (** Shortcut for [Foreground Black]          *)
val red               : style (** Shortcut for [Foreground Red]            *)
val green             : style (** Shortcut for [Foreground Green]          *)
val yellow            : style (** Shortcut for [Foreground Yellow]         *)
val blue              : style (** Shortcut for [Foreground Blue]           *)
val magenta           : style (** Shortcut for [Foreground Magenta]        *)
val cyan              : style (** Shortcut for [Foreground Cyan]           *)
val white             : style (** Shortcut for [Foreground White]          *)
val default           : style (** Shortcut for [Foreground Default]        *)

val bright_black      : style (** Shortcut for [Foreground Bright_black]   *)
val bright_red        : style (** Shortcut for [Foreground Bright_red]     *)
val bright_green      : style (** Shortcut for [Foreground Bright_green]   *)
val bright_yellow     : style (** Shortcut for [Foreground Bright_yellow]  *)
val bright_blue       : style (** Shortcut for [Foreground Bright_blue]    *)
val bright_magenta    : style (** Shortcut for [Foreground Bright_magenta] *)
val bright_cyan       : style (** Shortcut for [Foreground Bright_cyan]    *)
val bright_white      : style (** Shortcut for [Foreground Bright_white]   *)

val on_black          : style (** Shortcut for [Background Black]          *)
val on_red            : style (** Shortcut for [Background Red]            *)
val on_green          : style (** Shortcut for [Background Green]          *)
val on_yellow         : style (** Shortcut for [Background Yellow]         *)
val on_blue           : style (** Shortcut for [Background Blue]           *)
val on_magenta        : style (** Shortcut for [Background Magenta]        *)
val on_cyan           : style (** Shortcut for [Background Cyan]           *)
val on_white          : style (** Shortcut for [Background White]          *)
val on_default        : style (** Shortcut for [Background Default]        *)

val on_bright_black   : style (** Shortcut for [Background Bright_black]   *)
val on_bright_red     : style (** Shortcut for [Background Bright_red]     *)
val on_bright_green   : style (** Shortcut for [Background Bright_green]   *)
val on_bright_yellow  : style (** Shortcut for [Background Bright_yellow]  *)
val on_bright_blue    : style (** Shortcut for [Background Bright_blue]    *)
val on_bright_magenta : style (** Shortcut for [Background Bright_magenta] *)
val on_bright_cyan    : style (** Shortcut for [Background Bright_cyan]    *)
val on_bright_white   : style (** Shortcut for [Background Bright_white]   *)

val set_autoreset : bool -> unit
  (** Turns the autoreset feature on and off.  It defaults to on. *)

val apply_string : style list -> string -> string

val print_string : style list -> string -> unit
  (** [print_string attr txt] prints the string [txt] with the
    attibutes [attr].  After printing, the attributes are
    automatically reseted to the defaults, unless autoreset is turned
    off. *)

val printf : style list -> ('a, unit, string, unit) format4 -> 'a
  (** [printf attr format arg1 ... argN] prints the arguments
    [arg1],...,[argN] according to [format] with the attibutes [attr].
    After printing, the attributes are automatically reseted to the
    defaults, unless autoreset is turned off. *)


(** {2 Erasing} *)

type loc = Above | Below | Screen | Line

val erase : loc -> unit
  (** [erase Above] erases everything before the position of the cursor.
    [erase Below] erases everything after the position of the cursor.
    [erase Screen] erases the whole screen.
  *)


(** {2 Cursor} *)

val set_cursor : int -> int -> unit
  (** [set_cursor x y] puts the cursor at position [(x,y)], [x]
    indicating the column (the leftmost one being 1) and [y] being the
    line (the topmost one being 1).  If [x <= 0], the [x] coordinate
    is unchanged; if [y <= 0], the [y] coordinate is unchanged.  *)

val move_cursor : int -> int -> unit
  (** [move_cursor x y] moves the cursor by [x] columns (to the right
    if [x > 0], to the left if [x < 0]) and by [y] lines (downwards if
    [y > 0] and upwards if [y < 0]). *)

val save_cursor : unit -> unit
  (** [save_cursor()] saves the current position of the cursor. *)
val restore_cursor : unit -> unit
  (** [restore_cursor()] replaces the cursor to the position saved
    with [save_cursor()]. *)


(** {2 Scrolling} *)

val scroll : int -> unit
  (** [scroll n] scrolls the terminal by [n] lines, up (creating new
    lines at the bottom) if [n > 0] and down if [n < 0]. *)

(** {2 Functorial interface} *)

module type P = sig
  val print_string : string -> unit
end
  (** The input signature of the functor [Make].*)

module type S = sig
  val print_string : style list -> string -> unit
  (* Note that this goes through ksprintf, so, e.g., will not flush if given %!.  To do
     that, use [fmt] below. *)
  val printf : style list -> ('a, unit, string, unit) format4 -> 'a
  val erase : loc -> unit
  val set_cursor : int -> int -> unit
  val move_cursor : int -> int -> unit
  val save_cursor : unit -> unit
  val restore_cursor : unit -> unit
  val scroll : int -> unit

  (* This is used as follows:
     ----
     module A = ANSITerminal
     let red fmt = A.fmt [A.Foreground A.Red] fmt
     ....
     printf (red "This sentence mentions %s and is in red.%!") "a string"
     ----
  *)
  val fmt : style list
    -> ('a, 'b, 'c, 'd, 'd, 'a) format6
    -> ('a, 'b, 'c, 'd, 'd, 'a) format6
end
  (** The output signature of the functor [Make].*)

module Make : functor (P : P) -> S
  (** Functor building an implementation around an alternate print_string function. *)
