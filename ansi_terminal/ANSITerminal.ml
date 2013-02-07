(*********************************************************
  File: ANSITerminal.ml
   Allow colors, cursor movements, erasing,... under Unix and DOS shells.
   *********************************************************************

   Copyright 2004 by Troestler Christophe
   Christophe.Troestler(at)umh.ac.be

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
***************************************************************)
(** See the file ctlseqs.html (unix)
    and (for DOS) http://www.ka.net/jmenees/Dos/Ansi.htm
 *)


open Printf

(* Erasing *)

type loc = Above | Below | Screen | Line

(* Colors *)

let autoreset = ref true

let set_autoreset b = autoreset := b


type color =
  | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default
  | Bright_black | Bright_red | Bright_green | Bright_yellow | Bright_blue
  | Bright_magenta | Bright_cyan | Bright_white

type style =
  | Reset | Bold | Underlined | Dim | Blink | Inverse | Hidden
  | Bold_off | Underlined_off | Dim_off | Blink_off | Inverse_off | Hidden_off
  | Foreground of color
  | Background of color

let black             = Foreground Black
let red               = Foreground Red
let green             = Foreground Green
let yellow            = Foreground Yellow
let blue              = Foreground Blue
let magenta           = Foreground Magenta
let cyan              = Foreground Cyan
let white             = Foreground White
let default           = Foreground Default

let bright_black      = Foreground Bright_black
let bright_red        = Foreground Bright_red
let bright_green      = Foreground Bright_green
let bright_yellow     = Foreground Bright_yellow
let bright_blue       = Foreground Bright_blue
let bright_magenta    = Foreground Bright_magenta
let bright_cyan       = Foreground Bright_cyan
let bright_white      = Foreground Bright_white

let on_black          = Background Black
let on_red            = Background Red
let on_green          = Background Green
let on_yellow         = Background Yellow
let on_blue           = Background Blue
let on_magenta        = Background Magenta
let on_cyan           = Background Cyan
let on_white          = Background White
let on_default        = Background Default

let on_bright_black   = Background Bright_black
let on_bright_red     = Background Bright_red
let on_bright_green   = Background Bright_green
let on_bright_yellow  = Background Bright_yellow
let on_bright_blue    = Background Bright_blue
let on_bright_magenta = Background Bright_magenta
let on_bright_cyan    = Background Bright_cyan
let on_bright_white   = Background Bright_white

let style_to_format = function
  | Reset                     -> format_of_string "0"
  | Bold                      -> format_of_string "1"
  | Bold_off                  -> format_of_string "22"
  | Dim                       -> format_of_string "2"
  | Dim_off                   -> format_of_string "22"
  | Underlined                -> format_of_string "4"
  | Underlined_off            -> format_of_string "24"
  | Blink                     -> format_of_string "5"
  | Blink_off                 -> format_of_string "25"
  | Inverse                   -> format_of_string "7"
  | Inverse_off               -> format_of_string "27"
  | Hidden                    -> format_of_string "8"
  | Hidden_off                -> format_of_string "28"
  | Foreground Black          -> format_of_string "30"
  | Foreground Red            -> format_of_string "31"
  | Foreground Green          -> format_of_string "32"
  | Foreground Yellow         -> format_of_string "33"
  | Foreground Blue           -> format_of_string "34"
  | Foreground Magenta        -> format_of_string "35"
  | Foreground Cyan           -> format_of_string "36"
  | Foreground White          -> format_of_string "37"
  | Foreground Default        -> format_of_string "39"
  | Foreground Bright_black   -> format_of_string "90"
  | Foreground Bright_red     -> format_of_string "91"
  | Foreground Bright_green   -> format_of_string "92"
  | Foreground Bright_yellow  -> format_of_string "93"
  | Foreground Bright_blue    -> format_of_string "94"
  | Foreground Bright_magenta -> format_of_string "95"
  | Foreground Bright_cyan    -> format_of_string "96"
  | Foreground Bright_white   -> format_of_string "97"
  | Background Black          -> format_of_string "40"
  | Background Red            -> format_of_string "41"
  | Background Green          -> format_of_string "42"
  | Background Yellow         -> format_of_string "43"
  | Background Blue           -> format_of_string "44"
  | Background Magenta        -> format_of_string "45"
  | Background Cyan           -> format_of_string "46"
  | Background White          -> format_of_string "47"
  | Background Default        -> format_of_string "49"
  | Background Bright_black   -> format_of_string "100"
  | Background Bright_red     -> format_of_string "101"
  | Background Bright_green   -> format_of_string "102"
  | Background Bright_yellow  -> format_of_string "103"
  | Background Bright_blue    -> format_of_string "104"
  | Background Bright_magenta -> format_of_string "105"
  | Background Bright_cyan    -> format_of_string "106"
  | Background Bright_white   -> format_of_string "107"

let style_to_string style = string_of_format (style_to_format style)

let apply ~start ~semi ~m ~stop ~of_style ~concat styles x =
  let start, semi, m, stop = start (), semi (), m (), stop () in
  let (^) = concat in
  match styles with
  | [] -> start ^ m ^ stop
  | _::_ ->
    let s_list = List.fold_left (fun acc style ->
      semi::of_style style::acc
    ) [start] styles
    in
    let s_list = List.tl s_list in
    let s = List.fold_left (fun acc s -> s ^ acc) (List.hd s_list) (List.tl s_list) in
    s ^ m ^ x ^ stop

let start () = format_of_string "\027["
let semi  () = format_of_string ";"
let m     () = format_of_string "m"
let stop  () = format_of_string "\027[0m"

let apply_format styles fmt = apply
  ~start
  ~semi
  ~m
  ~stop
  ~of_style:style_to_format
  ~concat:(^^)
  styles fmt

let apply_string styles s = apply
  ~start: (fun () -> string_of_format (start ()))
  ~semi:  (fun () -> string_of_format (semi  ()))
  ~m:     (fun () -> string_of_format (m     ()))
  ~stop:  (fun () -> string_of_format (stop  ()))
  ~of_style:style_to_string
  ~concat:(^)
  styles s

(* Functorize the rest, so that we can easily substitute a different method of printing
   (e.g. Async.Std.Print) *)

module type P = sig
  val print_string : string -> unit
end

module type S = sig
  val print_string : style list -> string -> unit
  val printf : style list -> ('a, unit, string, unit) format4 -> 'a
  val erase : loc -> unit
  val set_cursor : int -> int -> unit
  val move_cursor : int -> int -> unit
  val save_cursor : unit -> unit
  val restore_cursor : unit -> unit
  val scroll : int -> unit
  val fmt : style list
    -> ('a, 'b, 'c, 'd, 'd, 'a) format6
    -> ('a, 'b, 'c, 'd, 'd, 'a) format6
end

module Make(P : P) : S = struct
  let print_string = P.print_string
  let printf fmt = ksprintf print_string fmt

  let fmt = apply_format
  (* Erasing cont'd *)

  let erase = function
    | Above  -> print_string "\027[1J"
    | Below  -> print_string "\027[0J"
    | Screen -> print_string "\027[2J"
    | Line   -> print_string "\027[K"


  (* Cursor *)

  let set_cursor x y =
    if x <= 0 then (if y > 0 then printf "\027[%id" y)
    else (* x > 0 *) if y <= 0 then printf "\027[%iG" x
    else printf "\027[%i;%iH" y x

  let move_cursor x y =
    if x > 0 then printf "\027[%iC" x
    else if x < 0 then printf "\027[%iD" (-x);
    if y > 0 then printf "\027[%iB" y
    else if y < 0 then printf "\027[%iA" (-y)

  let save_cursor () = print_string "\027[s"
  let restore_cursor () = print_string "\027[u"

  (* Scrolling *)

  let scroll lines =
    if lines > 0 then printf "\027[%iS" lines
    else if lines < 0 then printf "\027[%iT" (- lines)

  (* Color cont'd *)

  let print_string style txt =
    print_string "\027[";
    let s = String.concat ";" (List.map style_to_string style) in
    print_string s;
    print_string "m";
    print_string txt;
    if !autoreset then print_string "\027[0m"


  let printf style fmt = ksprintf (print_string style) fmt
end

include Make(struct let print_string = print_string end)



(* On DOS & windows, to enable the ANSI sequences, ANSI.SYS should be
   loaded in C:\CONFIG.SYS with a line of the type

   DEVICE = C:\DOS\ANSI.SYS
   DEVICEHIGH=C:\WINDOWS\COMMAND\ANSI.SYS

   This routine checks whether the line is present and, if not, it
   inserts it and tells the user to reboot.

   On WINNT, one will create a ANSI.NT in the user dir and a
   command.com link on the desktop (with Configfilename = our ANSI.NT)
   and tell the user to use it.

   REM: that does NOT work under winxp because OCaml programs are not
   considered to run in DOS mode only...

   http://support.microsoft.com/default.aspx?scid=kb;en-us;816179
   http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/console_functions.asp
*)


(* let is_readable file = *)
(*   try close_in(open_in file); true *)
(*   with Sys_error _ -> false *)

(* let config_sys = "C:\\CONFIG.SYS" *)
(* exception OK *)

(* let win9x () = *)
(*   (\* Locate ANSI.SYS *\) *)
(*   let ansi_sys = List.find is_readable [ *)
(*     "C:\\DOS\\ANSI.SYS"; *)
(*     "C:\\WINDOWS\\COMMAND\\ANSI.SYS"; ] in *)
(*   (\* Parse CONFIG.SYS to see wether it has the right line *\) *)
(*   try *)
(*     let re = Str.regexp_case_fold *)
(*                ("^DEVICE\\(HIGH\\)?[ \t]*=[ \t]*" ^ ansi_sys ^ "[ \t]*$") in *)
(*     let fh = open_in config_sys in *)
(*     begin try *)
(*       while true do *)
(*         if Str.string_match re (input_line fh) 0 then raise OK *)
(*       done *)
(*     with *)
(*     | End_of_file -> *)
(*         (\* Correct line not found: add it *\) *)
(*         close_in fh; *)
(*         raise(Sys_error "win9x") *)
(*     | OK -> close_in fh (\* Correct line found, keep going *\) *)
(*     end *)
(*   with Sys_error _ -> *)
(*     (\* config_sys not does not exists or does not contain the right line. *\) *)
(*     let fh = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] *)
(*                0x777 config_sys in *)
(*     output_string fh ("DEVICEHIGH=" ^ ansi_sys ^ "\n"); *)
(*     close_out fh; *)
(*     prerr_endline "Please restart your computer and rerun the program."; *)
(*     exit 1 *)



(* let winnt home = *)
(*   (\* Locate ANSI.SYS *\) *)
(*   let system = *)
(*     try Sys.getenv "SystemRoot" *)
(*     with Not_found -> "C:\\WINDOWS" in *)
(*   let ansi_sys = *)
(*     List.find is_readable (List.map (fun s -> Filename.concat system s) *)
(*                              [ "SYSTEM32\\ANSI.SYS"; ]) in *)
(*   (\* Create an ANSI.SYS file in the user dir *\) *)
(*   let ansi_nt = Filename.concat home "ANSI.NT" in *)
(*   let fh = open_out ansi_nt in *)
(*   output_string fh "dosonly\ndevice="; *)
(*   output_string fh ansi_sys; *)
(*   output_string fh "\ndevice=%SystemRoot%\\system32\\himem.sys *)
(* files=40 *)
(* dos=high, umb *)
(* " ; *)
(*   close_out fh; *)
(*   (\* Make a command.com link on the desktop *\) *)
(*   let fh = open_out (Filename.concat home "command.lnk") in *)
(*   close_out fh *)


(* let () = *)
(*   if Sys.os_type = "Win32" then begin *)
(*     try  winnt(Sys.getenv "USERPROFILE")  (\* WinNT, Win2000, WinXP *\) *)
(*     with Not_found -> win9x() (\* Win9x *\) *)
(*   end *)
