open Core

type clear_line =
  | To_line_end
  | To_line_start
  | Whole_line
[@@deriving sexp, compare ~localize, equal ~localize, quickcheck]

type clear_screen =
  | To_screen_end
  | To_screen_start
  | Whole_screen
  | Screen_and_scrollback
[@@deriving sexp, compare ~localize, equal ~localize, quickcheck]

type t =
  | CursorUp of int option
  | CursorDown of int option
  | CursorForward of int option
  | CursorBackward of int option
  | CursorNextLine of int option
  | CursorPrevLine of int option
  | CursorToCol of int option
  | CursorToPos of int option * int option
  | EraseDisplay of clear_screen option
  | EraseLine of clear_line option
  | ScrollUp of int option
  | ScrollDown of int option
  | Unknown of string
[@@deriving sexp, compare ~localize, equal ~localize, quickcheck]

let of_string_exn str =
  let str = String.chop_prefix_exn str ~prefix:"\027[" in
  let letter = String.suffix str 1 in
  let numbers =
    String.drop_suffix str 1 |> String.split ~on:';' |> List.map ~f:Int.of_string_opt
  in
  let n = List.hd numbers |> Option.join in
  match letter with
  | "A" -> CursorUp n
  | "B" -> CursorDown n
  | "C" -> CursorForward n
  | "D" -> CursorBackward n
  | "E" -> CursorNextLine n
  | "F" -> CursorPrevLine n
  | "G" -> CursorToCol n
  | "H" ->
    let m = List.nth numbers 1 |> Option.join in
    CursorToPos (n, m)
  | "J" ->
    EraseDisplay
      (match%map.Option n with
       | 0 -> To_screen_end
       | 1 -> To_screen_start
       | 2 -> Whole_screen
       | 3 -> Screen_and_scrollback
       | _ -> invalid_arg "EraseDisplay requires n in 0-3")
  | "K" ->
    EraseLine
      (match%map.Option n with
       | 0 -> To_line_end
       | 1 -> To_line_start
       | 2 -> Whole_line
       | _ -> invalid_arg "EraseLine requires n in 0-2")
  | "S" -> ScrollUp n
  | "T" -> ScrollDown n
  | _ -> Unknown str
;;

let of_string_opt str = Option.try_with (fun () -> of_string_exn str)

let to_string = function
  | CursorUp None -> "\027[A"
  | CursorUp (Some n) -> sprintf "\027[%dA" n
  | CursorDown None -> "\027[B"
  | CursorDown (Some n) -> sprintf "\027[%dB" n
  | CursorForward None -> "\027[C"
  | CursorForward (Some n) -> sprintf "\027[%dC" n
  | CursorBackward None -> "\027[D"
  | CursorBackward (Some n) -> sprintf "\027[%dD" n
  | CursorNextLine None -> "\027[E"
  | CursorNextLine (Some n) -> sprintf "\027[%dE" n
  | CursorPrevLine None -> "\027[F"
  | CursorPrevLine (Some n) -> sprintf "\027[%dF" n
  | CursorToCol None -> "\027[G"
  | CursorToCol (Some n) -> sprintf "\027[%dG" n
  | CursorToPos (None, None) -> "\027[H"
  | CursorToPos (Some n, None) -> sprintf "\027[%dH" n
  | CursorToPos (None, Some m) -> sprintf "\027[;%dH" m
  | CursorToPos (Some n, Some m) -> sprintf "\027[%d;%dH" n m
  | EraseDisplay None -> "\027[J"
  | EraseDisplay (Some To_screen_end) -> "\027[0J"
  | EraseDisplay (Some To_screen_start) -> "\027[1J"
  | EraseDisplay (Some Whole_screen) -> "\027[2J"
  | EraseDisplay (Some Screen_and_scrollback) -> "\027[3J"
  | EraseLine None -> "\027[K"
  | EraseLine (Some To_line_end) -> "\027[0K"
  | EraseLine (Some To_line_start) -> "\027[1K"
  | EraseLine (Some Whole_line) -> "\027[2K"
  | ScrollUp None -> "\027[S"
  | ScrollUp (Some n) -> sprintf "\027[%dS" n
  | ScrollDown None -> "\027[T"
  | ScrollDown (Some n) -> sprintf "\027[%dT" n
  | Unknown str -> "\027[" ^ str
;;

let to_string_hum = function
  | CursorUp None -> "(CursorUp)"
  | CursorUp (Some n) -> sprintf "(CursorUp:%d)" n
  | CursorDown None -> "(CursorDown)"
  | CursorDown (Some n) -> sprintf "(CursorDown:%d)" n
  | CursorForward None -> "(CursorForward)"
  | CursorForward (Some n) -> sprintf "(CursorForward:%d)" n
  | CursorBackward None -> "(CursorBackward)"
  | CursorBackward (Some n) -> sprintf "(CursorBackward:%d)" n
  | CursorNextLine None -> "(CursorNextLine)"
  | CursorNextLine (Some n) -> sprintf "(CursorNextLine:%d)" n
  | CursorPrevLine None -> "(CursorPrevLine)"
  | CursorPrevLine (Some n) -> sprintf "(CursorPrevLine:%d)" n
  | CursorToCol None -> "(CursorToCol:1)"
  | CursorToCol (Some n) -> sprintf "(CursorToCol:%d)" n
  | CursorToPos (None, None) -> "(CursorToPos:1;1)"
  | CursorToPos (Some n, None) -> sprintf "(CursorToPos:%d;1)" n
  | CursorToPos (None, Some m) -> sprintf "(CursorToPos:1;%d)" m
  | CursorToPos (Some n, Some m) -> sprintf "(CursorToPos:%d;%d)" n m
  | EraseDisplay None | EraseDisplay (Some To_screen_end) -> "(EraseScreen:ToEnd)"
  | EraseDisplay (Some To_screen_start) -> "(EraseScreen:ToStart)"
  | EraseDisplay (Some Whole_screen) -> "(EraseScreen)"
  | EraseDisplay (Some Screen_and_scrollback) -> "(EraseScreen:AndScrollback)"
  | EraseLine None | EraseLine (Some To_line_end) -> "(EraseLine:ToEnd)"
  | EraseLine (Some To_line_start) -> "(EraseLine:ToStart)"
  | EraseLine (Some Whole_line) -> "(EraseLine)"
  | ScrollUp None -> "(ScrollUp)"
  | ScrollUp (Some n) -> sprintf "(ScrollUp:%d)" n
  | ScrollDown None -> "(ScrollDown)"
  | ScrollDown (Some n) -> sprintf "(ScrollDown:%d)" n
  | Unknown str -> sprintf "(ANSI-CSI:%s)" str
;;
