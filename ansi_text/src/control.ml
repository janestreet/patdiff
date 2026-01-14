open Core

type clear_line =
  | To_line_end
  | To_line_start
  | Whole_line
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

type clear_screen =
  | To_screen_end
  | To_screen_start
  | Whole_screen
  | Screen_and_scrollback
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

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
[@@deriving compare ~localize, equal ~localize, quickcheck, sexp]

(* Custom quickcheck generator that only produces valid CSI sequences. ANSI CSI parameters
   must be non-negative integers. *)
let quickcheck_generator =
  let open Base_quickcheck.Generator in
  let open Base_quickcheck.Generator.Let_syntax in
  let nonneg_int_option = Option.quickcheck_generator (int_inclusive 0 999) in
  let gen_unknown =
    (* Generate only valid CSI parameter strings: digits, semicolons, and a final byte *)
    let%bind params =
      List.quickcheck_generator (int_inclusive 0 999)
      |> map ~f:(fun ints -> String.concat ~sep:";" (List.map ints ~f:Int.to_string))
    in
    let%bind final = of_list [ 'p'; 'q'; 'r'; 's'; 't'; 'u' ] in
    return (params ^ String.make 1 final)
  in
  union
    [ map nonneg_int_option ~f:(fun n -> CursorUp n)
    ; map nonneg_int_option ~f:(fun n -> CursorDown n)
    ; map nonneg_int_option ~f:(fun n -> CursorForward n)
    ; map nonneg_int_option ~f:(fun n -> CursorBackward n)
    ; map nonneg_int_option ~f:(fun n -> CursorNextLine n)
    ; map nonneg_int_option ~f:(fun n -> CursorPrevLine n)
    ; map nonneg_int_option ~f:(fun n -> CursorToCol n)
    ; (let%bind n = nonneg_int_option in
       let%bind m = nonneg_int_option in
       return (CursorToPos (n, m)))
    ; map [%quickcheck.generator: clear_screen option] ~f:(fun c -> EraseDisplay c)
    ; map [%quickcheck.generator: clear_line option] ~f:(fun c -> EraseLine c)
    ; map nonneg_int_option ~f:(fun n -> ScrollUp n)
    ; map nonneg_int_option ~f:(fun n -> ScrollDown n)
    ; map gen_unknown ~f:(fun s -> Unknown s)
    ]
;;

let of_csi ~params ~terminal =
  let numbers = String.split params ~on:';' |> List.map ~f:Int.of_string_opt in
  let n = List.hd numbers |> Option.join in
  let unknown () = Unknown (params ^ String.make 1 terminal) in
  match terminal with
  | 'A' -> CursorUp n
  | 'B' -> CursorDown n
  | 'C' -> CursorForward n
  | 'D' -> CursorBackward n
  | 'E' -> CursorNextLine n
  | 'F' -> CursorPrevLine n
  | 'G' -> CursorToCol n
  | 'H' ->
    let m = List.nth numbers 1 |> Option.join in
    CursorToPos (n, m)
  | 'J' ->
    (match n with
     | None -> EraseDisplay None
     | Some 0 -> EraseDisplay (Some To_screen_end)
     | Some 1 -> EraseDisplay (Some To_screen_start)
     | Some 2 -> EraseDisplay (Some Whole_screen)
     | Some 3 -> EraseDisplay (Some Screen_and_scrollback)
     | Some _ -> unknown ())
  | 'K' ->
    (match n with
     | None -> EraseLine None
     | Some 0 -> EraseLine (Some To_line_end)
     | Some 1 -> EraseLine (Some To_line_start)
     | Some 2 -> EraseLine (Some Whole_line)
     | Some _ -> unknown ())
  | 'S' -> ScrollUp n
  | 'T' -> ScrollDown n
  | _ -> unknown ()
;;

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
