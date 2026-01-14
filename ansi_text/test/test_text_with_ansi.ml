open! Core
open! Async
open Expect_test_helpers_core
open Ansi_text

let%expect_test "Ansi_text.width = String.length on boring strings" =
  let s = "Ansi_text.width = String.length on boring strings" in
  let t = parse s in
  assert (width t = String.length s);
  [%expect {| |}];
  return ()
;;

let%expect_test "ignore ANSI, including unknown codes, when computing width" =
  let t =
    parse "\027[0;31;123mfoo\027[F\027[31;42mbar\027[32;42mbaz\027[4i\027[0;32;42m"
  in
  print_s [%sexp (width t : int)];
  [%expect {| 9 |}];
  return ()
;;

let%expect_test "width calculations handle unicode" =
  let t = parse "\027[1;41m0123\027[2;32m4👋│👋5\027[4;64m6789\027[0m\027[E" in
  print_s [%sexp (width t : int)];
  [%expect {| 15 |}];
  return ()
;;

let%expect_test "a list with only ansi is empty" =
  let only_styles = parse "\027[10T\027[1;41m\027[2;32m\027[4;64m\027[0m" in
  let with_spaces = parse "\027[1;41m \027[2;32m\027[4;64m \027[0m\027[10S" in
  print_s [%sexp (is_empty only_styles : bool)];
  print_s [%sexp (is_empty with_spaces : bool)];
  [%expect
    {|
    true
    false
    |}];
  return ()
;;

let%expect_test "map over styles, controls, and texts" =
  let t =
    parse "\027[0;1;mfoo\027[2;3m  bar  \027[5;6mbaz\027[38;2;1;1;1;7m\027[3B\027[T\027[B"
  in
  let t =
    map t ~f:(function
      | `Style sty ->
        Some (`Style (if Style.includes_reset sty then sty else Attr.Reset :: sty))
      | _ -> None)
    |> map ~f:(function
      | `Control ctl ->
        Some
          (`Control
            (match ctl with
             | CursorDown (Some n) -> CursorDown (Some (n + 1))
             | _ -> ctl))
      | _ -> None)
    |> map ~f:(function
      | `Text txt -> Some (`Text (Text.of_string (String.strip (Text.to_string txt))))
      | _ -> None)
  in
  print_endline (to_string_hum t);
  [%expect
    {| (off +bold)foo(off +faint +italic)bar(off +blink +fastblink)baz(off fg:rgb256-1-1-1 +invert)(CursorDown:4)(ScrollDown)(CursorDown) |}];
  return ()
;;

let%expect_test "simplify_styles combines, shortens, and drops" =
  let t = parse "\027[0;1;m\027[2;3;mfoo\027[2;3m  bar  \027[5;6mbaz\027[38;2;1;1;1;7m" in
  print_endline (t |> simplify_styles |> to_string_hum);
  [%expect {| (off +faint +italic)foo  bar  (+fastblink)baz(fg:rgb256-1-1-1 +invert) |}];
  return ()
;;

let%expect_test "get_style_at_end" =
  let str = "\027[2A\027[3;1;2;4;38;5;250m some \027[22;48;5;1m text \027[58;5;32m" in
  print_endline (parse str |> style_at_end |> Style.to_string);
  [%expect {| [3;4;38;5;250;22;41;58;5;32m |}];
  return ()
;;

let%expect_test "get_style_at_end handles resets" =
  let s1 = "\027[1;2;3;4;38;5;250m some \027[K\027[2;48;5;1m text \027[0m" in
  let s2 = "\027[1;2;3;4;38;5;250m some \027[5i\027[2;48;5;1;0m text \027[1m" in
  print_endline (parse s1 |> style_at_end |> Style.to_string);
  print_endline (parse s2 |> style_at_end |> Style.to_string);
  [%expect
    {|
    [0m
    [0;1m
    |}];
  return ()
;;

let%expect_test "split in the middle" =
  let str = "\027[1;41m012345\027[2;32m6789ab\027[G\027[4;64mcdefgh\027[0m" in
  let before, after = parse str |> split ~pos:10 in
  let before = to_string before in
  let after = to_string after in
  print_endline before;
  print_endline after;
  print_endline (visualize before);
  print_endline (visualize after);
  [%expect
    {|
    [1;41m012345[2;32m6789[49;22;39m
    [41;2;32mab[G[4;64mcdefgh[0m
    (+bold bg:red)012345(+faint fg:green)6789(bg:default -weight fg:default)
    (bg:red +faint fg:green)ab(CursorToCol:1)(+uline ANSI-SGR:64)cdefgh(off)
    |}];
  return ()
;;

let%expect_test "split at a boundary" =
  let str = "\027[F\027[1;41m012345\027[2;32m6789ab\027[4;64mcdefgh\027[0m" in
  let before, after = parse str |> split ~pos:12 in
  let before = to_string before in
  let after = to_string after in
  print_endline before;
  print_endline after;
  print_endline (visualize before);
  print_endline (visualize after);
  [%expect
    {|
    [F[1;41m012345[2;32m6789ab[49;22;39m
    [41;2;32;4;64mcdefgh[0m
    (CursorPrevLine)(+bold bg:red)012345(+faint fg:green)6789ab(bg:default -weight fg:default)
    (bg:red +faint fg:green +uline ANSI-SGR:64)cdefgh(off)
    |}];
  return ()
;;

let%expect_test "split resets others" =
  let str = "\027[1;41m some \027[2;32m more \027[1K\027[4;64m text \027[0m" in
  let before, after = parse str |> split ~pos:14 in
  let before = to_string before in
  let after = to_string after in
  print_endline before;
  print_endline after;
  print_endline (visualize before);
  print_endline (visualize after);
  [%expect
    {|
    [1;41m some [2;32m more [1K[4;64m t[49;22;39;24;65m
    [41;2;32;4;64mext [0m
    (+bold bg:red) some (+faint fg:green) more (EraseLine:ToStart)(+uline ANSI-SGR:64) t(bg:default -weight fg:default -uline ANSI-SGR:65)
    (bg:red +faint fg:green +uline ANSI-SGR:64)ext (off)
    |}];
  return ()
;;

let%expect_test "split handles unicode" =
  let str = "\027[1;41m\027[2J0123\027[2;32m4👋👋5\027[4;64m6789\027[0m" in
  let before, after = parse str |> split ~pos:7 in
  print_endline (Int.to_string (width before));
  print_endline (Int.to_string (width after));
  let before = to_string before in
  let after = to_string after in
  print_endline before;
  print_endline after;
  print_endline (visualize before);
  print_endline (visualize after);
  [%expect
    {|
    7
    7
    [1;41m[2J0123[2;32m4👋[49;22;39m
    [41;2;32m👋5[4;64m6789[0m
    (+bold bg:red)(EraseScreen)0123(+faint fg:green)4👋(bg:default -weight fg:default)
    (bg:red +faint fg:green)👋5(+uline ANSI-SGR:64)6789(off)
    |}];
  return ()
;;
