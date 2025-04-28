open! Core
open! Async
open Expect_test_helpers_core

let%expect_test "Ansi_text.width = String.length on boring strings" =
  let s = "Ansi_text.width = String.length on boring strings" in
  let t = Ansi_text.parse s in
  assert (Ansi_text.width t = String.length s);
  [%expect {| |}];
  return ()
;;

let%expect_test "ignore ANSI, including unknown codes, when computing width" =
  let t = Ansi_text.parse "\027[0;31;123mfoo\027[31;42mbar\027[32;42mbaz\027[0;32;42m" in
  print_s [%sexp (Ansi_text.width t : int)];
  [%expect {| 9 |}];
  return ()
;;

let%expect_test "width calculations handle unicode" =
  let t = Ansi_text.parse "\027[1;41m0123\027[2;32m4👋│👋5\027[4;64m6789\027[0m" in
  print_s [%sexp (Ansi_text.width t : int)];
  [%expect {| 15 |}];
  return ()
;;

let%expect_test "a list with only styles is empty" =
  let only_styles = Ansi_text.parse "\027[1;41m\027[2;32m\027[4;64m\027[0m" in
  let with_spaces = Ansi_text.parse "\027[1;41m \027[2;32m\027[4;64m \027[0m" in
  print_s [%sexp (Ansi_text.is_empty only_styles : bool)];
  print_s [%sexp (Ansi_text.is_empty with_spaces : bool)];
  [%expect
    {|
    true
    false
    |}];
  return ()
;;

let%expect_test "map over styles and texts" =
  let t = Ansi_text.parse "\027[0;1;mfoo\027[2;3m  bar  \027[5;6mbaz\027[38;2;1;1;1;7m" in
  let t =
    Ansi_text.map
      t
      ~f_style:
        Ansi_text.(fun sty -> if Style.includes_reset sty then sty else Attr.Reset :: sty)
      ~f_text:Ansi_text.(fun txt -> Text.of_string (String.strip (Text.to_string txt)))
  in
  print_endline (Ansi_text.to_string_hum t);
  [%expect
    {| (off +bold)foo(off +faint +italic)bar(off +blink +fastblink)baz(off fg:rgb256-1-1-1 +invert) |}];
  return ()
;;

let%expect_test "simplify_styles combines, shortens, and drops" =
  let t =
    Ansi_text.parse
      "\027[0;1;m\027[2;3;mfoo\027[2;3m  bar  \027[5;6mbaz\027[38;2;1;1;1;7m"
  in
  print_endline Ansi_text.(t |> simplify_styles |> to_string_hum);
  [%expect {| (off +faint +italic)foo  bar  (+fastblink)baz(fg:rgb256-1-1-1 +invert) |}];
  return ()
;;

let%expect_test "get_style_at_end" =
  let str = "\027[3;1;2;4;38;5;250m some \027[22;48;5;1m text \027[58;5;32m" in
  print_endline Ansi_text.(parse str |> style_at_end |> Style.to_string);
  [%expect {| [3;4;38;5;250;22;41;58;5;32m |}];
  return ()
;;

let%expect_test "get_style_at_end handles resets" =
  let s1 = "\027[1;2;3;4;38;5;250m some \027[2;48;5;1m text \027[0m" in
  let s2 = "\027[1;2;3;4;38;5;250m some \027[2;48;5;1;0m text \027[1m" in
  print_endline Ansi_text.(parse s1 |> style_at_end |> Style.to_string);
  print_endline Ansi_text.(parse s2 |> style_at_end |> Style.to_string);
  [%expect
    {|
    [0m
    [0;1m
    |}];
  return ()
;;

let%expect_test "split in the middle" =
  let str = "\027[1;41m012345\027[2;32m6789ab\027[4;64mcdefgh\027[0m" in
  let before, after = Ansi_text.(parse str |> split ~pos:10) in
  let before = Ansi_text.to_string before in
  let after = Ansi_text.to_string after in
  print_endline before;
  print_endline after;
  print_endline (Ansi_text.visualize before);
  print_endline (Ansi_text.visualize after);
  [%expect
    {|
    [1;41m012345[2;32m6789[49;22;39m
    [41;2;32mab[4;64mcdefgh[0m
    (+bold bg:red)012345(+faint fg:green)6789(bg:default -weight fg:default)
    (bg:red +faint fg:green)ab(+uline ansi:64)cdefgh(off)
    |}];
  return ()
;;

let%expect_test "split at a boundary" =
  let str = "\027[1;41m012345\027[2;32m6789ab\027[4;64mcdefgh\027[0m" in
  let before, after = Ansi_text.(parse str |> split ~pos:12) in
  let before = Ansi_text.to_string before in
  let after = Ansi_text.to_string after in
  print_endline before;
  print_endline after;
  print_endline (Ansi_text.visualize before);
  print_endline (Ansi_text.visualize after);
  [%expect
    {|
    [1;41m012345[2;32m6789ab[49;22;39m
    [41;2;32;4;64mcdefgh[0m
    (+bold bg:red)012345(+faint fg:green)6789ab(bg:default -weight fg:default)
    (bg:red +faint fg:green +uline ansi:64)cdefgh(off)
    |}];
  return ()
;;

let%expect_test "split resets others" =
  let str = "\027[1;41m some \027[2;32m more \027[4;64m text \027[0m" in
  let before, after = Ansi_text.(parse str |> split ~pos:14) in
  let before = Ansi_text.to_string before in
  let after = Ansi_text.to_string after in
  print_endline before;
  print_endline after;
  print_endline (Ansi_text.visualize before);
  print_endline (Ansi_text.visualize after);
  [%expect
    {|
    [1;41m some [2;32m more [4;64m t[49;22;39;24;65m
    [41;2;32;4;64mext [0m
    (+bold bg:red) some (+faint fg:green) more (+uline ansi:64) t(bg:default -weight fg:default -uline ansi:65)
    (bg:red +faint fg:green +uline ansi:64)ext (off)
    |}];
  return ()
;;

let%expect_test "split handles unicode" =
  let str = "\027[1;41m0123\027[2;32m4👋👋5\027[4;64m6789\027[0m" in
  let before, after = Ansi_text.(parse str |> split ~pos:7) in
  print_endline (Int.to_string (Ansi_text.width before));
  print_endline (Int.to_string (Ansi_text.width after));
  let before = Ansi_text.to_string before in
  let after = Ansi_text.to_string after in
  print_endline before;
  print_endline after;
  print_endline (Ansi_text.visualize before);
  print_endline (Ansi_text.visualize after);
  [%expect
    {|
    7
    7
    [1;41m0123[2;32m4👋[49;22;39m
    [41;2;32m👋5[4;64m6789[0m
    (+bold bg:red)0123(+faint fg:green)4👋(bg:default -weight fg:default)
    (bg:red +faint fg:green)👋5(+uline ansi:64)6789(off)
    |}];
  return ()
;;
