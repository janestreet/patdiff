open! Core
open! Async
open Expect_test_helpers_core

let%expect_test "simple visualize" =
  print_endline (Ansi_text.visualize "\027[31mfoo\027[0m");
  [%expect {| (fg:red)foo(off) |}];
  return ()
;;

let%expect_test "visualize drops empty codes" =
  print_endline (Ansi_text.visualize "\027[mfoo\027[0m");
  [%expect {| foo(off) |}];
  return ()
;;

let%expect_test "simple minimize" =
  print_endline (Ansi_text.minimize "\027[0;31mfoo");
  [%expect {| [0;31mfoo |}];
  return ()
;;

let%expect_test "minimize resets" =
  let s = "a \027[0m line of text \027[0;0;0m\027[0m" in
  print_endline (Ansi_text.visualize s);
  print_endline (Ansi_text.minimize s);
  print_endline (Ansi_text.minimize s |> Ansi_text.visualize);
  [%expect
    {|
    a (off) line of text (off off off)(off)
    a [0m line of text
    a (off) line of text
    |}];
  return ()
;;

let%expect_test "minimize with newlines" =
  let s =
    "a \027[0m line of text\n\027[0m\n\027[1;2mline after skip\n\027[0;0m\027[0m!\n"
  in
  print_endline (Ansi_text.visualize s);
  print_endline (Ansi_text.minimize s);
  print_endline (Ansi_text.minimize s |> Ansi_text.visualize);
  [%expect
    {|
    a (off) line of text
    (off)
    (+bold +faint)line after skip
    (off off)(off)!

    a [0m line of text

    [2mline after skip
    [0m!

    a (off) line of text

    (+faint)line after skip
    (off)!
    |}];
  return ()
;;

let%expect_test "strip out ANSI codes that are later overridden by a reset" =
  let s = "\027[0m\027[31m\027[0m\027[32m\027[42mf\027[44moo\027[0m" in
  print_endline (Ansi_text.visualize s);
  print_endline (Ansi_text.minimize s);
  print_endline (Ansi_text.minimize s |> Ansi_text.visualize);
  [%expect
    {|
    (off)(fg:red)(off)(fg:green)(bg:green)f(bg:blue)oo(off)
    [0;32;42mf[44moo[0m
    (off fg:green bg:green)f(bg:blue)oo(off)
    |}];
  return ()
;;

let%expect_test "combine and simplify adjacent ANSI codes" =
  print_endline (Ansi_text.minimize "\027[0;44m\027[0;31m\027[1;32;44mfoo");
  [%expect {| [0;1;32;44mfoo |}];
  return ()
;;

let%expect_test "ignore a repeated code even with a reset" =
  let str = "  \027[0;41;30mabc\027[0;2mdef\027[0;1;2mghi\027[0m" in
  print_endline Ansi_text.(visualize str);
  print_endline Ansi_text.(minimize str);
  print_endline Ansi_text.(minimize str |> visualize);
  [%expect
    {|
    (off bg:red fg:black)abc(off +faint)def(off +bold +faint)ghi(off)
    [0;41;30mabc[0;2mdefghi[0m
    (off bg:red fg:black)abc(off +faint)defghi(off)
    |}];
  return ()
;;

let%expect_test "strip out ANSI codes that are redundant with earlier codes" =
  print_endline
    (Ansi_text.minimize "\027[0;31mfoo\027[31;42mbar\027[32;42mbaz\027[0;32;42m");
  [%expect {| [0;31mfoo[42mbar[32mbaz |}];
  return ()
;;

let%expect_test "strip all ANSI codes" =
  print_endline
    (Ansi_text.strip "\027[0m\027[31m\027[0m\027[32m\027[42mf\027[44moo\027[0m");
  [%expect {| foo |}];
  return ()
;;

let%expect_test "strip known & unknown codes, but not malformed codes" =
  let s = "\027[0;31;123mfoo\027[31;42mbar\027[32;42mbaz\027[0;32;42m\027[0;32a;42m" in
  print_endline (Ansi_text.strip s);
  [%expect {| foobarbaz[0;32a;42m |}];
  return ()
;;

let%expect_test "compress unknown codes" =
  print_endline (Ansi_text.minimize "\027[11;12mfoo\027[10;51;52mbar\027[54;52m");
  [%expect {| [12mfoo[10;52mbar |}];
  return ()
;;

let%expect_test "all the attributes" =
  let s =
    "\027[1;2;3;4;5;7;8;9;21;53;31;41;53mfoo\027[22;23;24;25;27;28;29;55;39;49;59mbar\027[0m"
  in
  print_endline (Ansi_text.visualize s);
  print_endline (Ansi_text.minimize s);
  [%expect
    {|
    (+bold +faint +italic +uline +blink +invert +hide +strike +2uline +overline fg:red bg:red +overline)foo(-weight -italic -uline -blink -invert -hide -strike -overline fg:default bg:default ul:default)bar(off)
    [2;3;5;7;8;9;21;31;41;53mfoo[22;23;24;25;27;28;29;55;39;49;59mbar[0m
    |}];
  return ()
;;

let%expect_test "apply turns on and off but doesn't simplify" =
  let str = "some \027[2;48;5;1m text" in
  let style = Ansi_text.Style.of_string_exn "\027[1;2;3;4;38;5;250m" in
  let styled = Ansi_text.apply style str in
  print_endline styled;
  print_endline (Ansi_text.minimize styled);
  [%expect
    {|
    [1;2;3;4;38;5;250msome [2;48;5;1m text[22;23;24;39m
    [2;3;4;38;5;250msome [41m text[22;23;24;39m
    |}];
  return ()
;;

let%expect_test "handle a patdiff header" =
  let s =
    "\027[1;94m@@@@@@@@\027[22;39m \027[1;45;37m--\027[22;49;39m \027[1mdiff of\027[0m \
     \027[31m/home/username/sbs/da_ob\027[39m \027[1m&\027[22m \
     \027[32m/home/us\027[0m│\027[1;94m@@@@@@@@\027[22;39m \027[1;46;37m++\027[22;49;39m \
     \027[1mdiff of\027[22m \027[31m/home/username/sbs/da_nb\027[39m \027[1m&\027[22m \
     \027[32m/home/us\027[0m"
  in
  let m = Ansi_text.minimize s in
  print_endline (Ansi_text.strip s);
  print_endline (Ansi_text.strip m);
  print_endline (Ansi_text.visualize s);
  print_endline (Ansi_text.visualize m);
  print_endline s;
  print_endline m;
  [%expect
    {|
    @@@@@@@@ -- diff of /home/username/sbs/da_ob & /home/us│@@@@@@@@ ++ diff of /home/username/sbs/da_nb & /home/us
    @@@@@@@@ -- diff of /home/username/sbs/da_ob & /home/us│@@@@@@@@ ++ diff of /home/username/sbs/da_nb & /home/us
    (+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:magenta fg:white)--(-weight bg:default fg:default) (+bold)diff of(off) (fg:red)/home/username/sbs/da_ob(fg:default) (+bold)&(-weight) (fg:green)/home/us(off)│(+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:cyan fg:white)++(-weight bg:default fg:default) (+bold)diff of(-weight) (fg:red)/home/username/sbs/da_nb(fg:default) (+bold)&(-weight) (fg:green)/home/us(off)
    (+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:magenta fg:white)--(-weight bg:default fg:default) (+bold)diff of(off) (fg:red)/home/username/sbs/da_ob(fg:default) (+bold)&(-weight) (fg:green)/home/us(off)│(+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:cyan fg:white)++(-weight bg:default fg:default) (+bold)diff of(-weight) (fg:red)/home/username/sbs/da_nb(fg:default) (+bold)&(-weight) (fg:green)/home/us(off)
    [1;94m@@@@@@@@[22;39m [1;45;37m--[22;49;39m [1mdiff of[0m [31m/home/username/sbs/da_ob[39m [1m&[22m [32m/home/us[0m│[1;94m@@@@@@@@[22;39m [1;46;37m++[22;49;39m [1mdiff of[22m [31m/home/username/sbs/da_nb[39m [1m&[22m [32m/home/us[0m
    [1;94m@@@@@@@@[22;39m [1;45;37m--[22;49;39m [1mdiff of[0m [31m/home/username/sbs/da_ob[39m [1m&[22m [32m/home/us[0m│[1;94m@@@@@@@@[22;39m [1;46;37m++[22;49;39m [1mdiff of[22m [31m/home/username/sbs/da_nb[39m [1m&[22m [32m/home/us[0m
    |}];
  return ()
;;

let%expect_test "center a patdiff header" =
  let header =
    Ansi_text.parse
      " \027[1mdiff of\027[22m \027[31mold base\027[39m and \027[32mold tip\027[39m "
  in
  let style = Ansi_text.Attr.[ Bold; Fg (Bright Blue) ] in
  let centered = Ansi_text.center ~char:'@' ~style ~width:80 header in
  print_endline (Ansi_text.to_string centered);
  print_endline (Ansi_text.to_string_hum centered);
  [%expect
    {|
    [1;94m@@@@@@@@@@@@@@@@@@@@@@@@@[22;39m [1mdiff of[22m [31mold base[39m and [32mold tip[39m [1;94m@@@@@@@@@@@@@@@@@@@@@@@@@[22;39m
    (+bold fg:bright-blue)@@@@@@@@@@@@@@@@@@@@@@@@@(-weight fg:default) (+bold)diff of(-weight) (fg:red)old base(fg:default) and (fg:green)old tip(fg:default) (+bold fg:bright-blue)@@@@@@@@@@@@@@@@@@@@@@@@@(-weight fg:default)
    |}];
  return ()
;;

let%expect_test "a line that was wonky in a patdiff test" =
  let str =
    "    \027[0;41;30m-|\027[0;2m    let per_date_by_date,\027[0;2m error_by_date \
     =\027[0m"
  in
  print_endline Ansi_text.(minimize str |> visualize);
  [%expect
    {| (off bg:red fg:black)-|(off +faint)    let per_date_by_date, error_by_date =(off) |}];
  return ()
;;

let%expect_test "another line that was wonky in a patdiff test" =
  let str = "    │2 \027[0;41;30m-|\027[0m\027[0;31m\027[0m\027[0;31m_\027[0m    " in
  print_endline Ansi_text.(minimize str);
  print_endline Ansi_text.(minimize str |> visualize);
  [%expect
    {|
    │2 [0;41;30m-|[0;31m_[0m
    │2 (off bg:red fg:black)-|(off fg:red)_(off)
    |}];
  return ()
;;

let%expect_test "split handles unicode" =
  let str = "\027[1;41m0123\027[2;32m4👋👋5\027[4;64m6789\027[0m" in
  let before, after = Ansi_text.(parse str |> split ~pos:7) in
  let before = Ansi_text.to_string before in
  let after = Ansi_text.to_string after in
  print_endline before;
  print_endline after;
  print_endline (Ansi_text.visualize before);
  print_endline (Ansi_text.visualize after);
  print_endline (Ansi_text.strip before);
  print_endline (Ansi_text.strip after);
  [%expect
    {|
    [1;41m0123[2;32m4👋[49;22;39m
    [41;2;32m👋5[4;64m6789[0m
    (+bold bg:red)0123(+faint fg:green)4👋(bg:default -weight fg:default)
    (bg:red +faint fg:green)👋5(+uline ansi:64)6789(off)
    01234👋
    👋56789
    |}];
  return ()
;;
