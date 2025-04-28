open! Core
open! Async
open Import

let patdiff ~prev ~next extra_flags =
  patdiff ~extra_flags:("-keep-whitespace" :: extra_flags) ~prev ~next
;;

let%expect_test "Show added newline at start of input" =
  let%bind () = patdiff ~prev:"bar\n" ~next:"\n bar\n" [] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,2(-weight) ============================================================
    (bg:red fg:black)-|(off fg:gray-12)bar(fg:default)
    (bg:green fg:black)+|(off)
    (bg:green fg:black)+|(off +invert fg:green) (-invert fg:default)bar
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

let%expect_test "-unrefined works too" =
  let%bind () = patdiff ~prev:"bar\n" ~next:"\n bar\n" [ "-unrefined" ] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,2(-weight) ============================================================
    (bg:red fg:black)-|(bg:default fg:red)bar(fg:default)
    (bg:green fg:black)+|(bg:default fg:default)
    (bg:green fg:black)+|(bg:default fg:green) bar(fg:default)
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

let%expect_test "-ascii works too (it implies -unrefined)" =
  let%bind () = patdiff ~prev:"bar\n" ~next:"\n bar\n" [ "-ascii" ] in
  [%expect
    {|
    ------ prev/file
    ++++++ next/file
    @|-1,1 +1,2 ============================================================
    -|bar
    +|
    +| bar
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

let%expect_test "Show leading whitespace" =
  let%bind () = patdiff ~prev:"bar\n" ~next:" bar\n" [] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,1(-weight) ============================================================
    (bg:red fg:black)-|(off fg:gray-12)bar(fg:default)
    (bg:green fg:black)+|(off +invert fg:green) (-invert fg:default)bar
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

let%expect_test "Show internal whitespace" =
  let%bind () = patdiff ~prev:"foo bar\n" ~next:"foo  bar\n" [] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,1(-weight) ============================================================
    (bg:red fg:black)-|(off fg:gray-12)foo(+invert fg:red) (-invert fg:gray-12)bar(fg:default)
    (bg:green fg:black)+|(off)foo(+invert fg:green)  (-invert fg:default)bar
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

let%expect_test "Show trailing whitespace" =
  let%bind () = patdiff ~prev:"foo\n" ~next:"foo \n" [] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,1(-weight) ============================================================
    (bg:red fg:black)-|(off fg:gray-12)foo(fg:default)
    (bg:green fg:black)+|(off)foo(+invert fg:green) (-invert fg:default)
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;
