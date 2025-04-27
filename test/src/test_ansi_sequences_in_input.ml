open! Core
open! Async
open Import

let aansi = "\027[0;1m\n"
let bansi = "\027[0;2m\n"

let%expect_test "ansi escape code in input" =
  let%bind () = patdiff ~extra_flags:[] ~prev:aansi ~next:bansi in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,1(-weight) ============================================================
    (bg:red fg:black)-|(off +bold fg:default)
    (bg:green fg:black)+|(off +faint fg:default)
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

let acolored_text = "\027[0;33myellow text\027[0m\n"
let bcolored_text = "\027[0;34mblue text\027[0m\n"

let%expect_test "colored text" =
  let%bind () = patdiff ~extra_flags:[] ~prev:acolored_text ~next:bcolored_text in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,1(-weight) ============================================================
    (bg:red fg:black)-|(off fg:yellow)yellow(fg:gray-12) text(fg:default)
    (bg:green fg:black)+|(off fg:blue)blue(fg:default) text(off)
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;
