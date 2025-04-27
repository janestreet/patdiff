open! Core
open! Async
open! Import

let prev =
  {|
min=0|max=10
|}
;;

let next =
  {|
min=5|max=10
|}
;;

let%expect_test "pipe" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,2 +1,2(-weight) ============================================================
    (bg:gray fg:black) |(bg:default fg:default)
    (bg:red fg:black)-|(off fg:gray-12)min=(fg:red)0(fg:gray-12)|max=10(fg:default)
    (bg:green fg:black)+|(off)min=(fg:green)5(fg:default)|max=10
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;
