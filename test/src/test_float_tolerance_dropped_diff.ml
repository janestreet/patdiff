open! Core
open! Async
open! Import

(* Regression test for a case where we used to drop the first line of this diff. *)

let prev =
  {|
((foo (1 2))
 (bar 0.5%))
|}
;;

let next =
  {|
()
|}
;;

let%expect_test "default" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,3 +1,2(-weight) ============================================================
    (bg:gray fg:black) |(bg:default fg:default)
    (bg:yellow fg:black)!|(bg:default fg:default)((fg:red)(foo (1 2))(fg:default)
    (bg:yellow fg:black)!|(bg:default fg:red) (bar 0.5%)(fg:default))
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

let%expect_test "-float-tolerance 0x" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-float-tolerance"; "0x" ] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,3 +1,2(-weight) ============================================================
    (bg:gray fg:black) |(bg:default fg:default)
    (bg:yellow fg:black)!|(bg:default fg:default)((fg:red)(foo (1 2))(fg:default)
    (bg:yellow fg:black)!|(bg:default fg:red) (bar 0.5%)(fg:default))
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

let%expect_test "-float-tolerance 0x -no-semantic-cleanup" =
  let%bind () =
    patdiff ~prev ~next ~extra_flags:[ "-float-tolerance"; "0x"; "-no-semantic-cleanup" ]
  in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,3 +1,2(-weight) ============================================================
    (bg:gray fg:black) |(bg:default fg:default)
    (bg:yellow fg:black)!|(bg:default fg:default)((fg:red)(foo (1 2))(fg:default)
    (bg:yellow fg:black)!|(bg:default fg:red) (bar 0.5%)(fg:default))
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;
