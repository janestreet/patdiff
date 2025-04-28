open! Core
open! Async
open! Import

let%expect_test "message when non-ASCII text files differ" =
  let prev =
    {|┌Signals──┐┌Values───┐┌Waves─────────┐
      │clock    ││         ││┌───┐   ┌───┐ │
      │         ││         ││    └───┘   └─│
|}
  in
  let next =
    {|┌Signals──┐┌Values───┐┌Waves─────────┐
      │clock2   ││         ││┌───┐   ┌───┐ │
      │         ││         ││    └───┘   └─│
|}
  in
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-location-style"; "omake" ] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    File "prev/file", line 2, characters 0-1:
    (bg:gray fg:black) |(bg:default fg:default)┌Signals──┐┌Values───┐┌Waves─────────┐
    (bg:red fg:black)-|(off fg:red)      │clock(fg:gray-12)    ││         ││┌───┐   ┌───┐ │(fg:default)
    (bg:green fg:black)+|(off fg:green)      │clock2(fg:default)   ││         ││┌───┐   ┌───┐ │
    (bg:gray fg:black) |(bg:default fg:default)      │         ││         ││    └───┘   └─│
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

let%expect_test "message when binary files differ" =
  let len = 100 in
  let bytes = Bytes.make len 'z' in
  Bytes.set bytes 50 '\000';
  let prev = Bytes.to_string bytes in
  Bytes.set bytes 51 '\001';
  (* change something *)
  let next = Bytes.to_string bytes in
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-location-style"; "omake" ] in
  [%expect
    {|
    File "prev/file", line 1, characters 0-1:
      File "next/file"
      binary files differ

    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;
