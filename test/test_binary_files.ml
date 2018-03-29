open! Core
open! Async
open! Import

let%expect_test "message when non-ASCII text files differ" =
  let mine = "\
    ┌Signals──┐┌Values───┐┌Waves─────────┐
    │clock    ││         ││┌───┐   ┌───┐ │
    │         ││         ││    └───┘   └─│\n" in
  let other = "\
    ┌Signals──┐┌Values───┐┌Waves─────────┐
    │clock2   ││         ││┌───┐   ┌───┐ │
    │         ││         ││    └───┘   └─│\n" in
  let%bind () = patdiff ~mine ~other ~extra_flags:[ "-location-style"; "omake" ] in
  [%expect {|
    (fg:red)------ (+bold)mine
    (fg:green)++++++ (+bold)other
    File "mine", line 2, characters 0-1:
    (fg:black) |(off)┌Signals──┐┌Values───┐┌Waves─────────┐
    (fg:black bg:red)-|(fg:red)    │clock(off)    ││         ││┌───┐   ┌───┐ │
    (fg:black bg:green)+|(fg:green)    │clock2(off)   ││         ││┌───┐   ┌───┐ │
    (fg:black) |(off)    │         ││         ││    └───┘   └─│
    ("Unclean exit" (Exit_non_zero 1)) |}];
;;

let%expect_test "message when binary files differ" =
  let len = 100 in
  (** This is not valid UTF-8: it's all continuation bytes, with no sequence leading
      bytes before them. *)
  let bytes = Bytes.make len '\x80' in
  Bytes.set bytes (len - 1) '\n';  (* enforce trailing newline *)
  let mine = Bytes.to_string bytes in
  Bytes.set bytes (len - 2) '\n';  (* change something *)
  let other = Bytes.to_string bytes in
  let%bind () = patdiff ~mine ~other ~extra_flags:[ "-location-style"; "omake" ] in
  [%expect {|
    File "mine", line 1, characters 0-1:
      File "other"
      binary files differ

    ("Unclean exit" (Exit_non_zero 1)) |}];
;;
