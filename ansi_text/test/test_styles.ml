open! Core
open! Async
open Expect_test_helpers_core
open Ansi_text.Attr

let test_delta old_style added_style =
  let delta = Ansi_text.Style.delta ~old_style ~added_style in
  print_endline (Ansi_text.Style.to_string_hum delta)
;;

let%expect_test "simple deltas" =
  let bold = [ Bold ] in
  let red = [ Fg (Standard Red) ] in
  let green = [ Fg (Standard Green) ] in
  let empty = [] in
  let off = [ Reset ] in
  (* Note that [delta] does not remove the bold here, it only ensures that [added_style]
     is applied. *)
  test_delta bold empty;
  [%expect {| |}];
  test_delta empty bold;
  [%expect {| (+bold) |}];
  test_delta bold bold;
  [%expect {| |}];
  test_delta bold off;
  [%expect {| (off) |}];
  test_delta off bold;
  [%expect {| (+bold) |}];
  test_delta bold red;
  [%expect {| (fg:red) |}];
  test_delta red bold;
  [%expect {| (+bold) |}];
  test_delta red green;
  [%expect {| (fg:green) |}];
  test_delta green red;
  [%expect {| (fg:red) |}];
  return ()
;;

let%expect_test "both styles have resets" =
  test_delta
    [ Underline; Reset; Fg (Standard Red); Bg (Standard Green); Italic ]
    [ Reset; Italic; Underline ];
  [%expect {| (off +italic +uline) |}];
  test_delta
    [ Underline; Fg (Standard Red); Reset; Bg (Standard Green); Italic ]
    [ Reset; Italic; Underline ];
  [%expect {| (bg:default +uline) |}];
  return ()
;;

let%expect_test "reset in the second style" =
  test_delta
    [ Bold; Underline; Fg (Standard Red); Bg (Standard Green) ]
    [ Blink; Reset; Italic; Underline ];
  [%expect {| (off +italic +uline) |}];
  return ()
;;

let%expect_test "reset in the first style" =
  test_delta
    [ Bold; Underline; Reset; Italic; Fg (Standard Red) ]
    [ Blink; Italic; Underline ];
  [%expect {| (+blink +uline) |}];
  return ()
;;

let%expect_test "no resets" =
  test_delta
    [ Bold; Underline; Italic; Fg (Standard Red) ]
    [ Blink; Italic; Underline; Bg Default ];
  [%expect {| (+blink bg:default) |}];
  return ()
;;
