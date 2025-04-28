open! Core
open! Async
open Expect_test_helpers_core

let%expect_test "simple deltas" =
  let bold = [ Ansi_text.Attr.Bold ] in
  let red = [ Ansi_text.Attr.Fg (Standard Red) ] in
  let green = [ Ansi_text.Attr.Fg (Standard Green) ] in
  let empty = [] in
  let off = [ Ansi_text.Attr.Reset ] in
  let test_delta old_style added_style =
    let delta = Ansi_text.Style.delta ~old_style ~added_style in
    print_endline (Ansi_text.Style.to_string_hum delta)
  in
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
