open! Core
open! Async
open Expect_test_helpers_core
open Ansi_text.Control

let%expect_test "of string |> to string is idempotent" =
  print_endline (of_csi ~params:"" ~terminal:'A' |> to_string);
  [%expect {| [A |}];
  print_endline (of_csi ~params:"1" ~terminal:'B' |> to_string);
  [%expect {| [1B |}];
  print_endline (of_csi ~params:"" ~terminal:'H' |> to_string);
  [%expect {| [H |}];
  print_endline (of_csi ~params:";" ~terminal:'H' |> to_string);
  (* Not literally idempotent, but ;H and H are equivalent. *)
  [%expect {| [H |}];
  print_endline (of_csi ~params:";2" ~terminal:'H' |> to_string);
  [%expect {| [;2H |}];
  print_endline (of_csi ~params:"3;4" ~terminal:'H' |> to_string);
  [%expect {| [3;4H |}];
  return ()
;;

let%expect_test "visualization with to_string_hum" =
  print_endline (of_csi ~params:"" ~terminal:'E' |> to_string_hum);
  [%expect {| (CursorNextLine) |}];
  print_endline (of_csi ~params:"" ~terminal:'K' |> to_string_hum);
  [%expect {| (EraseLine:ToEnd) |}];
  print_endline (of_csi ~params:"12" ~terminal:'C' |> to_string_hum);
  [%expect {| (CursorForward:12) |}];
  print_endline (of_csi ~params:"1;2" ~terminal:'H' |> to_string_hum);
  [%expect {| (CursorToPos:1;2) |}];
  return ()
;;
