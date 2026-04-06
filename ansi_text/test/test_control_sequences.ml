open! Core
open! Async
open Expect_test_helpers_core
open Ansi_text.Control

let%expect_test "of_csi_params |> to_string round-trips" =
  print_endline (of_csi_params [] ~terminal:'A' |> to_string);
  [%expect {| [A |}];
  print_endline (of_csi_params [ Some 1 ] ~terminal:'B' |> to_string);
  [%expect {| [1B |}];
  print_endline (of_csi_params [] ~terminal:'H' |> to_string);
  [%expect {| [H |}];
  print_endline (of_csi_params [ None; None ] ~terminal:'H' |> to_string);
  (* Not literally idempotent, but ;H and H are equivalent. *)
  [%expect {| [H |}];
  print_endline (of_csi_params [ None; Some 2 ] ~terminal:'H' |> to_string);
  [%expect {| [;2H |}];
  print_endline (of_csi_params [ Some 3; Some 4 ] ~terminal:'H' |> to_string);
  [%expect {| [3;4H |}];
  return ()
;;

let%expect_test "visualization with to_string_hum" =
  print_endline (of_csi_params [] ~terminal:'E' |> to_string_hum);
  [%expect {| (CursorNextLine) |}];
  print_endline (of_csi_params [] ~terminal:'K' |> to_string_hum);
  [%expect {| (EraseLine:ToEnd) |}];
  print_endline (of_csi_params [ Some 12 ] ~terminal:'C' |> to_string_hum);
  [%expect {| (CursorForward:12) |}];
  print_endline (of_csi_params [ Some 1; Some 2 ] ~terminal:'H' |> to_string_hum);
  [%expect {| (CursorToPos:1;2) |}];
  return ()
;;
