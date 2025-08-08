open! Core
open! Async
open Expect_test_helpers_core
open Ansi_text.Control

let%expect_test "of string |> to string is idempotent" =
  print_endline (of_string_exn "\027[A" |> to_string);
  [%expect {| [A |}];
  print_endline (of_string_exn "\027[1B" |> to_string);
  [%expect {| [1B |}];
  print_endline (of_string_exn "\027[H" |> to_string);
  [%expect {| [H |}];
  print_endline (of_string_exn "\027[;H" |> to_string);
  (* Not literally idempotent, but ;H and H are equivalent. *)
  [%expect {| [H |}];
  print_endline (of_string_exn "\027[;2H" |> to_string);
  [%expect {| [;2H |}];
  print_endline (of_string_exn "\027[3;4H" |> to_string);
  [%expect {| [3;4H |}];
  return ()
;;

let%expect_test "visualization with to_string_hum" =
  print_endline (of_string_exn "\027[E" |> to_string_hum);
  [%expect {| (CursorNextLine) |}];
  print_endline (of_string_exn "\027[K" |> to_string_hum);
  [%expect {| (EraseLine:ToEnd) |}];
  print_endline (of_string_exn "\027[12C" |> to_string_hum);
  [%expect {| (CursorForward:12) |}];
  print_endline (of_string_exn "\027[1;2H" |> to_string_hum);
  [%expect {| (CursorToPos:1;2) |}];
  return ()
;;
