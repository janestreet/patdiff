open! Core
open! Async
open! Import

let prev =
  {|
  upon shutdown_timer (fun () ->
    eprintf "shutting down\n";
    graceful_shutdown t `Core);
  (* do some other thing *)
  write_startup_data t
|}
;;

let next =
  {|
  upon shutdown_timer (fun () ->
    eprintf "shutting down\n";
    graceful_shutdown t (Core_because "sheduled shutdown");
    (* do some other thing *)
    write_startup_data t)
|}
;;

let%expect_test "default" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,6 +1,6(off) ============================================================
    (fg:black) |
    (fg:black) |(off)  upon shutdown_timer (fun () ->
    (fg:black) |(off)    eprintf "shutting down\n";
    (fg:black bg:red)-|(off)    graceful_shutdown t(fg:red) `(off)Core);
    (fg:black bg:green)+|(off)    graceful_shutdown t(fg:green) ((off)Core(fg:green)_because "sheduled shutdown"(off));
    (fg:black) |(off)    (* do some other thing *)
    (fg:black bg:yellow)!|(off)    write_startup_data t(fg:green))
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "-no-semantic-cleanup" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-no-semantic-cleanup" ] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,6 +1,6(off) ============================================================
    (fg:black) |
    (fg:black) |(off)  upon shutdown_timer (fun () ->
    (fg:black) |(off)    eprintf "shutting down\n";
    (fg:black bg:red)-|(off)    graceful_shutdown t(fg:red) `(off)Core);
    (fg:black bg:green)+|(off)    graceful_shutdown t(fg:green) ((off)Core(fg:green)_because "sheduled shutdown"(off));
    (fg:black) |(off)    (* do some other thing *)
    (fg:black bg:yellow)!|(off)    write_startup_data t(fg:green))
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;
