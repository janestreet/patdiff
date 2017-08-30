open! Core
open! Async
open Import

let patdiff ~mine ~other extra_flags =
  patdiff ~extra_flags:("-keep-whitespace" :: extra_flags) ~mine ~other

let%expect_test "Show added newline at start of input" =
  let%bind () = patdiff ~mine:"bar\n" ~other:"\n bar\n" [] in
  [%expect {|
        (fg:red)------ (+bold)mine
        (fg:green)++++++ (+bold)other
        (fg:black)@|(+bold)-1,1 +1,2(off) ============================================================
        (fg:black bg:red)-|(off)bar
        (fg:black bg:green)+|
        (fg:black bg:green)+|(fg:green) (off)bar
        ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "-unrefined works too" =
  let%bind () = patdiff ~mine:"bar\n" ~other:"\n bar\n" [ "-unrefined" ] in
  [%expect {|
      (fg:red)------ (+bold)mine
      (fg:green)++++++ (+bold)other
      (fg:black)@|(+bold)-1,1 +1,2(off) ============================================================
      (fg:black bg:red)-|(fg:red)bar
      (fg:black bg:green)+|
      (fg:black bg:green)+|(fg:green) bar
      ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "-ascii works too (it implies -unrefined)" =
  let%bind () = patdiff ~mine:"bar\n" ~other:"\n bar\n" [ "-ascii" ] in
  [%expect {xxx|
      ------ mine
      ++++++ other
      @|-1,1 +1,2 ============================================================
      -|bar
      +|
      +| bar
      ("Unclean exit" (Exit_non_zero 1)) |xxx}]
;;

let%expect_test "Show leading whitespace" =
  let%bind () = patdiff ~mine:"bar\n" ~other:" bar\n" [] in
  [%expect {|
        (fg:red)------ (+bold)mine
        (fg:green)++++++ (+bold)other
        (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
        (fg:black bg:red)-|(off)bar
        (fg:black bg:green)+|(fg:green) (off)bar
        ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "Show internal whitespace" =
  let%bind () = patdiff ~mine:"foo bar\n" ~other:"foo  bar\n" [] in
  [%expect {|
      (fg:red)------ (+bold)mine
      (fg:green)++++++ (+bold)other
      (fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
      (fg:black bg:red)-|(off)foo(fg:red) (off)bar
      (fg:black bg:green)+|(off)foo(fg:green)  (off)bar
      ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "Show trailing whitespace" =
  let%bind () = patdiff ~mine:"foo\n" ~other:"foo \n" [] in
  [%expect {|
(fg:red)------ (+bold)mine
(fg:green)++++++ (+bold)other
(fg:black)@|(+bold)-1,1 +1,1(off) ============================================================
(fg:black bg:red)-|(off)foo
(fg:black bg:green)+|(off)foo(fg:green)
("Unclean exit" (Exit_non_zero 1)) |}]
;;
