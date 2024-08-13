open! Core
open! Import
open Patdiff_kernel
module Patdiff_core = Patdiff_core.Without_unix

let%expect_test "refine does not raise with ~split_long_lines:true and no controlling tty"
  =
  let keep_ws = false in
  let hunks =
    Patdiff_core.diff
      ~context:Configuration.default_context
      ~line_big_enough:Configuration.default_line_big_enough
      ~keep_ws
      ~find_moves:false
      ~prev:[| "hello"; "world" |]
      ~next:[| "good bye"; "world" |]
  in
  let hunks =
    Patdiff_core.refine
      ~rules:Format.Rules.default
      ~output:Ascii
      ~split_long_lines:true
      ~produce_unified_lines:false
      ~keep_ws
      ~interleave:true
      ~word_big_enough:Configuration.default_word_big_enough
      hunks
  in
  print_s [%sexp (hunks : Hunks.t)];
  [%expect
    {|
    ((
      (prev_start 1)
      (prev_size  2)
      (next_start 1)
      (next_size  2)
      (ranges (
        (Replace
          (hello)
          ("good bye")
          ())
        (Same ((world world)))))))
    |}]
;;

let print_patdiff prev next =
  print_endline
    (Patdiff_core.patdiff
       ~produce_unified_lines:false
       ~output:Ascii
       ~prev:{ name = "old"; text = prev }
       ~next:{ name = "new"; text = next }
       ())
;;

let%expect_test "extra empty lines are not added" =
  print_patdiff "hello world" "hello";
  [%expect
    {|
    -1,1 +1,1
    -|hello world
    +|hello
    |}];
  print_patdiff "hello world" "world";
  [%expect
    {|
    -1,1 +1,1
    -|hello world
    +|world
    |}];
  print_patdiff "hello world" "";
  [%expect
    {|
    -1,1 +1,0
    -|hello world
    |}]
;;

let%expect_test "extra empty lines are not added for consecutive long lines" =
  let prev =
    String.strip
      [%string
        {|
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 0000000000000000000000000000000000000000
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb 1111111111111111111111111111111111111111
|}]
  in
  let next =
    String.strip
      [%string
        {|
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
|}]
  in
  print_patdiff prev next;
  [%expect
    {|
    -1,2 +1,2
    -|aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 0000000000000000000000000000000000000000
    -|bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb 1111111111111111111111111111111111111111
    +|aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    +|bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
    |}]
;;
