open! Core
open! Async
open! Import

let prev =
  String.lstrip
    {|
_no_inline node transform(input :  u32x4)
  returns out :  u32x4
vars
  x0 :  u32[4],
  x1 :  u32[3],
  x2 :  u32[4],
  x3 :  u32[3]



z
|}
;;

let next =
  String.lstrip
    {|
_no_inline node transform'(input' :  u32x4)
  returns out' :  u32x4
vars
  x0' :  u32[4],
  x1' :  u32[3],
  x2' :  u32[4],
  x3' :  u32[3]


z
|}
;;

let%expect_test "~split_long_lines:false" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,11 +1,10(-weight) ============================================================
    (bg:red fg:black)-|(off fg:gray-12)_no_inline node(fg:red) transform(fg:gray-12)((fg:red)input(fg:gray-12) :  u32x4)(fg:default)
    (bg:red fg:black)-|(off fg:gray-12)  returns(fg:red) out(fg:gray-12) :  u32x4(fg:default)
    (bg:green fg:black)+|(off)_no_inline node(fg:green) transform'(fg:default)((fg:green)input'(fg:default) :  u32x4)
    (bg:green fg:black)+|(off)  returns(fg:green) out'(fg:default) :  u32x4
    (bg:gray fg:black) |(bg:default fg:default)vars
    (bg:red fg:black)-|(off fg:red)  x0(fg:gray-12) :  u32[4],(fg:default)
    (bg:red fg:black)-|(off fg:red)  x1(fg:gray-12) :  u32[3],(fg:default)
    (bg:red fg:black)-|(off fg:red)  x2(fg:gray-12) :  u32[4],(fg:default)
    (bg:red fg:black)-|(off fg:red)  x3(fg:gray-12) :  u32[3](fg:default)
    (bg:red fg:black)-|(off)
    (bg:green fg:black)+|(off fg:green)  x0'(fg:default) :  u32[4],
    (bg:green fg:black)+|(off fg:green)  x1'(fg:default) :  u32[3],
    (bg:green fg:black)+|(off fg:green)  x2'(fg:default) :  u32[4],
    (bg:green fg:black)+|(off fg:green)  x3'(fg:default) :  u32[3]
    (bg:gray fg:black) |(bg:default fg:default)
    (bg:gray fg:black) |(bg:default fg:default)
    (bg:gray fg:black) |(bg:default fg:default)z
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

let%expect_test "~split_long_lines:true" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-split-long-lines" ] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,11 +1,10(-weight) ============================================================
    (bg:red fg:black)-|(off fg:gray-12)_no_inline node(fg:red) transform(fg:gray-12)((fg:red)input(fg:gray-12) :  u32x4)(fg:default)
    (bg:red fg:black)-|(off fg:gray-12)  returns(fg:red) out(fg:gray-12) :  u32x4(fg:default)
    (bg:green fg:black)+|(off)_no_inline node(fg:green) transform'(fg:default)((fg:green)input'(fg:default) :  u32x4)
    (bg:green fg:black)+|(off)  returns(fg:green) out'(fg:default) :  u32x4
    (bg:gray fg:black) |(bg:default fg:default)vars
    (bg:red fg:black)-|(off fg:red)  x0(fg:gray-12) :  u32[4],(fg:default)
    (bg:red fg:black)-|(off fg:red)  x1(fg:gray-12) :  u32[3],(fg:default)
    (bg:red fg:black)-|(off fg:red)  x2(fg:gray-12) :  u32[4],(fg:default)
    (bg:red fg:black)-|(off fg:red)  x3(fg:gray-12) :  u32[3](fg:default)
    (bg:red fg:black)-|(off)
    (bg:green fg:black)+|(off fg:green)  x0'(fg:default) :  u32[4],
    (bg:green fg:black)+|(off fg:green)  x1'(fg:default) :  u32[3],
    (bg:green fg:black)+|(off fg:green)  x2'(fg:default) :  u32[4],
    (bg:green fg:black)+|(off fg:green)  x3'(fg:default) :  u32[3]
    (bg:gray fg:black) |(bg:default fg:default)
    (bg:gray fg:black) |(bg:default fg:default)
    (bg:gray fg:black) |(bg:default fg:default)z
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;
