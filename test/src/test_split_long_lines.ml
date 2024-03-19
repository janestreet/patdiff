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
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,11 +1,10(off) ============================================================
    (fg:black bg:red)-|(off)_no_inline node(fg:red) transform(off)((fg:red)input(off) :  u32x4)
    (fg:black bg:red)-|(off)  returns(fg:red) out(off) :  u32x4
    (fg:black bg:green)+|(off)_no_inline node(fg:green) transform'(off)((fg:green)input'(off) :  u32x4)
    (fg:black bg:green)+|(off)  returns(fg:green) out'(off) :  u32x4
    (fg:black) |(off)vars
    (fg:black bg:red)-|(fg:red)  x0(off) :  u32[4],
    (fg:black bg:red)-|(fg:red)  x1(off) :  u32[3],
    (fg:black bg:red)-|(fg:red)  x2(off) :  u32[4],
    (fg:black bg:red)-|(fg:red)  x3(off) :  u32[3]
    (fg:black bg:red)-|
    (fg:black bg:green)+|(fg:green)  x0'(off) :  u32[4],
    (fg:black bg:green)+|(fg:green)  x1'(off) :  u32[3],
    (fg:black bg:green)+|(fg:green)  x2'(off) :  u32[4],
    (fg:black bg:green)+|(fg:green)  x3'(off) :  u32[3]
    (fg:black) |
    (fg:black) |
    (fg:black) |(off)z
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

let%expect_test "~split_long_lines:true" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-split-long-lines" ] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,11 +1,10(off) ============================================================
    (fg:black bg:red)-|(off)_no_inline node(fg:red) transform(off)((fg:red)input(off) :  u32x4)
    (fg:black bg:red)-|(off)  returns(fg:red) out(off) :  u32x4
    (fg:black bg:green)+|(off)_no_inline node(fg:green) transform'(off)((fg:green)input'(off) :  u32x4)
    (fg:black bg:green)+|(off)  returns(fg:green) out'(off) :  u32x4
    (fg:black) |(off)vars
    (fg:black bg:red)-|(fg:red)  x0(off) :  u32[4],
    (fg:black bg:red)-|(fg:red)  x1(off) :  u32[3],
    (fg:black bg:red)-|(fg:red)  x2(off) :  u32[4],
    (fg:black bg:red)-|(fg:red)  x3(off) :  u32[3]
    (fg:black bg:red)-|
    (fg:black bg:green)+|(fg:green)  x0'(off) :  u32[4],
    (fg:black bg:green)+|(fg:green)  x1'(off) :  u32[3],
    (fg:black bg:green)+|(fg:green)  x2'(off) :  u32[4],
    (fg:black bg:green)+|(fg:green)  x3'(off) :  u32[3]
    (fg:black) |
    (fg:black) |
    (fg:black) |(off)z
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;
