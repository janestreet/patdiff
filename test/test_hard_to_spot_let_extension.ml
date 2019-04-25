open! Core
open! Async
open! Import

let prev =
  {|
  begin
    if some_uncommon_test foo then
      f ?foo ();
    if some_other_test bar then
      do_something_important
  end
|}
;;


let next =
  {|
  begin
    if some_uncommon_test foo then
      let foo = Option.value foo ~default:F.nice_foo in
      f ~foo ();
    if some_other_test bar then
      do_something_important
  end
|}
;;

let%expect_test "default" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,7 +1,8(off) ============================================================
    (fg:black) |
    (fg:black) |(off)  begin
    (fg:black) |(off)    if some_uncommon_test foo then
    (fg:black bg:red)-|(fg:red)      f ?foo(off) ();
    (fg:black bg:green)+|(fg:green)      let foo = Option.value foo ~default:F.nice_foo in
    (fg:black bg:green)+|(fg:green)      f ~foo(off) ();
    (fg:black) |(off)    if some_other_test bar then
    (fg:black) |(off)      do_something_important
    (fg:black) |(off)  end
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "-no-semantic-cleanup" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-no-semantic-cleanup" ] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,7 +1,8(off) ============================================================
    (fg:black) |
    (fg:black) |(off)  begin
    (fg:black) |(off)    if some_uncommon_test foo then
    (fg:black bg:red)-|(off)      f(fg:red) ?foo(off) ();
    (fg:black bg:green)+|(fg:green)      let foo = Option.value foo ~default:F.nice_foo in
    (fg:black bg:green)+|(off)      f(fg:green) ~foo(off) ();
    (fg:black) |(off)    if some_other_test bar then
    (fg:black) |(off)      do_something_important
    (fg:black) |(off)  end
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let next =
  {|
  begin
    if some_uncommon_test foo then
      let foo = Option.value foo ~default:F.nice_foo in
      f ~foo ();
      if some_other_test bar then
        do_something_important
  end
|}
;;

let%expect_test "default indent" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,7 +1,8(off) ============================================================
    (fg:black) |
    (fg:black) |(off)  begin
    (fg:black) |(off)    if some_uncommon_test foo then
    (fg:black bg:red)-|(fg:red)      f ?foo(off) ();
    (fg:black bg:green)+|(fg:green)      let foo = Option.value foo ~default:F.nice_foo in
    (fg:black bg:green)+|(fg:green)      f ~foo(off) ();
    (fg:black) |(off)      if some_other_test bar then
    (fg:black) |(off)        do_something_important
    (fg:black) |(off)  end
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;

let%expect_test "-no-semantic-cleanup indent" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-no-semantic-cleanup" ] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,7 +1,8(off) ============================================================
    (fg:black) |
    (fg:black) |(off)  begin
    (fg:black) |(off)    if some_uncommon_test foo then
    (fg:black bg:red)-|(off)      f(fg:red) ?foo(off) ();
    (fg:black bg:green)+|(fg:green)      let foo = Option.value foo ~default:F.nice_foo in
    (fg:black bg:green)+|(off)      f(fg:green) ~foo(off) ();
    (fg:black) |(off)      if some_other_test bar then
    (fg:black) |(off)        do_something_important
    (fg:black) |(off)  end
    ("Unclean exit" (Exit_non_zero 1)) |}]
;;
