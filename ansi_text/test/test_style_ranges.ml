open! Core
open! Async
open Expect_test_helpers_core
open Ansi_text.Text_with_style_ranges

let%expect_test "find ranges in a patdiff body" =
  let diff =
    {|[41;30m-[49;39m[0m[90m(** The wire[39m[1;31m kind[22;39m[90m of a bricks log *)[39m
[41;30m-[49;39m[0m[90mval[39m[1;31m kind[22;39m[90m : Log_wire.wire_[39m[1;31mkind[22;39m
[42;30m+[49;39m[0m(** The wire[32m class[39m of a bricks log *)
[42;30m+[49;39m[0mval[32m wire_class[39m : Log_wire.wire_[32mclass[39m|}
  in
  let with_ranges = Ansi_text.parse diff |> of_text_with_ansi |> Option.value_exn in
  print_endline (to_unstyled with_ranges);
  print_endline (to_string_hum with_ranges);
  [%expect
    {|
    -(** The wire kind of a bricks log *)
    -val kind : Log_wire.wire_kind
    +(** The wire class of a bricks log *)
    +val wire_class : Log_wire.wire_class
    (bg:red fg:black)-(bg:default fg:default)(fg:gray)(off)(** The wire(fg:default)(+bold fg:red) kind(-weight fg:default)(fg:gray) of a bricks log *)
    (fg:default)(bg:red fg:black)-(bg:default fg:default)(fg:gray)val(fg:default)(+bold fg:red) kind(-weight fg:default)(fg:gray) : Log_wire.wire_(fg:default)(+bold fg:red)kind
    (-weight fg:default)(bg:green fg:black)+(bg:default fg:default)(off)(** The wire(fg:green) class(fg:default) of a bricks log *)
    (bg:green fg:black)+(bg:default fg:default)val(fg:green) wire_class(fg:default) : Log_wire.wire_(fg:green)class(fg:default)
    |}];
  return ()
;;

let header =
  {|[1;94m@@@@@@@@[22;39m      [35mView 1/1 : feature-ddiff[39m      [1;94m@@@@@@@@[22;39m
[1;94m@@@@@@@@[22;39m [1;48;2;80;40;80;37m--[22;49;39m [1mdiff of[22m [31mold base[39m [1m/[22m [32mold tip[39m [1m1,44[22m [1;94m@@@@@@@@[22;39m
[1;94m@@@@@@@@[22;39m [1;48;2;20;60;120;37m++[22;49;39m [1mdiff of[22m [31mnew base[39m [1m/[22m [32mnew tip[39m [1m1,31[22m [1;94m@@@@@@@@[22;39m|}
;;

let%expect_test "find ranges in a patdiff4 header" =
  let with_ranges = Ansi_text.parse header |> of_text_with_ansi |> Option.value_exn in
  print_endline (to_unstyled with_ranges);
  print_endline (to_string_hum with_ranges);
  [%expect
    {|
    @@@@@@@@      View 1/1 : feature-ddiff      @@@@@@@@
    @@@@@@@@ -- diff of old base / old tip 1,44 @@@@@@@@
    @@@@@@@@ ++ diff of new base / new tip 1,31 @@@@@@@@
    (+bold fg:bright-blue)@@@@@@@@(-weight fg:default)      (fg:magenta)View 1/1 : feature-ddiff(fg:default)      (+bold fg:bright-blue)@@@@@@@@
    (-weight fg:default)(+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:rgb256-80-40-80 fg:white)--(-weight bg:default fg:default) (+bold)diff of(-weight) (fg:red)old base(fg:default) (+bold)/(-weight) (fg:green)old tip(fg:default) (+bold)1,44(-weight) (+bold fg:bright-blue)@@@@@@@@
    (-weight fg:default)(+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:rgb256-20-60-120 fg:white)++(-weight bg:default fg:default) (+bold)diff of(-weight) (fg:red)new base(fg:default) (+bold)/(-weight) (fg:green)new tip(fg:default) (+bold)1,31(-weight) (+bold fg:bright-blue)@@@@@@@@(-weight fg:default)
    |}];
  return ()
;;

let%expect_test "find ranges in side-by-side patdiff4 output" =
  let diff =
    {|[1;48;2;200;100;0;37m!![22;49;39m[41;30m-[49;39m[0m[90mval[39m[1;31m kind[22;39m[90m :[48;2;80;40;80m B.Log[49m.wire_[39m[1;31mkind[22;39m               │[1;48;2;200;100;0;37m!![22;49;39m[41;30m-[49;39m[0m[90mval[39m[1;31m kind[22;39m[90m :[48;2;20;60;120m Log_wire[49m.wire_[39m[1;31mkind[22;39m
[1;48;2;200;100;0;37m!![22;49;39m[42;30m+[49;39m[0mval[32m wire_class[39m :[48;2;80;40;80m B.Log[49m.wire_[32mclass[39m        │[1;48;2;200;100;0;37m!![22;49;39m[42;30m+[49;39m[0mval[32m wire_class[39m :[48;2;20;60;120m Log_wire[49m.wire_[32mclass[39m|}
  in
  let with_ranges = Ansi_text.parse diff |> of_text_with_ansi |> Option.value_exn in
  print_endline (to_unstyled with_ranges);
  print_s [%sexp (with_ranges.ranges : Ansi_text.Style_ranges.t)];
  [%expect
    {|
    !!-val kind : B.Log.wire_kind               │!!-val kind : Log_wire.wire_kind
    !!+val wire_class : B.Log.wire_class        │!!+val wire_class : Log_wire.wire_class
    (((start 0)
      (end_  2)
      (style (
        Bold
        (Bg (
          Rgb256 (
            (r 200)
            (g 100)
            (b 0))))
        (Fg (Standard White)))))
     ((start 2)
      (end_  3)
      (style (
        (Bg (Standard Red))
        (Fg (Standard Black)))))
     ((start 3)
      (end_  6)
      (style ((Fg (Bright Black)))))
     ((start 3)
      (end_  48)
      (style (Reset)))
     ((start 6)
      (end_  11)
      (style (Bold (Fg (Standard Red)))))
     ((start 11)
      (end_  25)
      (style ((Fg (Bright Black)))))
     ((start 13)
      (end_  19)
      (style ((
        Bg (
          Rgb256 (
            (r 80)
            (g 40)
            (b 80)))))))
     ((start 25)
      (end_  29)
      (style (Bold (Fg (Standard Red)))))
     ((start 45)
      (end_  47)
      (style (
        Bold
        (Bg (
          Rgb256 (
            (r 200)
            (g 100)
            (b 0))))
        (Fg (Standard White)))))
     ((start 47)
      (end_  48)
      (style (
        (Bg (Standard Red))
        (Fg (Standard Black)))))
     ((start 48)
      (end_  51)
      (style ((Fg (Bright Black)))))
     ((start 51)
      (end_  56)
      (style (Bold (Fg (Standard Red)))))
     ((start 56)
      (end_  73)
      (style ((Fg (Bright Black)))))
     ((start 58)
      (end_  67)
      (style ((
        Bg (
          Rgb256 (
            (r 20)
            (g 60)
            (b 120)))))))
     ((start 73)
      (end_  77)
      (style (Bold (Fg (Standard Red)))))
     ((start 77)
      (end_  79)
      (style (
        Bold
        (Bg (
          Rgb256 (
            (r 200)
            (g 100)
            (b 0))))
        (Fg (Standard White)))))
     ((start 79)
      (end_  80)
      (style (
        (Bg (Standard Green))
        (Fg (Standard Black)))))
     ((start 80)
      (end_  125)
      (style (Reset)))
     ((start 83)
      (end_  94)
      (style ((Fg (Standard Green)))))
     ((start 96)
      (end_  102)
      (style ((
        Bg (
          Rgb256 (
            (r 80)
            (g 40)
            (b 80)))))))
     ((start 108)
      (end_  113)
      (style ((Fg (Standard Green)))))
     ((start 122)
      (end_  124)
      (style (
        Bold
        (Bg (
          Rgb256 (
            (r 200)
            (g 100)
            (b 0))))
        (Fg (Standard White)))))
     ((start 124)
      (end_  125)
      (style (
        (Bg (Standard Green))
        (Fg (Standard Black)))))
     ((start 128)
      (end_  139)
      (style ((Fg (Standard Green)))))
     ((start 141)
      (end_  150)
      (style ((
        Bg (
          Rgb256 (
            (r 20)
            (g 60)
            (b 120)))))))
     ((start 156)
      (end_  161)
      (style ((Fg (Standard Green))))))
    |}];
  return ()
;;

let%expect_test "apply" =
  let text = Ansi_text.Text.of_string "some uninteresting text" in
  let style = Ansi_text.Attr.[ Bold; Italic; Bg (Bright Magenta); Fg (Bright Green) ] in
  let range = [ { Ansi_text.Style_ranges.start = 5; end_ = 18; style } ] in
  let styled_text = Ansi_text.Style_ranges.apply ~text range in
  print_endline (Ansi_text.to_string_hum styled_text);
  [%expect
    {| some (+bold +italic bg:bright-magenta fg:bright-green)uninteresting(-weight -italic bg:default fg:default) text |}];
  return ()
;;

let%expect_test "unstyle_between" =
  let styled = Ansi_text.parse header |> of_text_with_ansi |> Option.value_exn in
  let partially_styled = unstyle_between styled ~start:52 ~end_:104 in
  print_endline (to_string_hum partially_styled);
  [%expect
    {|
    (+bold fg:bright-blue)@@@@@@@@(-weight fg:default)      (fg:magenta)View 1/1 : feature-ddiff(fg:default)      (+bold fg:bright-blue)@@@@@@@@
    (-weight fg:default)@@@@@@@@ -- diff of old base / old tip 1,44 @@@@@@@@
    (+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:rgb256-20-60-120 fg:white)++(-weight bg:default fg:default) (+bold)diff of(-weight) (fg:red)new base(fg:default) (+bold)/(-weight) (fg:green)new tip(fg:default) (+bold)1,31(-weight) (+bold fg:bright-blue)@@@@@@@@(-weight fg:default)
    |}];
  return ()
;;

let%expect_test "split" =
  let first, second =
    Ansi_text.parse header |> of_text_with_ansi |> Option.value_exn |> split ~pos:52
  in
  print_endline (to_string_hum first);
  print_endline "";
  print_endline (to_string_hum second);
  [%expect
    {|
    (+bold fg:bright-blue)@@@@@@@@(-weight fg:default)      (fg:magenta)View 1/1 : feature-ddiff(fg:default)      (+bold fg:bright-blue)@@@@@@@@
    (-weight fg:default)

    (+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:rgb256-80-40-80 fg:white)--(-weight bg:default fg:default) (+bold)diff of(-weight) (fg:red)old base(fg:default) (+bold)/(-weight) (fg:green)old tip(fg:default) (+bold)1,44(-weight) (+bold fg:bright-blue)@@@@@@@@
    (-weight fg:default)(+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:rgb256-20-60-120 fg:white)++(-weight bg:default fg:default) (+bold)diff of(-weight) (fg:red)new base(fg:default) (+bold)/(-weight) (fg:green)new tip(fg:default) (+bold)1,31(-weight) (+bold fg:bright-blue)@@@@@@@@(-weight fg:default)
    |}];
  return ()
;;
