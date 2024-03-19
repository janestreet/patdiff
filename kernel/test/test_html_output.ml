open! Core
open! Import
open Patdiff_kernel
module Patdiff_core = Patdiff_core.Without_unix

let%expect_test "test outputting a move to HTML" =
  let prev =
    {|
Some code that is going to get moved somewhere. Make it long so
things are really similar. We only match on at least 3 lines
so make it 3 lines long.
a
b
c
d
e
f
this is deleted
|}
  in
  let next =
    {|
a
b
c
Some code that is going to get moved somewhere. Make it long so
things are really differs. We only match on at least 3 lines
so make it 3 lines long.
d
e
f
Some code that is going to get moved somewhere. Make it long so
things are really similar. We only match on at least 3 lines
so make it 3 lines long.
|}
  in
  let prev : Diff_input.t = { name = "old"; text = prev } in
  let next : Diff_input.t = { name = "new"; text = next } in
  printf
    "%s\n"
    (Patdiff_core.patdiff
       ~find_moves:true
       ~prev
       ~next
       ~output:Html
       ~produce_unified_lines:false
       ());
  [%expect
    {|
    <pre style="font-family:consolas,monospace">
    -1,11 +1,13

    <span style="color:#880088"><span style="font-weight:bold"><|</span></span><span style="color:#880088">Some code that is going to get moved somewhere. Make it long so</span>
    <span style="color:#880088"><span style="font-weight:bold"><|</span></span><span style="color:#880088">things are really similar. We only match on at least 3 lines</span>
    <span style="color:#880088"><span style="font-weight:bold"><|</span></span><span style="color:#880088">so make it 3 lines long.</span>
      a
      b
      c
    <span style="color:#008800"><span style="font-weight:bold">+|</span></span><span style="color:#008800">Some code that is going to get moved somewhere. Make it long so</span>
    <span style="color:#008800"><span style="font-weight:bold">+|</span></span><span style="color:#008800">things are really differs. We only match on at least 3 lines</span>
    <span style="color:#008800"><span style="font-weight:bold">+|</span></span><span style="color:#008800">so make it 3 lines long.</span>
      d
      e
      f
    <span style="color:#880000"><span style="font-weight:bold">-|</span></span><span style="color:#880000">this is deleted</span>
    <span style="color:#008888"><span style="font-weight:bold">>|</span></span><span style="color:#008888">Some code that is going to get moved somewhere. Make it long so</span>
    <span style="color:#008888"><span style="font-weight:bold">>|</span></span><span style="color:#008888">things are really similar. We only match on at least 3 lines</span>
    <span style="color:#008888"><span style="font-weight:bold">>|</span></span><span style="color:#008888">so make it 3 lines long.</span>
    </pre>
    |}]
;;
