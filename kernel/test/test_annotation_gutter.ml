open! Core
open! Import
open Patdiff_kernel
module Patdiff_core = Patdiff_core.Without_unix

let recording_gutter ~width =
  let render side =
    let tag, body =
      match (side : Format.Annotation_gutter.side) with
      | Prev { line } -> "P", Int.to_string line
      | Next { line } -> "N", Int.to_string line
      | Same { prev; next } ->
        ( "S"
        , if prev = next
          then Int.to_string prev
          else Int.to_string prev ^ "/" ^ Int.to_string next )
      | Unified { prev; next } ->
        ( "U"
        , if prev = next
          then Int.to_string prev
          else Int.to_string prev ^ "/" ^ Int.to_string next )
    in
    tag ^ body
  in
  Format.Annotation_gutter.create ~width ~render
;;

let strip_rules = Format.Rules.strip_styles Format.Rules.default
let to_array s = String.split_lines s |> Array.of_list

let%expect_test "build_unified threads side+line through annotation_gutter" =
  let prev = to_array "alpha\nbeta\ngamma\ndelta\n" in
  let next = to_array "alpha\nBETA\ngamma\nDELTA\nepsilon\n" in
  let hunks =
    Patdiff_core.diff
      ~context:1
      ~line_big_enough:3
      ~keep_ws:true
      ~find_moves:false
      ~prev
      ~next
  in
  Patdiff_core.build_unified
    ~annotation_gutter:(recording_gutter ~width:4)
    ~rules:strip_rules
    ~output:Ascii
    hunks
  |> List.iter ~f:(List.iter ~f:print_endline);
  [%expect
    {|
    S1    alpha
    P2  -|beta
    N2  +|BETA
    S3    gamma
    P4  -|delta
    N4  +|DELTA
    N5  +|epsilon
    |}]
;;

let%expect_test "build_unified emits `Same for context lines and `Prev/`Next for changes" =
  let prev = to_array "ctx1\nold\nctx2\n" in
  let next = to_array "ctx1\nnew\nctx2\n" in
  let hunks =
    Patdiff_core.diff
      ~context:1
      ~line_big_enough:3
      ~keep_ws:true
      ~find_moves:false
      ~prev
      ~next
  in
  Patdiff_core.build_unified
    ~annotation_gutter:(recording_gutter ~width:3)
    ~rules:strip_rules
    ~output:Ascii
    hunks
  |> List.iter ~f:(List.iter ~f:print_endline);
  [%expect
    {|
    S1   ctx1
    P2 -|old
    N2 +|new
    S3   ctx2
    |}]
;;

let%expect_test "build_unified across multiple hunks emits each hunk's lines \
                 independently"
  =
  (* Two changes far apart with -context 1 → two separate hunks; line numbers reset to
     each hunk's [prev_start] / [next_start]. *)
  let prev = to_array "a1\nA1\na2\na3\na4\na5\na6\na7\nA8\na9\n" in
  let next = to_array "a1\nB1\na2\na3\na4\na5\na6\na7\nB8\na9\n" in
  let hunks =
    Patdiff_core.diff
      ~context:1
      ~line_big_enough:3
      ~keep_ws:true
      ~find_moves:false
      ~prev
      ~next
  in
  Patdiff_core.build_unified
    ~annotation_gutter:(recording_gutter ~width:3)
    ~rules:strip_rules
    ~output:Ascii
    hunks
  |> List.iteri ~f:(fun i hunk_lines ->
    if i > 0 then print_endline "---";
    List.iter hunk_lines ~f:print_endline);
  [%expect
    {|
    S1   a1
    P2 -|A1
    N2 +|B1
    S3   a2
    ---
    S8   a7
    P9 -|A8
    N9 +|B8
    S10  a9
    |}]
;;

let%expect_test "build_unified Unified range (produce_unified_lines:true) emits with \
                 side:Unified"
  =
  let prev = to_array "Foo bar buzz" in
  let next = to_array "Foo buzz" in
  let hunks =
    Patdiff_core.diff
      ~context:0
      ~line_big_enough:3
      ~keep_ws:true
      ~find_moves:false
      ~prev
      ~next
    |> Patdiff_core.refine
         ~rules:Format.Rules.default
         ~produce_unified_lines:true
         ~output:Ansi
         ~keep_ws:true
         ~split_long_lines:false
         ~interleave:true
         ~word_big_enough:3
  in
  Patdiff_core.build_unified
    ~annotation_gutter:(recording_gutter ~width:3)
    ~rules:Format.Rules.default
    ~output:Ansi
    hunks
  |> List.iter
       ~f:
         (List.iter ~f:(fun line ->
            (* Strip ANSI escapes so the snapshot is readable. *)
            print_endline (Ansi_text.strip line)));
  [%expect {| U1 !|Foo bar buzz |}]
;;

let%expect_test "build_unified Unified line carries diverged prev/next line numbers" =
  (* A preceding [Prev]-only hunk advances prev_line ahead of next_line, so the next
     hunk's [Unified] line is rendered with prev <> next. *)
  let prev = to_array "extra_line\nctx\nFoo bar buzz" in
  let next = to_array "ctx\nFoo buzz" in
  let hunks =
    Patdiff_core.diff
      ~context:0
      ~line_big_enough:3
      ~keep_ws:true
      ~find_moves:false
      ~prev
      ~next
    |> Patdiff_core.refine
         ~rules:Format.Rules.default
         ~produce_unified_lines:true
         ~output:Ansi
         ~keep_ws:true
         ~split_long_lines:false
         ~interleave:true
         ~word_big_enough:3
  in
  Patdiff_core.build_unified
    ~annotation_gutter:(recording_gutter ~width:4)
    ~rules:Format.Rules.default
    ~output:Ansi
    hunks
  |> List.iter
       ~f:
         (List.iter ~f:(fun line ->
            (* Strip ANSI escapes so the snapshot is readable. *)
            print_endline (Ansi_text.strip line)));
  [%expect
    {|
    P1  -|extra_line
    U3/2!|Foo bar buzz
    |}]
;;

let%expect_test "build_side_by_side renders gutter on both panes and shrinks panes" =
  let prev = to_array "a\nb\nc\n" in
  let next = to_array "a\nB\nc\n" in
  let hunks =
    Patdiff_core.diff
      ~context:1
      ~line_big_enough:3
      ~keep_ws:true
      ~find_moves:false
      ~prev
      ~next
    |> Patdiff_core.unrefined_structured
  in
  let render_with ?annotation_gutter () =
    Patdiff_core.build_side_by_side
      ?annotation_gutter
      ~width_override:60
      ~rules:strip_rules
      ~wrap_or_truncate:`neither
      ~output:Ascii
      hunks
    |> List.concat_map ~f:(fun block ->
      List.map block ~f:(fun (l, r) -> [%string "%{l}|%{r}"]))
  in
  print_endline "without gutter:";
  List.iter (render_with ()) ~f:print_endline;
  print_endline "";
  print_endline "with recording_gutter ~width:3:";
  List.iter
    (render_with ~annotation_gutter:(recording_gutter ~width:3) ())
    ~f:print_endline;
  [%expect
    {|
    without gutter:
    1   a                                                       |1   a
    2 -|b                                                       |
                                                                |2 +|B
    3   c                                                       |3   c

    with recording_gutter ~width:3:
    S1 1   a                                                    |S1 1   a
    P2 2 -|b                                                    |
                                                                |N2 2 +|B
    S3 3   c                                                    |S3 3   c
    |}]
;;

let%expect_test "build_side_by_side wrapping: render is not re-called on continuation \
                 rows"
  =
  (* A long line that wraps to multiple display rows. The gutter [render] callback should
     fire once per source-line emit (not per wrap-row). *)
  let long = String.init 100 ~f:(fun _ -> 'a') in
  let prev = to_array (long ^ "\nshort") in
  let next = to_array "short" in
  let hunks =
    Patdiff_core.diff
      ~context:0
      ~line_big_enough:3
      ~keep_ws:true
      ~find_moves:false
      ~prev
      ~next
    |> Patdiff_core.unrefined_structured
  in
  let calls = ref [] in
  let counting_gutter =
    let render side =
      calls := side :: !calls;
      "G"
    in
    Format.Annotation_gutter.create ~width:3 ~render
  in
  let lines =
    Patdiff_core.build_side_by_side
      ~annotation_gutter:counting_gutter
      ~width_override:60
      ~rules:strip_rules
      ~wrap_or_truncate:`wrap
      ~output:Ascii
      hunks
  in
  let total_rows = List.sum (module Int) lines ~f:List.length in
  print_s
    [%message
      ""
        ~render_calls:(List.rev !calls : Format.Annotation_gutter.side list)
        ~total_rows:(total_rows : int)];
  [%expect {| ((render_calls ((Prev (line 1)))) (total_rows 2)) |}]
;;

let%expect_test "build_side_by_side wrapping: gutter renders on the first row only, \
                 continuation rows are blank-padded to keep the column aligned"
  =
  let long = String.init 60 ~f:(fun _ -> 'a') in
  let prev = to_array (long ^ "\nctx") in
  let next = to_array "ctx" in
  let hunks =
    Patdiff_core.diff
      ~context:1
      ~line_big_enough:3
      ~keep_ws:true
      ~find_moves:false
      ~prev
      ~next
    |> Patdiff_core.unrefined_structured
  in
  Patdiff_core.build_side_by_side
    ~annotation_gutter:(recording_gutter ~width:3)
    ~width_override:60
    ~rules:strip_rules
    ~wrap_or_truncate:`wrap
    ~output:Ascii
    hunks
  |> List.concat_map ~f:(fun block ->
    List.map block ~f:(fun (l, r) -> [%string "%{l}|%{r}"]))
  |> List.iter ~f:print_endline;
  [%expect
    {|
    P1 1 -|aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|
           aaaaaaa                                              |
    S2/2   ctx                                                  |S2/1   ctx
    |}]
;;

let%expect_test "build_side_by_side aligned-replace pair: gutter splits to Prev / Next \
                 per pane"
  =
  (* [Side_by_side.align_replace_lines] collapses a one-line [Replace] pair into a [Same]
     line-info with non-`Same segments. The gutter then renders [Prev] on the left pane
     and [Next] on the right (each with its pane's line number), rather than a single
     [Same] rendered on both panes. *)
  let prev = to_array "abc def ghi" in
  let next = to_array "abc XYZ ghi" in
  let hunks =
    Patdiff_core.diff
      ~context:0
      ~line_big_enough:3
      ~keep_ws:true
      ~find_moves:false
      ~prev
      ~next
    |> Patdiff_core.refine_structured
         ~produce_unified_lines:false
         ~keep_ws:true
         ~split_long_lines:false
         ~interleave:false
         ~word_big_enough:3
  in
  Patdiff_core.build_side_by_side
    ~annotation_gutter:(recording_gutter ~width:3)
    ~width_override:60
    ~rules:strip_rules
    ~wrap_or_truncate:`neither
    ~output:Ascii
    hunks
  |> List.concat_map ~f:(fun block ->
    List.map block ~f:(fun (l, r) -> [%string "%{l}|%{r}"]))
  |> List.iter ~f:print_endline;
  [%expect
    {| P1 1 !|abc def ghi                                          |N1 1 !|abc XYZ ghi |}]
;;

let%expect_test "public print_side_by_side forwards annotation_gutter through filename \
                 header and body"
  =
  let prev = to_array "alpha\nbeta\ngamma\n" in
  let next = to_array "alpha\nBETA\ngamma\n" in
  let hunks =
    Patdiff_core.diff
      ~context:1
      ~line_big_enough:3
      ~keep_ws:true
      ~find_moves:false
      ~prev
      ~next
    |> Patdiff_core.unrefined_structured
  in
  let prev_file = File_name.Fake "prev.txt" in
  let next_file = File_name.Fake "next.txt" in
  Patdiff_core.print_side_by_side
    ~annotation_gutter:(recording_gutter ~width:3)
    ~width_override:60
    ~file_names:(prev_file, next_file)
    ~rules:strip_rules
    ~wrap_or_truncate:`truncate
    ~output:Ascii
    hunks;
  [%expect
    {|
         -|prev.txt              |     +|next.txt
    S1 1   alpha                                                |S1 1   alpha
    P2 2 -|beta                                                 |
                                                                |N2 2 +|BETA
    S3 3   gamma                                                |S3 3   gamma
    |}]
;;

let%expect_test "public output_to_string_side_by_side forwards annotation_gutter" =
  let prev = to_array "alpha\nbeta\ngamma\n" in
  let next = to_array "alpha\nBETA\ngamma\n" in
  let hunks =
    Patdiff_core.diff
      ~context:1
      ~line_big_enough:3
      ~keep_ws:true
      ~find_moves:false
      ~prev
      ~next
    |> Patdiff_core.unrefined_structured
  in
  let prev_file = File_name.Fake "prev.txt" in
  let next_file = File_name.Fake "next.txt" in
  let s =
    Patdiff_core.output_to_string_side_by_side
      ~annotation_gutter:(recording_gutter ~width:3)
      ~width_override:60
      ~file_names:(prev_file, next_file)
      ~rules:strip_rules
      ~wrap_or_truncate:`truncate
      ~output:Ascii
      hunks
  in
  print_endline s;
  [%expect
    {|
         -|prev.txt              |     +|next.txt
    S1 1   alpha                                                |S1 1   alpha
    P2 2 -|beta                                                 |
                                                                |N2 2 +|BETA
    S3 3   gamma                                                |S3 3   gamma
    |}]
;;
