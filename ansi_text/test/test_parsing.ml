open! Core
open! Async
open Expect_test_helpers_core
open Ansi_text

let%expect_test "simple visualize" =
  print_endline (visualize "\027[31mfoo\027[0m");
  [%expect {| (fg:red)foo(off) |}];
  return ()
;;

let%expect_test "ESC[m is equivalent to ESC[0m (reset)" =
  print_endline (visualize "\027[mfoo\027[0m");
  [%expect {| (off)foo(off) |}];
  return ()
;;

let%expect_test "visualize includes unknown codes" =
  print_endline (visualize "\027[6nfoo\027[110m");
  [%expect {| (ANSI-CSI:6n)foo(ANSI-SGR:110) |}];
  return ()
;;

let%expect_test "simple minimize" =
  print_endline (minimize "\027[0;31mfoo");
  [%expect {| [0;31mfoo |}];
  return ()
;;

let%expect_test "minimize resets" =
  let s = "a \027[0m line of text \027[0;0;0m\027[0m" in
  print_endline (visualize s);
  print_endline (minimize s);
  print_endline (minimize s |> visualize);
  [%expect
    {|
    a (off) line of text (off off off)(off)
    a [0m line of text
    a (off) line of text
    |}];
  return ()
;;

let%expect_test "minimize with newlines" =
  let s =
    "a \027[0m line of text\n\027[0m\n\027[1;2mline after skip\n\027[0;0m\027[0m!\n"
  in
  print_endline (visualize s);
  print_endline (minimize s);
  print_endline (minimize s |> visualize);
  [%expect
    {|
    a (off) line of text
    (off)
    (+bold +faint)line after skip
    (off off)(off)!

    a [0m line of text

    [2mline after skip
    [0m!

    a (off) line of text

    (+faint)line after skip
    (off)!
    |}];
  return ()
;;

let%expect_test "strip out ANSI codes that are later overridden by a reset" =
  let s = "\027[0m\027[31m\027[0m\027[32m\027[42mf\027[44moo\027[0m" in
  print_endline (visualize s);
  print_endline (minimize s);
  print_endline (minimize s |> visualize);
  [%expect
    {|
    (off)(fg:red)(off)(fg:green)(bg:green)f(bg:blue)oo(off)
    [0;32;42mf[44moo[0m
    (off fg:green bg:green)f(bg:blue)oo(off)
    |}];
  return ()
;;

let%expect_test "combine and simplify adjacent ANSI codes" =
  print_endline (minimize "\027[0;44m\027[0;31m\027[1;32;44mfoo");
  [%expect {| [0;1;32;44mfoo |}];
  return ()
;;

let%expect_test "ignore a repeated style even with a reset" =
  let str = "  \027[0;41;30mabc\027[0;2mdef\027[A\027[A\027[0;1;2mghi\027[0m" in
  print_endline (visualize str);
  print_endline (minimize str);
  print_endline (minimize str |> visualize);
  [%expect
    {|
    (off bg:red fg:black)abc(off +faint)def(CursorUp)(CursorUp)(off +bold +faint)ghi(off)
    [0;41;30mabc[0;2mdef[A[Aghi[0m
    (off bg:red fg:black)abc(off +faint)def(CursorUp)(CursorUp)ghi(off)
    |}];
  return ()
;;

let%expect_test "strip out styles that are redundant with earlier codes" =
  print_endline (minimize "\027[0;31mfoo\027[31;42mbar\027[32;42mbaz\027[0;32;42m");
  [%expect {| [0;31mfoo[42mbar[32mbaz |}];
  return ()
;;

let%expect_test "strip all ANSI codes" =
  print_endline
    (strip "\027[0m\027[31m\027[0m\027[5D\027[32m\027[42mf\027[44mo\027[Bo\027[0m");
  [%expect {| foo |}];
  return ()
;;

let%expect_test "strip known & unknown CSI codes" =
  let s = "\027[0;31;123mfoo\027[31;42mbar\027[32;42Qbaz\027[0;32;42m\027[0;32~;42m" in
  print_endline (strip s);
  [%expect {| foobarbaz;42m |}];
  return ()
;;

let%expect_test "preserve malformed CSI sequences as text" =
  let s = "before\027[123\027[31mafter" in
  print_endline (visualize s);
  print_endline (strip s);
  [%expect
    {|
    before(ANSI-Fe:[)123(fg:red)after
    before123after
    |}];
  return ()
;;

let%expect_test "compress unknown codes" =
  print_endline (minimize "\027[11;12mfoo\027[10;51;52mbar\027[54;52m");
  [%expect {| [12mfoo[10;52mbar |}];
  return ()
;;

let%expect_test "all the attributes" =
  let s =
    "\027[1;2;3;4;5;7;8;9;21;53;31;41;53mfoo\027[22;23;24;25;27;28;29;55;39;49;59mbar\027[0m"
  in
  print_endline (visualize s);
  print_endline (minimize s);
  [%expect
    {|
    (+bold +faint +italic +uline +blink +invert +hide +strike +2uline +overline fg:red bg:red +overline)foo(-weight -italic -uline -blink -invert -hide -strike -overline fg:default bg:default ul:default)bar(off)
    [2;3;5;7;8;9;21;31;41;53mfoo[22;23;24;25;27;28;29;55;39;49;59mbar[0m
    |}];
  return ()
;;

let%expect_test "apply turns on and off but doesn't simplify" =
  let str = "some \027[2;48;5;1m\027[1K text" in
  let style = Style.of_sgr ~params:"1;2;3;4;38;5;250" in
  let styled = apply style str in
  print_endline styled;
  print_endline (minimize styled);
  [%expect
    {|
    [1;2;3;4;38;5;250msome [2;48;5;1m[1K text[22;23;24;39m
    [2;3;4;38;5;250msome [41m[1K text[22;23;24;39m
    |}];
  return ()
;;

let%expect_test "handle a patdiff header" =
  let s =
    "\027[1;94m@@@@@@@@\027[22;39m \027[1;45;37m--\027[22;49;39m \027[1mdiff of\027[0m \
     \027[31m/home/username/sbs/da_ob\027[39m \027[1m&\027[22m \
     \027[32m/home/us\027[0m│\027[1;94m@@@@@@@@\027[22;39m \027[1;46;37m++\027[22;49;39m \
     \027[1mdiff of\027[22m \027[31m/home/username/sbs/da_nb\027[39m \027[1m&\027[22m \
     \027[32m/home/us\027[0m"
  in
  let m = minimize s in
  print_endline (strip s);
  print_endline (strip m);
  print_endline (visualize s);
  print_endline (visualize m);
  print_endline s;
  print_endline m;
  [%expect
    {|
    @@@@@@@@ -- diff of /home/username/sbs/da_ob & /home/us│@@@@@@@@ ++ diff of /home/username/sbs/da_nb & /home/us
    @@@@@@@@ -- diff of /home/username/sbs/da_ob & /home/us│@@@@@@@@ ++ diff of /home/username/sbs/da_nb & /home/us
    (+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:magenta fg:white)--(-weight bg:default fg:default) (+bold)diff of(off) (fg:red)/home/username/sbs/da_ob(fg:default) (+bold)&(-weight) (fg:green)/home/us(off)│(+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:cyan fg:white)++(-weight bg:default fg:default) (+bold)diff of(-weight) (fg:red)/home/username/sbs/da_nb(fg:default) (+bold)&(-weight) (fg:green)/home/us(off)
    (+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:magenta fg:white)--(-weight bg:default fg:default) (+bold)diff of(off) (fg:red)/home/username/sbs/da_ob(fg:default) (+bold)&(-weight) (fg:green)/home/us(off)│(+bold fg:bright-blue)@@@@@@@@(-weight fg:default) (+bold bg:cyan fg:white)++(-weight bg:default fg:default) (+bold)diff of(-weight) (fg:red)/home/username/sbs/da_nb(fg:default) (+bold)&(-weight) (fg:green)/home/us(off)
    [1;94m@@@@@@@@[22;39m [1;45;37m--[22;49;39m [1mdiff of[0m [31m/home/username/sbs/da_ob[39m [1m&[22m [32m/home/us[0m│[1;94m@@@@@@@@[22;39m [1;46;37m++[22;49;39m [1mdiff of[22m [31m/home/username/sbs/da_nb[39m [1m&[22m [32m/home/us[0m
    [1;94m@@@@@@@@[22;39m [1;45;37m--[22;49;39m [1mdiff of[0m [31m/home/username/sbs/da_ob[39m [1m&[22m [32m/home/us[0m│[1;94m@@@@@@@@[22;39m [1;46;37m++[22;49;39m [1mdiff of[22m [31m/home/username/sbs/da_nb[39m [1m&[22m [32m/home/us[0m
    |}];
  return ()
;;

let%expect_test "center a patdiff header" =
  let header =
    parse " \027[1mdiff of\027[22m \027[31mold base\027[39m and \027[32mold tip\027[39m "
  in
  let style = Attr.[ Bold; Fg (Bright Blue) ] in
  let centered = center ~char:'@' ~style ~width:80 header in
  print_endline (to_string centered);
  print_endline (to_string_hum centered);
  [%expect
    {|
    [1;94m@@@@@@@@@@@@@@@@@@@@@@@@@[22;39m [1mdiff of[22m [31mold base[39m and [32mold tip[39m [1;94m@@@@@@@@@@@@@@@@@@@@@@@@@[22;39m
    (+bold fg:bright-blue)@@@@@@@@@@@@@@@@@@@@@@@@@(-weight fg:default) (+bold)diff of(-weight) (fg:red)old base(fg:default) and (fg:green)old tip(fg:default) (+bold fg:bright-blue)@@@@@@@@@@@@@@@@@@@@@@@@@(-weight fg:default)
    |}];
  return ()
;;

let%expect_test "a line that was wonky in a patdiff test" =
  let str =
    "    \027[0;41;30m-|\027[0;2m    let per_date_by_date,\027[0;2m error_by_date \
     =\027[0m"
  in
  print_endline (minimize str |> visualize);
  [%expect
    {| (off bg:red fg:black)-|(off +faint)    let per_date_by_date, error_by_date =(off) |}];
  return ()
;;

let%expect_test "another line that was wonky in a patdiff test" =
  let str = "    │2 \027[0;41;30m-|\027[0m\027[0;31m\027[0m\027[0;31m_\027[0m    " in
  print_endline (minimize str);
  print_endline (minimize str |> visualize);
  [%expect
    {|
    │2 [0;41;30m-|[0;31m_[0m
    │2 (off bg:red fg:black)-|(off fg:red)_(off)
    |}];
  return ()
;;

let%expect_test "split handles unicode" =
  let str = "\027[1;41m0123\027[2;32m4👋👋5\027[4;64m6789\027[0m" in
  let before, after = parse str |> split ~pos:7 in
  let before = to_string before in
  let after = to_string after in
  print_endline before;
  print_endline after;
  print_endline (visualize before);
  print_endline (visualize after);
  print_endline (strip before);
  print_endline (strip after);
  [%expect
    {|
    [1;41m0123[2;32m4👋[49;22;39m
    [41;2;32m👋5[4;64m6789[0m
    (+bold bg:red)0123(+faint fg:green)4👋(bg:default -weight fg:default)
    (bg:red +faint fg:green)👋5(+uline ANSI-SGR:64)6789(off)
    01234👋
    👋56789
    |}];
  return ()
;;

let%expect_test "OSC 8 hyperlinks" =
  let s = "\027]8;;https://example.com\027\\clickable text\027]8;;\027\\" in
  print_endline (visualize s);
  print_endline (minimize s);
  print_endline (strip s);
  [%expect
    {|
    (HREF:https://example.com)clickable text(/HREF)
    ]8;;https://example.com\clickable text]8;;\
    clickable text
    |}];
  return ()
;;

let%expect_test "OSC 8 hyperlink with bold text" =
  let s = "\027]8;;https://example.com\027\\\027[1mbold link\027[0m\027]8;;\027\\" in
  print_endline (visualize s);
  print_endline (minimize s);
  print_endline (strip s);
  [%expect
    {|
    (HREF:https://example.com)(+bold)bold link(off)(/HREF)
    ]8;;https://example.com\[1mbold link[0m]8;;\
    bold link
    |}];
  return ()
;;

let%expect_test "OSC 8 hyperlink with colored text" =
  let s =
    "\027]8;;https://example.com\027\\\027[31mred link\027[0m\027]8;;\027\\ and \
     \027]8;;https://other.com\027\\\027[42mgreen link\027[49m\027]8;;\027\\"
  in
  print_endline (visualize s);
  print_endline (minimize s);
  print_endline (strip s);
  [%expect
    {|
    (HREF:https://example.com)(fg:red)red link(off)(/HREF) and (HREF:https://other.com)(bg:green)green link(bg:default)(/HREF)
    ]8;;https://example.com\[31mred link[0m]8;;\ and ]8;;https://other.com\[42mgreen link[49m]8;;\
    red link and green link
    |}];
  return ()
;;

let%expect_test "parse |> to_string appropriately preserves input" =
  let round_trip s = print_endline (parse s |> to_string |> String.escaped) in
  round_trip "plain text";
  [%expect {| plain text |}];
  round_trip "\027[31mred\027[0m";
  [%expect {| \027[31mred\027[0m |}];
  round_trip "\027[1;38;5;196mbold bright red\027[0m";
  [%expect {| \027[1;38;5;196mbold bright red\027[0m |}];
  round_trip "\027]8;;https://example.com\027\\link\027]8;;\027\\";
  [%expect {| \027]8;;https://example.com\027\\link\027]8;;\027\\ |}];
  round_trip "\027[0;32~;42m";
  [%expect {| \027[0;32~;42m |}];
  round_trip "before\027[1mbold\027[0mafter";
  [%expect {| before\027[1mbold\027[0mafter |}];
  round_trip "\027[1m\027[31m\027[42m";
  [%expect {| \027[1m\027[31m\027[42m |}];
  round_trip "text\027";
  [%expect {| text\027 |}];
  round_trip "\027c";
  [%expect {| \027c |}];
  return ()
;;

let%expect_test "ESC handling: structure and roundtripping" =
  let test s =
    let parsed = parse s in
    let serialized = to_string parsed in
    let as_ocaml = serialized |> String.escaped in
    printf "input:      %s\n" (String.escaped s);
    printf "serialized: %s\n" as_ocaml;
    if not (String.equal s serialized) then printf "not equivalent!";
    print_endline (visualize s);
    print_newline ()
  in
  (* ESC at end of input *)
  test "text\027";
  [%expect
    {|
    input:      text\027
    serialized: text\027
    text(ESC)
    |}];
  (* ESC followed by ESC: each preserved separately *)
  test "\027\027";
  [%expect
    {|
    input:      \027\027
    serialized: \027\027
    (ESC)(ESC)
    |}];
  (* ESC-CSI-ESC: two incomplete ESCs surrounding valid CSI *)
  test "\027\027[31m\027";
  [%expect
    {|
    input:      \027\027[31m\027
    serialized: \027\027[31m\027
    (ESC)(fg:red)(ESC)
    |}];
  (* Simple Fe escape sequence *)
  test "\027c";
  [%expect
    {|
    input:      \027c
    serialized: \027c
    (ANSI-Fp:c)
    |}];
  (* Multiple Fe escapes followed by text *)
  test "\027some\027text";
  [%expect
    {|
    input:      \027some\027text
    serialized: \027some\027text
    (ANSI-Fp:s)ome(ANSI-Fp:t)ext
    |}];
  (* nF escape sequences roundtrip correctly. *)
  test "before\027 Fbetween\027(Bafter";
  [%expect
    {|
    input:      before\027 Fbetween\027(Bafter
    serialized: before\027 Fbetween\027(Bafter
    before(ANSI-nF: F)between(ANSI-nF:(B)after
    |}];
  return ()
;;

let%expect_test "CSI with non-alpha final bytes" =
  (* Final bytes 0x40-0x7E are all valid per the spec *)
  let test s =
    print_endline (visualize s);
    print_endline (parse s |> to_string |> String.escaped)
  in
  (* @ is 0x40, the start of the final byte range *)
  test "\027[1@";
  [%expect
    {|
    (ANSI-CSI:1@)
    \027[1@
    |}];
  (* ~ is 0x7E, the end of the final byte range *)
  test "\027[1~";
  [%expect
    {|
    (ANSI-CSI:1~)
    \027[1~
    |}];
  (* ` is 0x60 *)
  test "\027[5`";
  [%expect
    {|
    (ANSI-CSI:5`)
    \027[5`
    |}];
  return ()
;;

let%expect_test "CSI with out-of-range parameters becomes Unknown" =
  (* EraseDisplay only accepts 0-3, EraseLine only accepts 0-2 *)
  let test s =
    print_endline (visualize s);
    print_endline (parse s |> to_string |> String.escaped)
  in
  (* Valid EraseDisplay *)
  test "\027[2J";
  [%expect
    {|
    (EraseScreen)
    \027[2J
    |}];
  (* Invalid EraseDisplay parameter (5 is out of range) *)
  test "\027[5J";
  [%expect
    {|
    (ANSI-CSI:5J)
    \027[5J
    |}];
  (* Valid EraseLine *)
  test "\027[2K";
  [%expect
    {|
    (EraseLine)
    \027[2K
    |}];
  (* Invalid EraseLine parameter (5 is out of range) *)
  test "\027[5K";
  [%expect
    {|
    (ANSI-CSI:5K)
    \027[5K
    |}];
  return ()
;;

let%expect_test "SGR with out-of-range 256-color code" =
  (* 38;5;300 is syntactically valid but 300 is out of range for 256-color *)
  let s = "\027[38;5;300mtext\027[0m" in
  print_endline (visualize s);
  print_endline (parse s |> to_string |> String.escaped);
  [%expect
    {|
    (ANSI-SGR:38;5;300)text(off)
    \027[38;5;300mtext\027[0m
    |}];
  return ()
;;

let%expect_test "SGR with unknown color type" =
  (* 38;3;... is not a known color format (only 38;5;n and 38;2;r;g;b are) *)
  let s = "\027[38;3;1;2;3mtext\027[0m" in
  print_endline (visualize s);
  print_endline (parse s |> to_string |> String.escaped);
  [%expect
    {|
    (ANSI-SGR:38 +italic +bold +faint +italic)text(off)
    \027[38;3;1;2;3mtext\027[0m
    |}];
  return ()
;;

let%expect_test "multiple valid codes interleaved with unknown codes" =
  (* 1=bold, 110=unknown, 31=red fg, 38;5;300=invalid 256-color, 4=underline *)
  let s = "\027[1;110;31;38;5;300;4mtext\027[0m" in
  print_endline (visualize s);
  print_endline (parse s |> to_string |> String.escaped);
  [%expect
    {|
    (+bold ANSI-SGR:110 fg:red ANSI-SGR:38;5;300 +uline)text(off)
    \027[1;110;31;38;5;300;4mtext\027[0m
    |}];
  return ()
;;

let%expect_test "complex SGR: many RGB colors" =
  (* fg RGB, bg RGB, underline color RGB *)
  let s = "\027[38;2;255;0;0;48;2;0;255;0;58;2;0;0;255mtext\027[0m" in
  print_endline (visualize s);
  print_endline (parse s |> to_string |> String.escaped);
  [%expect
    {|
    (fg:rgb256-255-0-0 bg:rgb256-0-255-0 ul:rgb256-0-0-255)text(off)
    \027[38;2;255;0;0;48;2;0;255;0;58;2;0;0;255mtext\027[0m
    |}];
  return ()
;;

let%expect_test "unterminated OSC followed by CSI" =
  (* OSC without ST, immediately followed by a CSI sequence *)
  let s = "\027]8;;url\027[31mred\027[0m" in
  print_endline (visualize s);
  print_endline (parse s |> to_string |> String.escaped);
  [%expect
    {|
    (ANSI-Fe:])8;;url(fg:red)red(off)
    \027]8;;url\027[31mred\027[0m
    |}];
  return ()
;;

(* Quickcheck tests for parsing.

   Note: We do NOT guarantee exact byte-for-byte roundtripping for all inputs. Some escape
   sequences normalize during parsing: semantically equivalent sequences may serialize to
   a canonical form. Instead, we test that parsing is idempotent: once a string has been
   parsed and serialized, parsing it again produces the same result. *)

let%expect_test "quickcheck: parsing is idempotent (parse . to_string . parse = parse)" =
  let string_with_escapes =
    let open Quickcheck.Generator in
    let char_gen =
      weighted_union
        [ 1.0, return '\027' (* ESC *)
        ; 1.0, return '[' (* CSI introducer *)
        ; 1.0, return ']' (* OSC introducer *)
        ; 1.0, return '\\' (* ST terminator *)
        ; 1.0, return 'm' (* SGR terminator *)
        ; 0.5, return ';' (* parameter separator *)
        ; 3.0, Char.gen_digit
        ; 2.0, Char.gen_alpha
        ; 1.0, Char.gen_print
        ]
    in
    Int.gen_incl 0 50 >>= fun len -> String.gen_with_length len char_gen
  in
  Expect_test_helpers_core.quickcheck
    ~sexp_of:[%sexp_of: string]
    ~f:(fun s ->
      let once = parse s in
      let twice = parse (to_string once) in
      if not (Ansi_text.equal once twice)
      then
        raise_s
          [%message
            "Parsing not idempotent"
              (s : string)
              (once : Ansi_text.t)
              ~serialized:(to_string once : string)
              (twice : Ansi_text.t)])
    string_with_escapes;
  [%expect {| |}];
  return ()
;;

let%expect_test "quickcheck: to_string output always parses cleanly" =
  (* Any Text_with_ansi.t, when serialized, should parse without becoming malformed text
     (i.e., all escape sequences should be valid). *)
  let has_malformed_text t =
    List.exists t ~f:(function
      | `Text text ->
        let s = Ansi_text.Text.to_string text in
        String.is_substring s ~substring:"\027["
        || String.is_substring s ~substring:"\027]"
      | _ -> false)
  in
  Expect_test_helpers_core.quickcheck
    ~sexp_of:[%sexp_of: Ansi_text.t]
    ~f:(fun t ->
      let serialized = to_string t in
      let reparsed = parse serialized in
      if has_malformed_text reparsed
      then
        raise_s
          [%message
            "Serialization produced malformed escape sequences"
              (t : Ansi_text.t)
              (serialized : string)
              (reparsed : Ansi_text.t)])
    [%quickcheck.generator: Ansi_text.t];
  [%expect {| |}];
  return ()
;;

let%expect_test "quickcheck: parsing never puts ESC in text" =
  (* Every ESC (0x1B) in the input should be captured by an escape sequence type, never
     left as raw text. *)
  let has_esc_in_text t =
    List.exists t ~f:(function
      | `Text text ->
        let s = Ansi_text.Text.to_string text in
        String.exists s ~f:(Char.equal '\027')
      | _ -> false)
  in
  let string_with_escapes =
    let open Quickcheck.Generator in
    let char_gen =
      weighted_union
        [ 2.0, return '\027' (* ESC - higher weight to stress test *)
        ; 1.0, return '[' (* CSI introducer *)
        ; 1.0, return ']' (* OSC introducer *)
        ; 1.0, return '\\' (* ST terminator *)
        ; 1.0, return 'm' (* SGR terminator *)
        ; 0.5, return ';' (* parameter separator *)
        ; 2.0, Char.gen_digit
        ; 2.0, Char.gen_alpha
        ]
    in
    Int.gen_incl 0 50 >>= fun len -> String.gen_with_length len char_gen
  in
  Expect_test_helpers_core.quickcheck
    ~sexp_of:[%sexp_of: string]
    ~f:(fun s ->
      let parsed = parse s in
      if has_esc_in_text parsed
      then
        raise_s [%message "ESC found in text element" (s : string) (parsed : Ansi_text.t)])
    string_with_escapes;
  [%expect {| |}];
  return ()
;;
