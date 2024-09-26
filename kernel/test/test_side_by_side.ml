open! Core
open! Import
open Patdiff_kernel

let test ~include_colors ~terminal_width ~prev ~next =
  let module Patdiff_core =
    Patdiff_core.Private.Make (struct
      let console_width () = Ok terminal_width

      let implementation : Output.t -> (module Output.S) = function
        | Ansi -> (module Ansi_output)
        | Ascii -> (module Ascii_output)
        | Html -> (module Html_output.Without_mtime)
      ;;
    end)
  in
  let prev = String.split_lines prev |> Array.of_list in
  let next = String.split_lines next |> Array.of_list in
  let hunks =
    Patdiff_core.diff
      ~context:(-1)
      ~line_big_enough:3
      ~keep_ws:false
      ~find_moves:true
      ~prev
      ~next
  in
  let refined =
    Patdiff_core.refine_structured
      ~produce_unified_lines:false
      ~keep_ws:false
      ~split_long_lines:false
      ~word_big_enough:3
      ~interleave:true
      hunks
  in
  let rules = Format.Rules.default in
  let print_output handle_colors =
    print_endline "Wrapped";
    print_endline
      (String.init ((terminal_width - 1) / 2) ~f:(fun _ -> '-')
       ^ "│"
       ^ String.init 40 ~f:(fun _ -> '-'));
    Patdiff_core.print_side_by_side
      ~file_names:(File_name.Fake "before", File_name.Fake "after")
      ~rules
      ~wrap_or_truncate:`wrap
      ~output:Output.Ansi
      refined;
    print_endline "Truncated";
    print_endline
      (String.init ((terminal_width - 1) / 2) ~f:(fun _ -> '-')
       ^ "│"
       ^ String.init 40 ~f:(fun _ -> '-'));
    Patdiff_core.print_side_by_side
      ~file_names:(File_name.Fake "before", File_name.Fake "after")
      ~rules
      ~wrap_or_truncate:`truncate
      ~output:Output.Ansi
      refined;
    handle_colors (Expect_test_helpers_base.expect_test_output ()) |> print_endline
  in
  print_output Ansicodes.strip;
  if include_colors then print_output Ansicodes.visualize
;;

let%expect_test _ =
  test
    ~include_colors:true
    ~terminal_width:80
    ~prev:
      {|
a
b
cats
dogs are the best pets to have
elephant
f
g
h
i
j
k
|}
    ~next:
      {|
a
b
cats
dogs are the cutest pets to have
elephant
f
g
h
i
j
k
|};
  [%expect
    {|
    Wrapped
    ---------------------------------------│----------------------------------------
       -|before                            │   +|after
     1                                     │ 1
     2   a                                 │ 2   a
     3   b                                 │ 3   b
     4   cats                              │ 4   cats
     5 !|dogs are the best pets to have    │ 5 !|dogs are the cutest pets to have
     6   elephant                          │ 6   elephant
     7   f                                 │ 7   f
     8   g                                 │ 8   g
     9   h                                 │ 9   h
    10   i                                 │10   i
    11   j                                 │11   j
    12   k                                 │12   k
    Truncated
    ---------------------------------------│----------------------------------------
       -|before                            │   +|after
     1                                     │ 1
     2   a                                 │ 2   a
     3   b                                 │ 3   b
     4   cats                              │ 4   cats
     5 !|dogs are the best pets to have    │ 5 !|dogs are the cutest pets to have
     6   elephant                          │ 6   elephant
     7   f                                 │ 7   f
     8   g                                 │ 8   g
     9   h                                 │ 9   h
    10   i                                 │10   i
    11   j                                 │11   j
    12   k                                 │12   k

    Wrapped
    ---------------------------------------│----------------------------------------
       (fg:red +bold)-|(off)(fg:red)(off)before                            │   (fg:green +bold)+|(off)(fg:green)(off)after
     1                                     │ 1
     2   a                                 │ 2   a
     3   b                                 │ 3   b
     4   cats                              │ 4   cats
     5 (fg:yellow +bold)!|(off)dogs are the(fg:red) best(off) pets to have    │ 5 (fg:yellow +bold)!|(off)dogs are the(fg:green) cutest(off) pets to have
     6   elephant                          │ 6   elephant
     7   f                                 │ 7   f
     8   g                                 │ 8   g
     9   h                                 │ 9   h
    10   i                                 │10   i
    11   j                                 │11   j
    12   k                                 │12   k
    Truncated
    ---------------------------------------│----------------------------------------
       (fg:red +bold)-|(off)(fg:red)(off)before                            │   (fg:green +bold)+|(off)(fg:green)(off)after
     1                                     │ 1
     2   a                                 │ 2   a
     3   b                                 │ 3   b
     4   cats                              │ 4   cats
     5 (fg:yellow +bold)!|(off)dogs are the(fg:red) best(off) pets to have    │ 5 (fg:yellow +bold)!|(off)dogs are the(fg:green) cutest(off) pets to have
     6   elephant                          │ 6   elephant
     7   f                                 │ 7   f
     8   g                                 │ 8   g
     9   h                                 │ 9   h
    10   i                                 │10   i
    11   j                                 │11   j
    12   k                                 │12   k
    |}]
;;

let%expect_test _ =
  test
    ~include_colors:true
    ~terminal_width:80
    ~prev:"These two files"
    ~next:"are completely different";
  [%expect
    {|
    Wrapped
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 -|These two files                    │
                                           │1 +|are completely different
    Truncated
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 -|These two files                    │
                                           │1 +|are completely different

    Wrapped
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:red +bold)-|(off)(fg:red)(off)(fg:red)These two files(off)                    │
                                           │1 (fg:green +bold)+|(off)(fg:green)(off)(fg:green)are completely different(off)
    Truncated
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:red +bold)-|(off)(fg:red)(off)(fg:red)These two files(off)                    │
                                           │1 (fg:green +bold)+|(off)(fg:green)(off)(fg:green)are completely different(off)
    |}]
;;

let%expect_test _ =
  test
    ~include_colors:true
    ~terminal_width:80
    ~prev:"Line with a small change"
    ~next:"Line with small change";
  [%expect
    {|
    Wrapped
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 !|Line with a small change           │1 !|Line with small change
    Truncated
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 !|Line with a small change           │1 !|Line with small change

    Wrapped
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)Line with(fg:red) a(off) small change           │1 (fg:yellow +bold)!|(off)Line with small change
    Truncated
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)Line with(fg:red) a(off) small change           │1 (fg:yellow +bold)!|(off)Line with small change
    |}];
  test
    ~include_colors:true
    ~terminal_width:60
    ~prev:"Line with a small change"
    ~next:"Line with small change";
  [%expect
    {|
    Wrapped
    -----------------------------│----------------------------------------
      -|before                   │  +|after
    1 !|Line with a small change │1 !|Line with small change
    Truncated
    -----------------------------│----------------------------------------
      -|before                   │  +|after
    1 !|Line with a small change │1 !|Line with small change

    Wrapped
    -----------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                   │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)Line with(fg:red) a(off) small change │1 (fg:yellow +bold)!|(off)Line with small change
    Truncated
    -----------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                   │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)Line with(fg:red) a(off) small change │1 (fg:yellow +bold)!|(off)Line with small change
    |}];
  test
    ~include_colors:true
    ~terminal_width:55
    ~prev:"Line with a small change"
    ~next:"Line with small change";
  [%expect
    {|
    Wrapped
    ---------------------------│----------------------------------------
      -|before                 │  +|after
    1 !|Line with a small chang│1 !|Line with small change
        e                      │
    Truncated
    ---------------------------│----------------------------------------
      -|before                 │  +|after
    1 !|Line with a small chang│1 !|Line with small change

    Wrapped
    ---------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                 │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)Line with(fg:red) a(off) small chang│1 (fg:yellow +bold)!|(off)Line with small change
        e                      │
    Truncated
    ---------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                 │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)Line with(fg:red) a(off) small chang│1 (fg:yellow +bold)!|(off)Line with small change
    |}];
  test
    ~include_colors:true
    ~terminal_width:50
    ~prev:"Line with a small change"
    ~next:"Line with small change";
  [%expect
    {|
    Wrapped
    ------------------------│----------------------------------------
      -|before              │  +|after
    1 !|Line with a small ch│1 !|Line with small chan
        ange                │    ge
    Truncated
    ------------------------│----------------------------------------
      -|before              │  +|after
    1 !|Line with a small ch│1 !|Line with small chan

    Wrapped
    ------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before              │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)Line with(fg:red) a(off) small ch│1 (fg:yellow +bold)!|(off)Line with small chan
        ange                │    ge
    Truncated
    ------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before              │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)Line with(fg:red) a(off) small ch│1 (fg:yellow +bold)!|(off)Line with small chan
    |}]
;;

let%expect_test _ =
  test
    ~include_colors:true
    ~terminal_width:80
    ~prev:
      {|
The quick brown fox
The rain in Spain
Veni vidi vici
|}
    ~next:
      {|
The quick brown fox
Veni vidi vici
|};
  [%expect
    {|
    Wrapped
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1                                      │1
    2   The quick brown fox                │2   The quick brown fox
    3 -|The rain in Spain                  │
    4   Veni vidi vici                     │3   Veni vidi vici
    Truncated
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1                                      │1
    2   The quick brown fox                │2   The quick brown fox
    3 -|The rain in Spain                  │
    4   Veni vidi vici                     │3   Veni vidi vici

    Wrapped
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1                                      │1
    2   The quick brown fox                │2   The quick brown fox
    3 (fg:red +bold)-|(off)(fg:red)(off)(fg:red)The rain in Spain(off)                  │
    4   Veni vidi vici                     │3   Veni vidi vici
    Truncated
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1                                      │1
    2   The quick brown fox                │2   The quick brown fox
    3 (fg:red +bold)-|(off)(fg:red)(off)(fg:red)The rain in Spain(off)                  │
    4   Veni vidi vici                     │3   Veni vidi vici
    |}]
;;

let%expect_test "test unicode" =
  test ~include_colors:true ~terminal_width:80 ~prev:"测试一二三" ~next:"测试一三";
  [%expect
    {|
    Wrapped
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 -|测试一二三                         │
                                           │1 +|测试一三
    Truncated
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 -|测试一二三                         │
                                           │1 +|测试一三

    Wrapped
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:red +bold)-|(off)(fg:red)(off)(fg:red)测试一二三(off)                         │
                                           │1 (fg:green +bold)+|(off)(fg:green)(off)(fg:green)测试一三(off)
    Truncated
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:red +bold)-|(off)(fg:red)(off)(fg:red)测试一二三(off)                         │
                                           │1 (fg:green +bold)+|(off)(fg:green)(off)(fg:green)测试一三(off)
    |}]
;;

let%expect_test "test move" =
  test
    ~include_colors:false
    ~terminal_width:80
    ~prev:
      {|
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce sit amet
malesuada leo. Vivamus vitae orci quis justo ornare molestie. Donec fringilla
tempus magna, ut semper lacus tincidunt at. Suspendisse et rutrum arcu. Aliquam
erat volutpat. Pellentesque pretium pellentesque elit, a consequat metus
placerat a. Praesent hendrerit euismod sem nec facilisis. Curabitur finibus ex
sagittis massa blandit, et dictum lectus lobortis. Sed fringilla fringilla
tortor vel finibus. Sed vel tortor pulvinar, fermentum quam non, blandit lorem.

Maecenas ac elit turpis. Nam ex turpis, ullamcorper et ultricies eu, pretium et
elit. Duis bibendum aliquet quam et tempor. Donec quis dapibus justo. Praesent
eget pellentesque nisi. Nulla vestibulum orci quis dui laoreet, eget posuere sem
interdum. Morbi ac sodales ligula. Proin arcu ipsum, venenatis id cursus et,
blandit eu mi. Sed iaculis egestas ligula, lacinia condimentum velit commodo
non. In eu elit convallis, tempus sapien sed, maximus purus. Sed vitae enim et
tellus accumsan bibendum eu vel turpis. Phasellus massa leo, eleifend vel
tincidunt ut, consequat et est. Duis quis condimentum ex. Etiam nec faucibus
lorem. Aliquam vehicula porta sapien, ut aliquam purus cursus vitae. Nullam at
ex vehicula, egestas sapien vitae, molestie ipsum.

Suspendisse iaculis lacinia arcu a vehicula. Nunc eleifend fermentum iaculis.
Duis dignissim, mi sit amet vehicula auctor, odio mauris consectetur lectus, ac
tincidunt lorem diam a nisi. Duis vehicula ex ac tortor sagittis, ac commodo
lectus venenatis. Nam efficitur justo eros, et ornare neque aliquet ut. Duis
vulputate nulla nunc, eget pellentesque diam aliquam at. Cras rhoncus orci at
tortor posuere convallis sed sed risus. Quisque sed ipsum ex.

Cras non semper ante. Vivamus non nulla scelerisque, fermentum sem at, laoreet
est. Sed convallis, magna sit amet maximus sollicitudin, nulla metus sodales
eros, eu molestie arcu urna ut nunc. Pellentesque habitant morbi tristique
senectus et netus et malesuada fames ac turpis egestas. Morbi sollicitudin,
turpis sit amet ultricies interdum, urna nisl rhoncus tellus, id consectetur
urna risus ut arcu. Aliquam hendrerit eros id ex tempor vehicula. Nunc a pretium
risus. Nulla tincidunt, mauris eu pellentesque hendrerit, nisi nibh volutpat
sapien, vitae vehicula lacus tellus dictum augue. Pellentesque malesuada vitae
tellus lobortis laoreet. Donec fringilla lacinia nulla sit amet eleifend.
Suspendisse iaculis metus sed massa bibendum, quis consequat metus lacinia.
Etiam scelerisque odio nec pulvinar dapibus. Duis interdum interdum quam vel
dapibus. Quisque dapibus nisl quis magna accumsan, et lobortis magna eleifend.
Ut venenatis cursus diam, vel dictum augue interdum vitae. Ut scelerisque
condimentum augue, eget bibendum augue lacinia in.

Aenean porta elit vitae pharetra dapibus. Duis a odio neque. Curabitur
ullamcorper enim ut metus luctus, eu blandit augue consectetur. Vestibulum
blandit lorem eget blandit fringilla. In et libero non lacus elementum pulvinar
id a orci. Maecenas porta urna mollis, egestas lacus id, feugiat nisi. Vivamus
imperdiet ornare dui eleifend semper. Integer erat ipsum, vestibulum a lobortis
eu, posuere in orci. Pellentesque gravida in purus eu ullamcorper. Nunc urna
tortor, hendrerit nec eleifend et, dapibus sed dolor.
|}
    ~next:
      {|
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce sit amet
malesuada leo. Vivamus vitae orci quis justo ornare molestie. Donec fringilla
tempus magna, ut semper lacus tincidunt at. Suspendisse et rutrum arcu. Aliquam
erat volutpat. Pellentesque pretium pellentesque elit, a consequat metus
placerat a. Praesent hendrerit euismod sem nec facilisis. Curabitur finibus ex
sagittis massa blandit, et dictum lectus lobortis. Sed fringilla fringilla
tortor vel finibus. Sed vel tortor pulvinar, fermentum quam non, blandit lorem.

Cras non semper ante. Vivamus non nulla scelerisque, fermentum sem at, laoreet
est. Sed convallis, magna sit amet maximus sollicitudin, nulla metus sodales
eros, eu molestie arcu urna ut nunc. Pellentesque habitant morbi tristique
senectus et netus et malesuada fames ac turpis egestas. Morbi sollicitudin,
turpis sit amet ultricies interdum, urna nisl rhoncus tellus, id consectetur
tellus lobortis laoreet. Donec fringilla lacinia nulla sit amet eleifend.
Suspendisse iaculis metus sed massa bibendum, quis consequat metus lacinia.
Etiam scelerisque odio nec pulvinar dapibus. Duis interdum interdum quam vel
dapibus. Quisque dapibus nisl quis magna accumsan, et lobortis magna eleifend.
Ut venenatis cursus diam, vel dictum augue interdum vitae. Ut scelerisque
condimentum augue, eget bibendum augue lacinia in.


Maecenas ac elit turpis. Nam ex turpis, ullamcorper et ultricies eu, pretium et
elit. Duis bibendum aliquet quam et tempor. Donec quis dapibus justo. Praesent
eget pellentesque nisi. Nulla vestibulum orci quis dui laoreet, eget posuere sem
interdum. Morbi ac sodales ligula. Proin arcu ipsum, venenatis id cursus et,
blandit eu mi. Sed iaculis egestas ligula, lacinia condimentum velit commodo
non. In eu elit convallis, tempus sapien sed, maximus purus. Sed vitae enim et
tellus accumsan bibendum eu vel turpis. Phasellus massa leo, eleifend vel
tincidunt ut, consequat et est. Duis quis condimentum ex. Etiam nec faucibus
lorem. Aliquam vehicula porta sapien, ut aliquam purus cursus vitae. Nullam at
ex vehicula, egestas sapien vitae, molestie ipsum.

Suspendisse iaculis lacinia arcu a vehicula. Nunc eleifend fermentum iaculis.
Duis dignissim, mi sit amet vehicula auctor, odio mauris consectetur lectus, ac
tincidunt lorem diam a nisi. Duis vehicula ex ac tortor sagittis, ac commodo
lectus venenatis. Nam efficitur justo eros, et ornare neque aliquet ut. Duis
vulputate nulla nunc, eget pellentesque diam aliquam at. Cras rhoncus orci at
tortor posuere convallis sed sed risus. Quisque sed ipsum ex.

Aenean porta elit vitae pharetra dapibus. Duis a odio neque. Curabitur
ullamcorper enim ut metus luctus, eu blandit augue consectetur. Vestibulum
blandit lorem eget blandit fringilla. In et libero non lacus elementum pulvinar
id a orci. Maecenas porta urna mollis, egestas lacus id, feugiat nisi. Vivamus
imperdiet ornare dui eleifend semper. Integer erat ipsum, vestibulum a lobortis
eu, posuere in orci. Pellentesque gravida in purus eu ullamcorper. Nunc urna
tortor, hendrerit nec eleifend et, dapibus sed dolor.
|};
  [%expect
    {|
    Wrapped
    ---------------------------------------│----------------------------------------
       -|before                            │   +|after
     1                                     │ 1
     2   Lorem ipsum dolor sit amet, consec│ 2   Lorem ipsum dolor sit amet, consec
         tetur adipiscing elit. Fusce sit a│     tetur adipiscing elit. Fusce sit a
         met                               │     met
     3   malesuada leo. Vivamus vitae orci │ 3   malesuada leo. Vivamus vitae orci
         quis justo ornare molestie. Donec │     quis justo ornare molestie. Donec
         fringilla                         │     fringilla
     4   tempus magna, ut semper lacus tinc│ 4   tempus magna, ut semper lacus tinc
         idunt at. Suspendisse et rutrum ar│     idunt at. Suspendisse et rutrum ar
         cu. Aliquam                       │     cu. Aliquam
     5   erat volutpat. Pellentesque pretiu│ 5   erat volutpat. Pellentesque pretiu
         m pellentesque elit, a consequat m│     m pellentesque elit, a consequat m
         etus                              │     etus
     6   placerat a. Praesent hendrerit eui│ 6   placerat a. Praesent hendrerit eui
         smod sem nec facilisis. Curabitur │     smod sem nec facilisis. Curabitur
         finibus ex                        │     finibus ex
     7   sagittis massa blandit, et dictum │ 7   sagittis massa blandit, et dictum
         lectus lobortis. Sed fringilla fri│     lectus lobortis. Sed fringilla fri
         ngilla                            │     ngilla
     8   tortor vel finibus. Sed vel tortor│ 8   tortor vel finibus. Sed vel tortor
          pulvinar, fermentum quam non, bla│      pulvinar, fermentum quam non, bla
         ndit lorem.                       │     ndit lorem.
                                           │ 9 >|
                                           │10 >|Cras non semper ante. Vivamus non
                                           │     nulla scelerisque, fermentum sem a
                                           │     t, laoreet
                                           │11 >|est. Sed convallis, magna sit amet
                                           │      maximus sollicitudin, nulla metus
                                           │      sodales
                                           │12 >|eros, eu molestie arcu urna ut nun
                                           │     c. Pellentesque habitant morbi tri
                                           │     stique
                                           │13 >|senectus et netus et malesuada fam
                                           │     es ac turpis egestas. Morbi sollic
                                           │     itudin,
                                           │14 >|turpis sit amet ultricies interdum
                                           │     , urna nisl rhoncus tellus, id con
                                           │     sectetur
                                           │15 >|tellus lobortis laoreet. Donec fri
                                           │     ngilla lacinia nulla sit amet elei
                                           │     fend.
                                           │16 >|Suspendisse iaculis metus sed mass
                                           │     a bibendum, quis consequat metus l
                                           │     acinia.
                                           │17 >|Etiam scelerisque odio nec pulvina
                                           │     r dapibus. Duis interdum interdum
                                           │     quam vel
                                           │18 >|dapibus. Quisque dapibus nisl quis
                                           │      magna accumsan, et lobortis magna
                                           │      eleifend.
                                           │19 >|Ut venenatis cursus diam, vel dict
                                           │     um augue interdum vitae. Ut sceler
                                           │     isque
                                           │20 >|condimentum augue, eget bibendum a
                                           │     ugue lacinia in.
     9                                     │21
    10                                     │22
    11   Maecenas ac elit turpis. Nam ex tu│23   Maecenas ac elit turpis. Nam ex tu
         rpis, ullamcorper et ultricies eu,│     rpis, ullamcorper et ultricies eu,
          pretium et                       │      pretium et
    12   elit. Duis bibendum aliquet quam e│24   elit. Duis bibendum aliquet quam e
         t tempor. Donec quis dapibus justo│     t tempor. Donec quis dapibus justo
         . Praesent                        │     . Praesent
    13   eget pellentesque nisi. Nulla vest│25   eget pellentesque nisi. Nulla vest
         ibulum orci quis dui laoreet, eget│     ibulum orci quis dui laoreet, eget
          posuere sem                      │      posuere sem
    14   interdum. Morbi ac sodales ligula.│26   interdum. Morbi ac sodales ligula.
          Proin arcu ipsum, venenatis id cu│      Proin arcu ipsum, venenatis id cu
         rsus et,                          │     rsus et,
    15   blandit eu mi. Sed iaculis egestas│27   blandit eu mi. Sed iaculis egestas
          ligula, lacinia condimentum velit│      ligula, lacinia condimentum velit
          commodo                          │      commodo
    16   non. In eu elit convallis, tempus │28   non. In eu elit convallis, tempus
         sapien sed, maximus purus. Sed vit│     sapien sed, maximus purus. Sed vit
         ae enim et                        │     ae enim et
    17   tellus accumsan bibendum eu vel tu│29   tellus accumsan bibendum eu vel tu
         rpis. Phasellus massa leo, eleifen│     rpis. Phasellus massa leo, eleifen
         d vel                             │     d vel
    18   tincidunt ut, consequat et est. Du│30   tincidunt ut, consequat et est. Du
         is quis condimentum ex. Etiam nec │     is quis condimentum ex. Etiam nec
         faucibus                          │     faucibus
    19   lorem. Aliquam vehicula porta sapi│31   lorem. Aliquam vehicula porta sapi
         en, ut aliquam purus cursus vitae.│     en, ut aliquam purus cursus vitae.
          Nullam at                        │      Nullam at
    20   ex vehicula, egestas sapien vitae,│32   ex vehicula, egestas sapien vitae,
          molestie ipsum.                  │      molestie ipsum.
    21                                     │33
    22   Suspendisse iaculis lacinia arcu a│34   Suspendisse iaculis lacinia arcu a
          vehicula. Nunc eleifend fermentum│      vehicula. Nunc eleifend fermentum
          iaculis.                         │      iaculis.
    23   Duis dignissim, mi sit amet vehicu│35   Duis dignissim, mi sit amet vehicu
         la auctor, odio mauris consectetur│     la auctor, odio mauris consectetur
          lectus, ac                       │      lectus, ac
    24   tincidunt lorem diam a nisi. Duis │36   tincidunt lorem diam a nisi. Duis
         vehicula ex ac tortor sagittis, ac│     vehicula ex ac tortor sagittis, ac
          commodo                          │      commodo
    25   lectus venenatis. Nam efficitur ju│37   lectus venenatis. Nam efficitur ju
         sto eros, et ornare neque aliquet │     sto eros, et ornare neque aliquet
         ut. Duis                          │     ut. Duis
    26   vulputate nulla nunc, eget pellent│38   vulputate nulla nunc, eget pellent
         esque diam aliquam at. Cras rhoncu│     esque diam aliquam at. Cras rhoncu
         s orci at                         │     s orci at
    27   tortor posuere convallis sed sed r│39   tortor posuere convallis sed sed r
         isus. Quisque sed ipsum ex.       │     isus. Quisque sed ipsum ex.
    28 <|                                  │
    29 <|Cras non semper ante. Vivamus non │
         nulla scelerisque, fermentum sem a│
         t, laoreet                        │
    30 <|est. Sed convallis, magna sit amet│
          maximus sollicitudin, nulla metus│
          sodales                          │
    31 <|eros, eu molestie arcu urna ut nun│
         c. Pellentesque habitant morbi tri│
         stique                            │
    32 <|senectus et netus et malesuada fam│
         es ac turpis egestas. Morbi sollic│
         itudin,                           │
    33 <|turpis sit amet ultricies interdum│
         , urna nisl rhoncus tellus, id con│
         sectetur                          │
    34 <|urna risus ut arcu. Aliquam hendre│
         rit eros id ex tempor vehicula. Nu│
         nc a pretium                      │
    35 <|risus. Nulla tincidunt, mauris eu │
         pellentesque hendrerit, nisi nibh │
         volutpat                          │
    36 <|sapien, vitae vehicula lacus tellu│
         s dictum augue. Pellentesque males│
         uada vitae                        │
    37 <|tellus lobortis laoreet. Donec fri│
         ngilla lacinia nulla sit amet elei│
         fend.                             │
    38 <|Suspendisse iaculis metus sed mass│
         a bibendum, quis consequat metus l│
         acinia.                           │
    39 <|Etiam scelerisque odio nec pulvina│
         r dapibus. Duis interdum interdum │
         quam vel                          │
    40 <|dapibus. Quisque dapibus nisl quis│
          magna accumsan, et lobortis magna│
          eleifend.                        │
    41 <|Ut venenatis cursus diam, vel dict│
         um augue interdum vitae. Ut sceler│
         isque                             │
    42 <|condimentum augue, eget bibendum a│
         ugue lacinia in.                  │
    43                                     │40
    44   Aenean porta elit vitae pharetra d│41   Aenean porta elit vitae pharetra d
         apibus. Duis a odio neque. Curabit│     apibus. Duis a odio neque. Curabit
         ur                                │     ur
    45   ullamcorper enim ut metus luctus, │42   ullamcorper enim ut metus luctus,
         eu blandit augue consectetur. Vest│     eu blandit augue consectetur. Vest
         ibulum                            │     ibulum
    46   blandit lorem eget blandit fringil│43   blandit lorem eget blandit fringil
         la. In et libero non lacus element│     la. In et libero non lacus element
         um pulvinar                       │     um pulvinar
    47   id a orci. Maecenas porta urna mol│44   id a orci. Maecenas porta urna mol
         lis, egestas lacus id, feugiat nis│     lis, egestas lacus id, feugiat nis
         i. Vivamus                        │     i. Vivamus
    48   imperdiet ornare dui eleifend semp│45   imperdiet ornare dui eleifend semp
         er. Integer erat ipsum, vestibulum│     er. Integer erat ipsum, vestibulum
          a lobortis                       │      a lobortis
    49   eu, posuere in orci. Pellentesque │46   eu, posuere in orci. Pellentesque
         gravida in purus eu ullamcorper. N│     gravida in purus eu ullamcorper. N
         unc urna                          │     unc urna
    50   tortor, hendrerit nec eleifend et,│47   tortor, hendrerit nec eleifend et,
          dapibus sed dolor.               │      dapibus sed dolor.
    Truncated
    ---------------------------------------│----------------------------------------
       -|before                            │   +|after
     1                                     │ 1
     2   Lorem ipsum dolor sit amet, consec│ 2   Lorem ipsum dolor sit amet, consec
     3   malesuada leo. Vivamus vitae orci │ 3   malesuada leo. Vivamus vitae orci
     4   tempus magna, ut semper lacus tinc│ 4   tempus magna, ut semper lacus tinc
     5   erat volutpat. Pellentesque pretiu│ 5   erat volutpat. Pellentesque pretiu
     6   placerat a. Praesent hendrerit eui│ 6   placerat a. Praesent hendrerit eui
     7   sagittis massa blandit, et dictum │ 7   sagittis massa blandit, et dictum
     8   tortor vel finibus. Sed vel tortor│ 8   tortor vel finibus. Sed vel tortor
                                           │ 9 >|
                                           │10 >|Cras non semper ante. Vivamus non
                                           │11 >|est. Sed convallis, magna sit amet
                                           │12 >|eros, eu molestie arcu urna ut nun
                                           │13 >|senectus et netus et malesuada fam
                                           │14 >|turpis sit amet ultricies interdum
                                           │15 >|tellus lobortis laoreet. Donec fri
                                           │16 >|Suspendisse iaculis metus sed mass
                                           │17 >|Etiam scelerisque odio nec pulvina
                                           │18 >|dapibus. Quisque dapibus nisl quis
                                           │19 >|Ut venenatis cursus diam, vel dict
                                           │20 >|condimentum augue, eget bibendum a
     9                                     │21
    10                                     │22
    11   Maecenas ac elit turpis. Nam ex tu│23   Maecenas ac elit turpis. Nam ex tu
    12   elit. Duis bibendum aliquet quam e│24   elit. Duis bibendum aliquet quam e
    13   eget pellentesque nisi. Nulla vest│25   eget pellentesque nisi. Nulla vest
    14   interdum. Morbi ac sodales ligula.│26   interdum. Morbi ac sodales ligula.
    15   blandit eu mi. Sed iaculis egestas│27   blandit eu mi. Sed iaculis egestas
    16   non. In eu elit convallis, tempus │28   non. In eu elit convallis, tempus
    17   tellus accumsan bibendum eu vel tu│29   tellus accumsan bibendum eu vel tu
    18   tincidunt ut, consequat et est. Du│30   tincidunt ut, consequat et est. Du
    19   lorem. Aliquam vehicula porta sapi│31   lorem. Aliquam vehicula porta sapi
    20   ex vehicula, egestas sapien vitae,│32   ex vehicula, egestas sapien vitae,
    21                                     │33
    22   Suspendisse iaculis lacinia arcu a│34   Suspendisse iaculis lacinia arcu a
    23   Duis dignissim, mi sit amet vehicu│35   Duis dignissim, mi sit amet vehicu
    24   tincidunt lorem diam a nisi. Duis │36   tincidunt lorem diam a nisi. Duis
    25   lectus venenatis. Nam efficitur ju│37   lectus venenatis. Nam efficitur ju
    26   vulputate nulla nunc, eget pellent│38   vulputate nulla nunc, eget pellent
    27   tortor posuere convallis sed sed r│39   tortor posuere convallis sed sed r
    28 <|                                  │
    29 <|Cras non semper ante. Vivamus non │
    30 <|est. Sed convallis, magna sit amet│
    31 <|eros, eu molestie arcu urna ut nun│
    32 <|senectus et netus et malesuada fam│
    33 <|turpis sit amet ultricies interdum│
    34 <|urna risus ut arcu. Aliquam hendre│
    35 <|risus. Nulla tincidunt, mauris eu │
    36 <|sapien, vitae vehicula lacus tellu│
    37 <|tellus lobortis laoreet. Donec fri│
    38 <|Suspendisse iaculis metus sed mass│
    39 <|Etiam scelerisque odio nec pulvina│
    40 <|dapibus. Quisque dapibus nisl quis│
    41 <|Ut venenatis cursus diam, vel dict│
    42 <|condimentum augue, eget bibendum a│
    43                                     │40
    44   Aenean porta elit vitae pharetra d│41   Aenean porta elit vitae pharetra d
    45   ullamcorper enim ut metus luctus, │42   ullamcorper enim ut metus luctus,
    46   blandit lorem eget blandit fringil│43   blandit lorem eget blandit fringil
    47   id a orci. Maecenas porta urna mol│44   id a orci. Maecenas porta urna mol
    48   imperdiet ornare dui eleifend semp│45   imperdiet ornare dui eleifend semp
    49   eu, posuere in orci. Pellentesque │46   eu, posuere in orci. Pellentesque
    50   tortor, hendrerit nec eleifend et,│47   tortor, hendrerit nec eleifend et,
    |}]
;;

let%expect_test "test first word deleted" =
  let prev = {|bananas are good|} in
  let next = {|are good|} in
  test ~include_colors:true ~terminal_width:80 ~prev ~next;
  [%expect
    {|
    Wrapped
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 !|bananas are good                   │1 !|are good
    Truncated
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 !|bananas are good                   │1 !|are good

    Wrapped
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)(fg:red)bananas(off) are good                   │1 (fg:yellow +bold)!|(off)are good
    Truncated
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)(fg:red)bananas(off) are good                   │1 (fg:yellow +bold)!|(off)are good
    |}];
  test ~include_colors:true ~terminal_width:16 ~prev ~next;
  [%expect
    {|
    Wrapped
    -------│----------------------------------------
      -|bef│  +|aft
    1 !|ban│1 !|are
        ana│     go
        s a│    od
        re │
        goo│
        d  │
    Truncated
    -------│----------------------------------------
      -|bef│  +|aft
    1 !|ban│1 !|are

    Wrapped
    -------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)bef│  (fg:green +bold)+|(off)(fg:green)(off)aft
    1 (fg:yellow +bold)!|(off)(fg:red)ban(off)│1 (fg:yellow +bold)!|(off)are
        (fg:red)ana(off)│     go
        (fg:red)s(off) a│    od
        re │
        goo│
        d  │
    Truncated
    -------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)bef│  (fg:green +bold)+|(off)(fg:green)(off)aft
    1 (fg:yellow +bold)!|(off)(fg:red)ban(off)│1 (fg:yellow +bold)!|(off)are
    |}]
;;

let%expect_test "test first word added" =
  let prev = {|are good|} in
  let next = {|bananas are good|} in
  test ~include_colors:true ~terminal_width:80 ~prev ~next;
  [%expect
    {|
    Wrapped
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 !|are good                           │1 !|bananas are good
    Truncated
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 !|are good                           │1 !|bananas are good

    Wrapped
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)are good                           │1 (fg:yellow +bold)!|(off)(fg:green)bananas(off) are good
    Truncated
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)are good                           │1 (fg:yellow +bold)!|(off)(fg:green)bananas(off) are good
    |}];
  test ~include_colors:true ~terminal_width:16 ~prev ~next;
  [%expect
    {|
    Wrapped
    -------│----------------------------------------
      -|bef│  +|aft
    1 !|are│1 !|ban
         go│    ana
        od │    s a
           │    re
           │    goo
           │    d
    Truncated
    -------│----------------------------------------
      -|bef│  +|aft
    1 !|are│1 !|ban

    Wrapped
    -------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)bef│  (fg:green +bold)+|(off)(fg:green)(off)aft
    1 (fg:yellow +bold)!|(off)are│1 (fg:yellow +bold)!|(off)(fg:green)ban(off)
         go│    (fg:green)ana(off)
        od │    (fg:green)s(off) a
           │    re
           │    goo
           │    d
    Truncated
    -------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)bef│  (fg:green +bold)+|(off)(fg:green)(off)aft
    1 (fg:yellow +bold)!|(off)are│1 (fg:yellow +bold)!|(off)(fg:green)ban(off)
    |}]
;;

let%expect_test "test word deleted in middle" =
  let prev = {|bananas are good|} in
  let next = {|bananas good|} in
  test ~include_colors:true ~terminal_width:80 ~prev ~next;
  [%expect
    {|
    Wrapped
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 !|bananas are good                   │1 !|bananas good
    Truncated
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 !|bananas are good                   │1 !|bananas good

    Wrapped
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)bananas(fg:red) are(off) good                   │1 (fg:yellow +bold)!|(off)bananas good
    Truncated
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:yellow +bold)!|(off)bananas(fg:red) are(off) good                   │1 (fg:yellow +bold)!|(off)bananas good
    |}];
  test ~include_colors:true ~terminal_width:16 ~prev ~next;
  [%expect
    {|
    Wrapped
    -------│----------------------------------------
      -|bef│  +|aft
    1 !|ban│1 !|ban
        ana│    ana
        s a│    s g
        re │    ood
        goo│
        d  │
    Truncated
    -------│----------------------------------------
      -|bef│  +|aft
    1 !|ban│1 !|ban

    Wrapped
    -------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)bef│  (fg:green +bold)+|(off)(fg:green)(off)aft
    1 (fg:yellow +bold)!|(off)ban│1 (fg:yellow +bold)!|(off)ban
        ana│    ana
        s(fg:red) a(off)│    s g
        (fg:red)re(off) │    ood
        goo│
        d  │
    Truncated
    -------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)bef│  (fg:green +bold)+|(off)(fg:green)(off)aft
    1 (fg:yellow +bold)!|(off)ban│1 (fg:yellow +bold)!|(off)ban
    |}]
;;

let%expect_test "test whole line deleted" =
  let prev =
    {|12345
678910
|}
  in
  let next = {|678910|} in
  test ~include_colors:true ~terminal_width:80 ~prev ~next;
  [%expect
    {|
    Wrapped
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 -|12345                              │
    2   678910                             │1   678910
    Truncated
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
    1 -|12345                              │
    2   678910                             │1   678910

    Wrapped
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:red +bold)-|(off)(fg:red)(off)(fg:red)12345(off)                              │
    2   678910                             │1   678910
    Truncated
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
    1 (fg:red +bold)-|(off)(fg:red)(off)(fg:red)12345(off)                              │
    2   678910                             │1   678910
    |}];
  test ~include_colors:true ~terminal_width:16 ~prev ~next;
  [%expect
    {|
    Wrapped
    -------│----------------------------------------
      -|bef│  +|aft
    1 -|123│
        45 │
    2   678│1   678
        910│    910
    Truncated
    -------│----------------------------------------
      -|bef│  +|aft
    1 -|123│
    2   678│1   678

    Wrapped
    -------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)bef│  (fg:green +bold)+|(off)(fg:green)(off)aft
    1 (fg:red +bold)-|(off)(fg:red)(off)(fg:red)123(off)│
        (fg:red)45(off) │
    2   678│1   678
        910│    910
    Truncated
    -------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)bef│  (fg:green +bold)+|(off)(fg:green)(off)aft
    1 (fg:red +bold)-|(off)(fg:red)(off)(fg:red)123(off)│
    2   678│1   678
    |}]
;;

let%expect_test "test whole line added" =
  let prev = {|678910|} in
  let next =
    {|12345
678910
|}
  in
  test ~include_colors:true ~terminal_width:80 ~prev ~next;
  [%expect
    {|
    Wrapped
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
                                           │1 +|12345
    1   678910                             │2   678910
    Truncated
    ---------------------------------------│----------------------------------------
      -|before                             │  +|after
                                           │1 +|12345
    1   678910                             │2   678910

    Wrapped
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
                                           │1 (fg:green +bold)+|(off)(fg:green)(off)(fg:green)12345(off)
    1   678910                             │2   678910
    Truncated
    ---------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                             │  (fg:green +bold)+|(off)(fg:green)(off)after
                                           │1 (fg:green +bold)+|(off)(fg:green)(off)(fg:green)12345(off)
    1   678910                             │2   678910
    |}];
  test ~include_colors:true ~terminal_width:16 ~prev ~next;
  [%expect
    {|
    Wrapped
    -------│----------------------------------------
      -|bef│  +|aft
           │1 +|123
           │    45
    1   678│2   678
        910│    910
    Truncated
    -------│----------------------------------------
      -|bef│  +|aft
           │1 +|123
    1   678│2   678

    Wrapped
    -------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)bef│  (fg:green +bold)+|(off)(fg:green)(off)aft
           │1 (fg:green +bold)+|(off)(fg:green)(off)(fg:green)123(off)
           │    (fg:green)45(off)
    1   678│2   678
        910│    910
    Truncated
    -------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)bef│  (fg:green +bold)+|(off)(fg:green)(off)aft
           │1 (fg:green +bold)+|(off)(fg:green)(off)(fg:green)123(off)
    1   678│2   678
    |}]
;;

let%expect_test "Make sure first word of replace block is formatted correctly" =
  let prev =
    {|
ABC
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
21 22 23 24 25
MNO
|}
  in
  let next =
    {|
ABC
1 2 3 4 5 6 7 8
new line
new 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
MNO
|}
  in
  test ~include_colors:true ~terminal_width:140 ~prev ~next;
  [%expect
    {|
    Wrapped
    ---------------------------------------------------------------------│----------------------------------------
      -|before                                                           │  +|after
    1                                                                    │1
    2   ABC                                                              │2   ABC
    3 !|1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20               │3 !|1 2 3 4 5 6 7 8
                                                                         │4 +|new line
    4 !|21 22 23 24 25                                                   │5 !|new 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    5   MNO                                                              │6   MNO
    Truncated
    ---------------------------------------------------------------------│----------------------------------------
      -|before                                                           │  +|after
    1                                                                    │1
    2   ABC                                                              │2   ABC
    3 !|1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20               │3 !|1 2 3 4 5 6 7 8
                                                                         │4 +|new line
    4 !|21 22 23 24 25                                                   │5 !|new 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    5   MNO                                                              │6   MNO

    Wrapped
    ---------------------------------------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                                                           │  (fg:green +bold)+|(off)(fg:green)(off)after
    1                                                                    │1
    2   ABC                                                              │2   ABC
    3 (fg:yellow +bold)!|(off)1 2 3 4 5 6 7 8(fg:red) 9(off) 10 11 12 13 14 15 16 17 18 19 20(fg:red)(off)               │3 (fg:yellow +bold)!|(off)1 2 3 4 5 6 7 8(fg:green)(off)
                                                                         │4 (fg:green +bold)+|(off)(fg:green)(off)(fg:green)new line(off)
    4 (fg:yellow +bold)!|(off)(fg:red)(off)21 22 23 24 25                                                   │5 (fg:yellow +bold)!|(off)(fg:green)new(off) 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    5   MNO                                                              │6   MNO
    Truncated
    ---------------------------------------------------------------------│----------------------------------------
      (fg:red +bold)-|(off)(fg:red)(off)before                                                           │  (fg:green +bold)+|(off)(fg:green)(off)after
    1                                                                    │1
    2   ABC                                                              │2   ABC
    3 (fg:yellow +bold)!|(off)1 2 3 4 5 6 7 8(fg:red) 9(off) 10 11 12 13 14 15 16 17 18 19 20(fg:red)(off)               │3 (fg:yellow +bold)!|(off)1 2 3 4 5 6 7 8(fg:green)(off)
                                                                         │4 (fg:green +bold)+|(off)(fg:green)(off)(fg:green)new line(off)
    4 (fg:yellow +bold)!|(off)(fg:red)(off)21 22 23 24 25                                                   │5 (fg:yellow +bold)!|(off)(fg:green)new(off) 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    5   MNO                                                              │6   MNO
    |}]
;;

let%expect_test "Test html output" =
  let prev =
    {|
ABC
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
21 22 23 24 25
MNO
|}
  in
  let next =
    {|
ABC
1 2 3 4 5 6 7 8
new line
new 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
MNO
|}
  in
  let prev = String.split_lines prev |> Array.of_list in
  let next = String.split_lines next |> Array.of_list in
  let module Patdiff = Patdiff_core.Without_unix in
  let hunks =
    Patdiff.diff
      ~context:(-1)
      ~line_big_enough:3
      ~keep_ws:false
      ~find_moves:true
      ~prev
      ~next
  in
  let refined =
    Patdiff.refine_structured
      ~produce_unified_lines:false
      ~keep_ws:false
      ~split_long_lines:false
      ~word_big_enough:3
      ~interleave:true
      hunks
  in
  Patdiff.print_side_by_side
    ~width_override:100
    ~file_names:(File_name.Fake "a", File_name.Fake "b")
    ~rules:Format.Rules.default
    ~wrap_or_truncate:`wrap
    ~output:Html
    refined;
  [%expect
    {|
    <pre style="font-family:consolas,monospace">
      <span style="color:#880000"><span style="font-weight:bold">-|</span></span><span style="color:#880000"></span>a                                            │  <span style="color:#008800"><span style="font-weight:bold">+|</span></span><span style="color:#008800"></span>b
    1                                                │1
    2   ABC                                          │2   ABC
    3 <span style="color:#888800"><span style="font-weight:bold">!|</span></span>1 2 3 4 5 6 7 8<span style="color:#880000"> 9</span> 10 11 12 13 14 15 16 17 18 │3 <span style="color:#888800"><span style="font-weight:bold">!|</span></span>1 2 3 4 5 6 7 8<span style="color:#008800"></span>
        19 20<span style="color:#880000"></span>                                        │
                                                     │4 <span style="color:#008800"><span style="font-weight:bold">+|</span></span><span style="color:#008800"></span><span style="color:#008800">new line</span>
    4 <span style="color:#888800"><span style="font-weight:bold">!|</span></span><span style="color:#880000"></span>21 22 23 24 25                               │5 <span style="color:#888800"><span style="font-weight:bold">!|</span></span><span style="color:#008800">new</span> 10 11 12 13 14 15 16 17 18 19 20 21 22 23
                                                     │     24 25
    5   MNO                                          │6   MNO
    </pre>
    |}]
;;
