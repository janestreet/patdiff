open! Core
open! Import
open Patdiff_kernel
module Patdiff_core = Patdiff_core.Without_unix

let test_refined_structured ?mark_newline_changes ~produce_unified_lines ~prev ~next () =
  let prev = String.split_lines prev |> Array.of_list in
  let next = String.split_lines next |> Array.of_list in
  let hunks =
    Patdiff_core.diff
      ~context:Configuration.default_context
      ~line_big_enough:Configuration.default_line_big_enough
      ~keep_ws:false
      ~find_moves:true
      ~prev
      ~next
  in
  let refined_hunks =
    Patdiff_core.refine_structured
      ?mark_newline_changes
      ~produce_unified_lines
      ~keep_ws:false
      ~split_long_lines:true
      ~interleave:true
      ~word_big_enough:Configuration.default_word_big_enough
      hunks
  in
  print_s
    [%sexp
      (refined_hunks
       : ([ `Next | `Prev | `Same ] * string) list Patience_diff_lib.Patience_diff.Hunk.t
           list)]
;;

let%expect_test _ =
  let prev =
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
  in
  let next =
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
|}
  in
  test_refined_structured ~produce_unified_lines:true ~prev ~next ();
  [%expect
    {|
    ((
      (prev_start 1)
      (prev_size  12)
      (next_start 1)
      (next_size  12)
      (ranges (
        (Same (
          (((Same ""))   ((Same "")))
          (((Same a))    ((Same a)))
          (((Same b))    ((Same b)))
          (((Same cats)) ((Same cats)))))
        (Replace
          ((
            (Same "")
            (Same "dogs are the")
            (Prev " best")
            (Same " pets to have")))
          ((
            (Same "")
            (Same "dogs are the")
            (Next " cutest")
            (Same " pets to have")))
          ())
        (Same (
          (((Same elephant)) ((Same elephant)))
          (((Same f))        ((Same f)))
          (((Same g))        ((Same g)))
          (((Same h))        ((Same h)))
          (((Same i))        ((Same i)))
          (((Same j))        ((Same j)))
          (((Same k))        ((Same k)))))))))
    |}]
;;

let%expect_test _ =
  let prev =
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
  in
  let next =
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
|}
  in
  test_refined_structured ~produce_unified_lines:true ~prev ~next ();
  [%expect
    {|
    ((
      (prev_start 1)
      (prev_size  49)
      (next_start 1)
      (next_size  47)
      (ranges (
        (Same (
          (((Same ""))
           ((Same "")))
          (((
             Same
             "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce sit amet"))
           ((
             Same
             "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce sit amet")))
          (((
             Same
             "malesuada leo. Vivamus vitae orci quis justo ornare molestie. Donec fringilla"))
           ((
             Same
             "malesuada leo. Vivamus vitae orci quis justo ornare molestie. Donec fringilla")))
          (((
             Same
             "tempus magna, ut semper lacus tincidunt at. Suspendisse et rutrum arcu. Aliquam"))
           ((
             Same
             "tempus magna, ut semper lacus tincidunt at. Suspendisse et rutrum arcu. Aliquam")))
          (((
             Same
             "erat volutpat. Pellentesque pretium pellentesque elit, a consequat metus"))
           ((
             Same
             "erat volutpat. Pellentesque pretium pellentesque elit, a consequat metus")))
          (((
             Same
             "placerat a. Praesent hendrerit euismod sem nec facilisis. Curabitur finibus ex"))
           ((
             Same
             "placerat a. Praesent hendrerit euismod sem nec facilisis. Curabitur finibus ex")))
          (((
             Same
             "sagittis massa blandit, et dictum lectus lobortis. Sed fringilla fringilla"))
           ((
             Same
             "sagittis massa blandit, et dictum lectus lobortis. Sed fringilla fringilla")))
          (((
             Same
             "tortor vel finibus. Sed vel tortor pulvinar, fermentum quam non, blandit lorem."))
           ((
             Same
             "tortor vel finibus. Sed vel tortor pulvinar, fermentum quam non, blandit lorem.")))))
        (Next
          (((Same ""))
           ((
             Same
             "Cras non semper ante. Vivamus non nulla scelerisque, fermentum sem at, laoreet"))
           ((
             Same
             "est. Sed convallis, magna sit amet maximus sollicitudin, nulla metus sodales"))
           ((
             Same
             "eros, eu molestie arcu urna ut nunc. Pellentesque habitant morbi tristique"))
           ((
             Same
             "senectus et netus et malesuada fames ac turpis egestas. Morbi sollicitudin,"))
           ((
             Same
             "turpis sit amet ultricies interdum, urna nisl rhoncus tellus, id consectetur")))
          ((Move 0)))
        (Prev
          (((
             Same
             "urna risus ut arcu. Aliquam hendrerit eros id ex tempor vehicula. Nunc a pretium"))
           ((
             Same
             "risus. Nulla tincidunt, mauris eu pellentesque hendrerit, nisi nibh volutpat"))
           ((
             Same
             "sapien, vitae vehicula lacus tellus dictum augue. Pellentesque malesuada vitae")))
          ((Within_move 0)))
        (Next
          (((
             Same
             "tellus lobortis laoreet. Donec fringilla lacinia nulla sit amet eleifend."))
           ((
             Same
             "Suspendisse iaculis metus sed massa bibendum, quis consequat metus lacinia."))
           ((
             Same
             "Etiam scelerisque odio nec pulvinar dapibus. Duis interdum interdum quam vel"))
           ((
             Same
             "dapibus. Quisque dapibus nisl quis magna accumsan, et lobortis magna eleifend."))
           ((
             Same
             "Ut venenatis cursus diam, vel dictum augue interdum vitae. Ut scelerisque"))
           ((Same "condimentum augue, eget bibendum augue lacinia in.")))
          ((Move 0)))
        (Same ((
          ((Same ""))
          ((Same "")))))
        (Same (
          (((Same ""))
           ((Same "")))
          (((
             Same
             "Maecenas ac elit turpis. Nam ex turpis, ullamcorper et ultricies eu, pretium et"))
           ((
             Same
             "Maecenas ac elit turpis. Nam ex turpis, ullamcorper et ultricies eu, pretium et")))
          (((
             Same
             "elit. Duis bibendum aliquet quam et tempor. Donec quis dapibus justo. Praesent"))
           ((
             Same
             "elit. Duis bibendum aliquet quam et tempor. Donec quis dapibus justo. Praesent")))
          (((
             Same
             "eget pellentesque nisi. Nulla vestibulum orci quis dui laoreet, eget posuere sem"))
           ((
             Same
             "eget pellentesque nisi. Nulla vestibulum orci quis dui laoreet, eget posuere sem")))
          (((
             Same
             "interdum. Morbi ac sodales ligula. Proin arcu ipsum, venenatis id cursus et,"))
           ((
             Same
             "interdum. Morbi ac sodales ligula. Proin arcu ipsum, venenatis id cursus et,")))
          (((
             Same
             "blandit eu mi. Sed iaculis egestas ligula, lacinia condimentum velit commodo"))
           ((
             Same
             "blandit eu mi. Sed iaculis egestas ligula, lacinia condimentum velit commodo")))
          (((
             Same
             "non. In eu elit convallis, tempus sapien sed, maximus purus. Sed vitae enim et"))
           ((
             Same
             "non. In eu elit convallis, tempus sapien sed, maximus purus. Sed vitae enim et")))
          (((
             Same
             "tellus accumsan bibendum eu vel turpis. Phasellus massa leo, eleifend vel"))
           ((
             Same
             "tellus accumsan bibendum eu vel turpis. Phasellus massa leo, eleifend vel")))
          (((
             Same
             "tincidunt ut, consequat et est. Duis quis condimentum ex. Etiam nec faucibus"))
           ((
             Same
             "tincidunt ut, consequat et est. Duis quis condimentum ex. Etiam nec faucibus")))
          (((
             Same
             "lorem. Aliquam vehicula porta sapien, ut aliquam purus cursus vitae. Nullam at"))
           ((
             Same
             "lorem. Aliquam vehicula porta sapien, ut aliquam purus cursus vitae. Nullam at")))
          (((Same "ex vehicula, egestas sapien vitae, molestie ipsum."))
           ((Same "ex vehicula, egestas sapien vitae, molestie ipsum.")))
          (((Same ""))
           ((Same "")))
          (((
             Same
             "Suspendisse iaculis lacinia arcu a vehicula. Nunc eleifend fermentum iaculis."))
           ((
             Same
             "Suspendisse iaculis lacinia arcu a vehicula. Nunc eleifend fermentum iaculis.")))
          (((
             Same
             "Duis dignissim, mi sit amet vehicula auctor, odio mauris consectetur lectus, ac"))
           ((
             Same
             "Duis dignissim, mi sit amet vehicula auctor, odio mauris consectetur lectus, ac")))
          (((
             Same
             "tincidunt lorem diam a nisi. Duis vehicula ex ac tortor sagittis, ac commodo"))
           ((
             Same
             "tincidunt lorem diam a nisi. Duis vehicula ex ac tortor sagittis, ac commodo")))
          (((
             Same
             "lectus venenatis. Nam efficitur justo eros, et ornare neque aliquet ut. Duis"))
           ((
             Same
             "lectus venenatis. Nam efficitur justo eros, et ornare neque aliquet ut. Duis")))
          (((
             Same
             "vulputate nulla nunc, eget pellentesque diam aliquam at. Cras rhoncus orci at"))
           ((
             Same
             "vulputate nulla nunc, eget pellentesque diam aliquam at. Cras rhoncus orci at")))
          (((Same "tortor posuere convallis sed sed risus. Quisque sed ipsum ex."))
           ((Same "tortor posuere convallis sed sed risus. Quisque sed ipsum ex.")))))
        (Prev
          (((Same ""))
           ((
             Same
             "Cras non semper ante. Vivamus non nulla scelerisque, fermentum sem at, laoreet"))
           ((
             Same
             "est. Sed convallis, magna sit amet maximus sollicitudin, nulla metus sodales"))
           ((
             Same
             "eros, eu molestie arcu urna ut nunc. Pellentesque habitant morbi tristique"))
           ((
             Same
             "senectus et netus et malesuada fames ac turpis egestas. Morbi sollicitudin,"))
           ((
             Same
             "turpis sit amet ultricies interdum, urna nisl rhoncus tellus, id consectetur"))
           ((
             Same
             "urna risus ut arcu. Aliquam hendrerit eros id ex tempor vehicula. Nunc a pretium"))
           ((
             Same
             "risus. Nulla tincidunt, mauris eu pellentesque hendrerit, nisi nibh volutpat"))
           ((
             Same
             "sapien, vitae vehicula lacus tellus dictum augue. Pellentesque malesuada vitae"))
           ((
             Same
             "tellus lobortis laoreet. Donec fringilla lacinia nulla sit amet eleifend."))
           ((
             Same
             "Suspendisse iaculis metus sed massa bibendum, quis consequat metus lacinia."))
           ((
             Same
             "Etiam scelerisque odio nec pulvinar dapibus. Duis interdum interdum quam vel"))
           ((
             Same
             "dapibus. Quisque dapibus nisl quis magna accumsan, et lobortis magna eleifend."))
           ((
             Same
             "Ut venenatis cursus diam, vel dictum augue interdum vitae. Ut scelerisque"))
           ((Same "condimentum augue, eget bibendum augue lacinia in.")))
          ((Move 0)))
        (Same (
          (((Same ""))
           ((Same "")))
          (((
             Same
             "Aenean porta elit vitae pharetra dapibus. Duis a odio neque. Curabitur"))
           ((
             Same
             "Aenean porta elit vitae pharetra dapibus. Duis a odio neque. Curabitur")))
          (((
             Same
             "ullamcorper enim ut metus luctus, eu blandit augue consectetur. Vestibulum"))
           ((
             Same
             "ullamcorper enim ut metus luctus, eu blandit augue consectetur. Vestibulum")))
          (((
             Same
             "blandit lorem eget blandit fringilla. In et libero non lacus elementum pulvinar"))
           ((
             Same
             "blandit lorem eget blandit fringilla. In et libero non lacus elementum pulvinar")))
          (((
             Same
             "id a orci. Maecenas porta urna mollis, egestas lacus id, feugiat nisi. Vivamus"))
           ((
             Same
             "id a orci. Maecenas porta urna mollis, egestas lacus id, feugiat nisi. Vivamus")))
          (((
             Same
             "imperdiet ornare dui eleifend semper. Integer erat ipsum, vestibulum a lobortis"))
           ((
             Same
             "imperdiet ornare dui eleifend semper. Integer erat ipsum, vestibulum a lobortis")))
          (((
             Same
             "eu, posuere in orci. Pellentesque gravida in purus eu ullamcorper. Nunc urna"))
           ((
             Same
             "eu, posuere in orci. Pellentesque gravida in purus eu ullamcorper. Nunc urna")))
          (((Same "tortor, hendrerit nec eleifend et, dapibus sed dolor."))
           ((Same "tortor, hendrerit nec eleifend et, dapibus sed dolor.")))))))))
    |}]
;;

let%expect_test "test generating a unified structured range" =
  let prev = {|Some really long line that will be unified after I make a small change|} in
  let next = {|Some really long line that will be unified after I make small change|} in
  test_refined_structured ~produce_unified_lines:true ~prev ~next ();
  [%expect
    {|
    ((
      (prev_start 1)
      (prev_size  1)
      (next_start 1)
      (next_size  1)
      (ranges ((
        Unified
        ((
          (Same "")
          (Same "Some really long line that will be unified after I make")
          (Prev " a")
          (Same " small change")))
        ())))))
    |}]
;;

let%expect_test "test newline changes while ignoring whitespace" =
  let prev =
    {|Line one
Some line that will be broken up over multiple lines
Line two
Line three
|}
  in
  let next =
    {|Line one
Some line that will be
broken up over
multiple lines
Line two
Line three
|}
  in
  test_refined_structured ~produce_unified_lines:false ~prev ~next ();
  [%expect {| () |}]
;;

let%expect_test "test newline changes with [mark_newline_changes]" =
  let prev =
    {|Line one
Some line that will be broken up over multiple lines
Line two
Line three
|}
  in
  let next =
    {|Line one
Some line that will be
broken up over
multiple lines
Line two
Line three
|}
  in
  test_refined_structured
    ~mark_newline_changes:true
    ~produce_unified_lines:false
    ~prev
    ~next
    ();
  (* With [mark_newline_changes], the single line that was broken up into multiple lines
     should now be marked as a [Replace], with the difference being the added [Next ""]s
     representing the newlines. *)
  [%expect
    {|
    ((
      (prev_start 1)
      (prev_size  4)
      (next_start 1)
      (next_size  6)
      (ranges (
        (Same ((
          ((Same "Line one"))
          ((Same "Line one")))))
        (Replace
          ((
            (Same "")
            (Same "Some line that will be")
            (Same " broken up over")
            (Same " multiple lines")))
          (((Same "") (Same "Some line that will be") (Next ""))
           ((Next "") (Same "broken up over")         (Next ""))
           ((Next "")
            (Same "multiple lines")))
          ())
        (Same (
          (((Same "Line two"))   ((Same "Line two")))
          (((Same "Line three")) ((Same "Line three")))))))))
    |}]
;;
