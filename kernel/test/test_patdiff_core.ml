open! Core
open! Import
open Patdiff_kernel
module Patdiff_core = Patdiff_core.Without_unix

module%test _ = struct
  let prev : Diff_input.t = { name = "old"; text = "Foo bar buzz" }
  let next : Diff_input.t = { name = "old"; text = "Foo buzz" }

  let%expect_test "Ansi output generates a single line diff" =
    printf
      "%s\n"
      (Patdiff_core.patdiff
         ~split_long_lines:false
         ~produce_unified_lines:true
         ~output:Ansi
         ~prev
         ~next
         ());
    [%expect
      {|
      -1,1 +1,1
      [1;33m!|[22;39mFoo[31m bar[39m buzz
      |}]
  ;;

  let%expect_test "Ascii is supported if [produce_unified_lines] is false" =
    printf
      "%s\n"
      (Patdiff_core.patdiff
         ~split_long_lines:false
         ~produce_unified_lines:false
         ~output:Ascii
         ~prev
         ~next
         ());
    [%expect
      {|
      -1,1 +1,1
      -|Foo bar buzz
      +|Foo buzz
      |}]
  ;;

  let%expect_test "don't highlight empty newlines (ascii)" =
    printf
      "%s\n"
      (Patdiff_core.patdiff
         ~keep_ws:true
         ~split_long_lines:false
         ~produce_unified_lines:false
         ~output:Ascii
         ~prev:{ name = "old"; text = "" }
         ~next:{ name = "new"; text = "\n\n\n" }
         ());
    [%expect
      {|
      -1,0 +1,3
      +|
      +|
      +|
      |}]
  ;;

  let%expect_test "don't highlight empty newlines (ansi)" =
    printf
      "%s\n"
      (Patdiff_core.patdiff
         ~keep_ws:true
         ~split_long_lines:false
         ~produce_unified_lines:false
         ~output:Ansi
         ~prev:{ name = "old"; text = "" }
         ~next:{ name = "new"; text = "\n\n\n" }
         ());
    [%expect
      {|
      -1,0 +1,3
      [1;32m+|[22;39m[32m[39m
      [1;32m+|[22;39m[32m[39m
      [1;32m+|[22;39m[32m[39m
      |}]
  ;;

  let%expect_test "do highlight empty newlines with some spaces (ansi)" =
    printf
      "%s\n"
      (Patdiff_core.patdiff
         ~keep_ws:true
         ~split_long_lines:false
         ~produce_unified_lines:false
         ~output:Ansi
         ~prev:{ name = "old"; text = "" }
         ~next:{ name = "new"; text = "  \n  \n  \n" }
         ());
    [%expect
      {|
      -1,0 +1,3
      [1;32m+|[22;39m[7;32m  [27;39m
      [1;32m+|[22;39m[7;32m  [27;39m
      [1;32m+|[22;39m[7;32m  [27;39m
      |}]
  ;;

  let%test "Ascii is not supported if [produce_unified_lines] is true" =
    match
      Patdiff_core.patdiff
        ~split_long_lines:false
        ~produce_unified_lines:true
        ~output:Ascii
        ~prev
        ~next
        ()
    with
    | exception _ -> true
    | (_ : string) -> false
  ;;

  let%expect_test "float tolerance works as expected" =
    [ None, "1.0", "1.00000000000001"
    ; None, "1.0", "1.0"
    ; Some 0.01, "1.0", "1.005"
    ; Some 0.01, "1.0", "1.015"
    ]
    |> List.iter ~f:(fun (mult_float_tolerance, old_text, new_text) ->
      print_endline
        (Patdiff_core.patdiff
           ?float_tolerance:(Option.map mult_float_tolerance ~f:Percent.of_mult)
           ~produce_unified_lines:false
           ~output:Ascii
           ~prev:{ name = "old"; text = old_text }
           ~next:{ name = "new"; text = new_text }
           ()));
    [%expect
      {|
      -1,1 +1,1
      -|1.0
      +|1.00000000000001


      -1,1 +1,1
      -|1.0
      +|1.015
      |}]
  ;;

  let%expect_test "test single empty line" =
    let original =
      {|Line one
Some line that will be deleted (and replaced with a single newline)
  with some indented content on the next line
Line four
Line five
Line six
|}
    in
    let modified =
      {|Line one

Line four
Line five
An added line goes here
Line six|}
    in
    print_endline
      (Patdiff_core.patdiff
         ~produce_unified_lines:false
         ~output:Ascii
         ~prev:{ name = "old"; text = original }
         ~next:{ name = "new"; text = modified }
         ());
    [%expect
      {|
      -1,6 +1,6
        Line one
      -|Some line that will be deleted (and replaced with a single newline)
      -|  with some indented content on the next line
        Line four
        Line five
      +|An added line goes here
        Line six
      |}]
  ;;
end

module%test [@name "python"] _ = struct
  let prev : Diff_input.t = { name = "old.py"; text = "print(5)" }
  let next : Diff_input.t = { name = "new.py"; text = "if True:\n    print(5)" }
  let doesn't_contain_ansi_escapes s = not (String.contains s '\027')

  let%expect_test "Ansi output generates a single line diff" =
    printf
      "%s\n"
      (Patdiff_core.patdiff
         ~split_long_lines:false
         ~produce_unified_lines:true
         ~output:Ansi
         ~prev
         ~next
         ());
    [%expect
      {|
      -1,1 +1,2
      [1;33m!|[22;39m[32mif True:[39m
      [1;33m!|[22;39m[7;32m    [27;39mprint(5)
      |}]
  ;;

  let%expect_test "Ascii is supported if [produce_unified_lines] is false" =
    printf
      "%s\n"
      (Patdiff_core.patdiff
         ~split_long_lines:false
         ~produce_unified_lines:false
         ~output:Ascii
         ~prev
         ~next
         ());
    [%expect
      {|
      -1,1 +1,2
      -|print(5)
      +|if True:
      +|    print(5)
      |}]
  ;;

  let%test _ =
    Patdiff_core.patdiff ~output:Ascii ~produce_unified_lines:false ~prev ~next ()
    |> doesn't_contain_ansi_escapes
  ;;

  let%test _ =
    Patdiff_core.patdiff
      ~output:Ascii
      ~produce_unified_lines:false
      ~keep_ws:false
      ~prev
      ~next
      ()
    |> doesn't_contain_ansi_escapes
  ;;

  let%test _ =
    Patdiff_core.patdiff
      ~output:Ascii
      ~produce_unified_lines:false
      ~keep_ws:true
      ~prev
      ~next
      ()
    |> doesn't_contain_ansi_escapes
  ;;

  let%test _ =
    Patdiff_core.patdiff
      ~output:Ascii
      ~produce_unified_lines:false
      ~rules:(Format.Rules.strip_styles Format.Rules.default)
      ~prev
      ~next
      ()
    |> doesn't_contain_ansi_escapes
  ;;

  let test_moves ~prev ~next =
    let prev : Diff_input.t = { name = "old"; text = prev } in
    let next : Diff_input.t = { name = "new"; text = next } in
    (* Override the default config to show moves better without colors *)
    let rules = Format.Rules.default in
    let removed_in_move = Format.Rule.create ~pre:(Format.Rule.Affix.create ">-") [] in
    let added_in_move = Format.Rule.create ~pre:(Format.Rule.Affix.create ">+") [] in
    let line_unified_in_move =
      Format.Rule.create ~pre:(Format.Rule.Affix.create ">!") []
    in
    let rules = { rules with removed_in_move; added_in_move; line_unified_in_move } in
    printf
      "%s\n"
      (Patdiff_core.patdiff
         ~rules
         ~find_moves:true
         ~prev
         ~next
         ~output:Ascii
         ~produce_unified_lines:false
         ())
  ;;

  let%expect_test "test a simple move" =
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
    test_moves ~prev ~next;
    [%expect
      {|
      -4,46 +4,46
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
      <|
      <|Suspendisse iaculis lacinia arcu a vehicula. Nunc eleifend fermentum iaculis.
      <|Duis dignissim, mi sit amet vehicula auctor, odio mauris consectetur lectus, ac
      <|tincidunt lorem diam a nisi. Duis vehicula ex ac tortor sagittis, ac commodo
      <|lectus venenatis. Nam efficitur justo eros, et ornare neque aliquet ut. Duis
      <|vulputate nulla nunc, eget pellentesque diam aliquam at. Cras rhoncus orci at
      <|tortor posuere convallis sed sed risus. Quisque sed ipsum ex.

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
      >|
      >|Suspendisse iaculis lacinia arcu a vehicula. Nunc eleifend fermentum iaculis.
      >|Duis dignissim, mi sit amet vehicula auctor, odio mauris consectetur lectus, ac
      >|tincidunt lorem diam a nisi. Duis vehicula ex ac tortor sagittis, ac commodo
      >|lectus venenatis. Nam efficitur justo eros, et ornare neque aliquet ut. Duis
      >|vulputate nulla nunc, eget pellentesque diam aliquam at. Cras rhoncus orci at
      >|tortor posuere convallis sed sed risus. Quisque sed ipsum ex.

        Aenean porta elit vitae pharetra dapibus. Duis a odio neque. Curabitur
        ullamcorper enim ut metus luctus, eu blandit augue consectetur. Vestibulum
        blandit lorem eget blandit fringilla. In et libero non lacus elementum pulvinar
        id a orci. Maecenas porta urna mollis, egestas lacus id, feugiat nisi. Vivamus
        imperdiet ornare dui eleifend semper. Integer erat ipsum, vestibulum a lobortis
        eu, posuere in orci. Pellentesque gravida in purus eu ullamcorper. Nunc urna
        tortor, hendrerit nec eleifend et, dapibus sed dolor.
      |}]
  ;;

  let%expect_test "test a move with a slight modification" =
    let prev =
      {|
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

Suspendisse convallis justo vitae leo efficitur ultrices. Fusce ullamcorper nisl
accumsan maximus ultricies. Donec eu orci feugiat, lacinia ipsum a, rhoncus
augue. Nullam mollis, dolor in varius auctor, ex tortor dapibus quam, id
consectetur ipsum nunc ac leo. Maecenas condimentum nunc sed convallis ultrices.
Suspendisse semper hendrerit ante, quis convallis dui eleifend et. Duis dui
risus, laoreet eu suscipit eget, pretium vitae risus. Lorem ipsum dolor sit
amet, consectetur adipiscing elit. Proin bibendum consequat pharetra. Morbi
scelerisque vitae tellus sodales mattis.

Nunc maximus porttitor nulla sit amet porta. Aenean tellus dui, viverra nec
congue vitae, posuere a erat. Vestibulum lectus nibh, gravida et orci at,
gravida eleifend tellus. Vestibulum nec iaculis odio. Donec et dui quis orci
gravida euismod non sed eros. Aliquam ipsum metus, tempus a risus molestie,
maximus vestibulum nibh. Cras sit amet sagittis nunc. Donec bibendum eros
maximus lacus imperdiet, ac mollis quam tristique. Nunc eleifend ullamcorper
tincidunt. Ut ut pulvinar ante. Mauris gravida, arcu vitae suscipit ultrices,
elit metus sagittis odio, at pretium leo leo in odio. Ut dictum mi ac purus
ullamcorper finibus.
|}
    in
    let next =
      {|
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

Suspendisse iaculis lacinia arcu a vehicula. Nunc eleifend fermentum iaculis.
Duis dignissim, mi sit amet vehicula auctor, odio mauris consectetur lectus, ac
tincidunt lorem diam a nisi. Duis vehicula exe ace tortor sagittis, ac commodo
lectus venenatis. Nam efficitur justo eros, et ornare neque aliquet ut. Duis
vulputate nulla nunc, eget pellentesque diam aliquam at. Cras rhoncus orci at
tortor posuere convallis sed sed risus. Quisque sed ipsum ex.

Suspendisse convallis justo vitae leo efficitur ultrices. Fusce ullamcorper nisl
accumsan maximus ultricies. Donec eu orci feugiat, lacinia ipsum a, rhoncus
augue. Nullam mollis, dolor in varius auctor, ex tortor dapibus quam, id
consectetur ipsum nunc ac leo. Maecenas condimentum nunc sed convallis ultrices.
Suspendisse semper hendrerit ante, quis convallis dui eleifend et. Duis dui
risus, laoreet eu suscipit eget, pretium vitae risus. Lorem ipsum dolor sit
amet, consectetur adipiscing elit. Proin bibendum consequat pharetra. Morbi
scelerisque vitae tellus sodales mattis.

Nunc maximus porttitor nulla sit amet porta. Aenean tellus dui, viverra nec
congue vitae, posuere a erat. Vestibulum lectus nibh, gravida et orci at,
gravida eleifend tellus. Vestibulum nec iaculis odio. Donec et dui quis orci
gravida euismod non sed eros. Aliquam ipsum metus, tempus a risus molestie,
maximus vestibulum nibh. Cras sit amet sagittis nunc. Donec bibendum eros
maximus lacus imperdiet, ac mollis quam tristique. Nunc eleifend ullamcorper
tincidunt. Ut ut pulvinar ante. Mauris gravida, arcu vitae suscipit ultrices,
elit metus sagittis odio, at pretium leo leo in odio. Ut dictum mi ac purus
ullamcorper finibus.
|}
    in
    test_moves ~prev ~next;
    [%expect
      {|
      -1,57 +1,57

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
      <|
      <|Suspendisse iaculis lacinia arcu a vehicula. Nunc eleifend fermentum iaculis.
      <|Duis dignissim, mi sit amet vehicula auctor, odio mauris consectetur lectus, ac
      <|tincidunt lorem diam a nisi. Duis vehicula ex ac tortor sagittis, ac commodo
      <|lectus venenatis. Nam efficitur justo eros, et ornare neque aliquet ut. Duis
      <|vulputate nulla nunc, eget pellentesque diam aliquam at. Cras rhoncus orci at
      <|tortor posuere convallis sed sed risus. Quisque sed ipsum ex.

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
      >|
      >|Suspendisse iaculis lacinia arcu a vehicula. Nunc eleifend fermentum iaculis.
      >|Duis dignissim, mi sit amet vehicula auctor, odio mauris consectetur lectus, ac
      >-tincidunt lorem diam a nisi. Duis vehicula ex ac tortor sagittis, ac commodo
      >+tincidunt lorem diam a nisi. Duis vehicula exe ace tortor sagittis, ac commodo
      >|lectus venenatis. Nam efficitur justo eros, et ornare neque aliquet ut. Duis
      >|vulputate nulla nunc, eget pellentesque diam aliquam at. Cras rhoncus orci at
      >|tortor posuere convallis sed sed risus. Quisque sed ipsum ex.

        Suspendisse convallis justo vitae leo efficitur ultrices. Fusce ullamcorper nisl
        accumsan maximus ultricies. Donec eu orci feugiat, lacinia ipsum a, rhoncus
        augue. Nullam mollis, dolor in varius auctor, ex tortor dapibus quam, id
        consectetur ipsum nunc ac leo. Maecenas condimentum nunc sed convallis ultrices.
        Suspendisse semper hendrerit ante, quis convallis dui eleifend et. Duis dui
        risus, laoreet eu suscipit eget, pretium vitae risus. Lorem ipsum dolor sit
        amet, consectetur adipiscing elit. Proin bibendum consequat pharetra. Morbi
        scelerisque vitae tellus sodales mattis.

        Nunc maximus porttitor nulla sit amet porta. Aenean tellus dui, viverra nec
        congue vitae, posuere a erat. Vestibulum lectus nibh, gravida et orci at,
        gravida eleifend tellus. Vestibulum nec iaculis odio. Donec et dui quis orci
        gravida euismod non sed eros. Aliquam ipsum metus, tempus a risus molestie,
        maximus vestibulum nibh. Cras sit amet sagittis nunc. Donec bibendum eros
        maximus lacus imperdiet, ac mollis quam tristique. Nunc eleifend ullamcorper
      |}]
  ;;

  let%expect_test "test we prefer more similar lines" =
    test_moves
      ~prev:
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
|}
      ~next:
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
|};
    [%expect
      {|
      -1,10 +1,13

      <|Some code that is going to get moved somewhere. Make it long so
      <|things are really similar. We only match on at least 3 lines
      <|so make it 3 lines long.
        a
        b
        c
      +|Some code that is going to get moved somewhere. Make it long so
      +|things are really differs. We only match on at least 3 lines
      +|so make it 3 lines long.
        d
        e
        f
      >|Some code that is going to get moved somewhere. Make it long so
      >|things are really similar. We only match on at least 3 lines
      >|so make it 3 lines long.
      |}]
  ;;

  let%expect_test "test moves of match statements" =
    test_moves
      ~prev:
        {|
let result =
  (match variable with
  | Case1 case1 ->
    let a = b in
    let double = case1 * case1 in
    a + double
  | Case2 case2 ->
    let foo = case2 ^ "-something" in
    let bar = String.length foo in
    String.is_substring (bar ^ Int.to_string) ~substring
  | Case3 case3 ->
      let one_third = case3 *. 0.3333 in
      let inverse = 1. -. one_third in
      Global.log.debug_s [%sexp "Case3"];
      let sqrt = sqrt inverse in
      let sum = ref 0. in
      List.iter [ 1; 2; 3] ~f:(fun _index ->
        sum := !sum +. sqrt;
      );
      Percentage.of_float !sum) in
fetch result
|}
      ~next:
        {|
let result =
  (match variable with
  | Case3 case3 ->
      let one_third = case3 *. 0.3333 in
      let inverse = 1. -. one_third in
      Global.log.debug_s [%sexp "Case3"];
      let sqrt = sqrt inverse in
      let sum = ref 0. in
      List.iter [ 1; 2; 3] ~f:(fun _index ->
        sum := !sum +. sqrt;
      );
      Percentage.of_float !sum
  | Case1 case1 ->
    let a = b in
    let double = case1 * case1 in
    a + double
  | Case2 case2 ->
    let foo = case2 ^ "-something" in
    let bar = String.length foo in
    String.is_substring (bar ^ Int.to_string) ~substring) in
fetch result
|};
    [%expect
      {|
      -1,22 +1,22

        let result =
          (match variable with
      <|  | Case1 case1 ->
      <|    let a = b in
      <|    let double = case1 * case1 in
      <|    a + double
      <|  | Case2 case2 ->
      <|    let foo = case2 ^ "-something" in
      <|    let bar = String.length foo in
      <|    String.is_substring (bar ^ Int.to_string) ~substring
          | Case3 case3 ->
              let one_third = case3 *. 0.3333 in
              let inverse = 1. -. one_third in
              Global.log.debug_s [%sexp "Case3"];
              let sqrt = sqrt inverse in
              let sum = ref 0. in
              List.iter [ 1; 2; 3] ~f:(fun _index ->
                sum := !sum +. sqrt;
              );
      -|      Percentage.of_float !sum) in
      +|      Percentage.of_float !sum
      >|  | Case1 case1 ->
      >|    let a = b in
      >|    let double = case1 * case1 in
      >|    a + double
      >|  | Case2 case2 ->
      >|    let foo = case2 ^ "-something" in
      >|    let bar = String.length foo in
      >-    String.is_substring (bar ^ Int.to_string) ~substring
      >+    String.is_substring (bar ^ Int.to_string) ~substring) in
        fetch result
      |}]
  ;;

  let%expect_test "make sure we don't use a replace as a move" =
    test_moves
      ~prev:
        {|
module Stable : sig
  module Row : sig
    module V1 : sig
      type t
    end

    module V2 : sig
      type t
    end

    module V3 : sig
      type t = Row.t
    end

    module V4 : sig
      type t =
        { first_name : string
        ; last_name : string
        ; age : int
        ; address : string
        ; favorite_food : string
        }
    end
  end

  module V1 : sig
    type t [@@deriving bin_io, sexp_of, compare]
  end

  module V2 : sig
    type t [@@deriving bin_io, sexp_of, compare]

    val to_v1 : t -> V1.t
  end

  module V3 : sig
    type nonrec t = t [@@deriving bin_io, sexp_of, compare]

    val to_v2 : t -> V2.t
  end

  module V4 : sig
    type t = Row.V4.t list [@@deriving bin_io, sexp_of, compare]

    val to_v3 : t -> V3.t
  end
end
|}
      ~next:
        {|
module Stable : sig
  module Row : sig
    module V1 : sig
      type t
    end

    module V2 : sig
      type t
    end

    module V3 : sig
      type t
    end

    module V4 : sig
      type t = Row.t
    end
  end

  module V1 : sig
    type t [@@deriving bin_io, sexp_of, compare]
  end

  module V2 : sig
    type t [@@deriving bin_io, sexp_of, compare]

    val to_v1 : t -> V1.t
  end

  module V3 : sig
    type t [@@deriving bin_io, sexp_of, compare]

    val to_v2 : t -> V2.t
  end

  module V4 : sig
    type nonrec t = t [@@deriving bin_io, sexp_of, compare]

    val to_v3 : t -> V3.t
  end
end
|};
    [%expect
      {|
      -1,48 +1,42

        module Stable : sig
          module Row : sig
            module V1 : sig
              type t
            end

            module V2 : sig
              type t
            end

            module V3 : sig
      -|      type t = Row.t
      -|    end
      -|
      -|    module V4 : sig
      -|      type t =
      -|        { first_name : string
      -|        ; last_name : string
      -|        ; age : int
      -|        ; address : string
      -|        ; favorite_food : string
      -|        }
      +|      type t
      +|    end
      +|
      +|    module V4 : sig
      +|      type t = Row.t
            end
          end

          module V1 : sig
            type t [@@deriving bin_io, sexp_of, compare]
          end

          module V2 : sig
            type t [@@deriving bin_io, sexp_of, compare]

            val to_v1 : t -> V1.t
          end

          module V3 : sig
      -|    type nonrec t = t [@@deriving bin_io, sexp_of, compare]
      -|
      -|    val to_v2 : t -> V2.t
      -|  end
      -|
      -|  module V4 : sig
      -|    type t = Row.V4.t list [@@deriving bin_io, sexp_of, compare]
      +|    type t [@@deriving bin_io, sexp_of, compare]
      +|
      +|    val to_v2 : t -> V2.t
      +|  end
      +|
      +|  module V4 : sig
      +|    type nonrec t = t [@@deriving bin_io, sexp_of, compare]

            val to_v3 : t -> V3.t
          end
        end
      |}]
  ;;

  let%expect_test "don't include deletions or additions on the edges of moves" =
    test_moves
      ~prev:
        {|
        1
        2
        3
        to_delete
        5
        6
        7
        8
        9
        10
        11
        12
        13
        14
        15
|}
      ~next:
        {|
        1
        2
        3
        8
        9
        10
        11
        12
        5
        6
        7
        added
        13
        14
        15
|};
    [%expect
      {|
      -1,16 +1,16

                1
                2
                3
      -|        to_delete
      <|        5
      <|        6
      <|        7
                8
                9
                10
                11
                12
      >|        5
      >|        6
      >|        7
      +|        added
                13
                14
                15
      |}]
  ;;

  let%expect_test "a move plus not keeping whitespace hides deleted empty lines" =
    test_moves
      ~prev:
        {|
a
b
c
d
e
f
g
h
i
j
k
l

section
to
move

m
n
o
p
|}
      ~next:
        {|
a
b
c
section
to
move
d
e
f
g
h
i
j
k
l
m
n
o
p
|};
    [%expect
      {|
      -1,22 +1,20

        a
        b
        c
      >|section
      >|to
      >|move
        d
        e
        f
        g
        h
        i
        j
        k
        l
      <|section
      <|to
      <|move
        m
        n
        o
        p
      |}]
  ;;

  let%expect_test "test moves when nesting changes" =
    test_moves
      ~prev:
        {|
let foo = 3

let bar = 4

let rec test x =
  if x > 0
  then test (x-1)
  else x
;;

let message =
  "This is a message"
;;

let call_the_server () =
  Server.call {
      user;
      password;
      request
  }
;;

let read_the_file () =
  Reader.load_sexp "some-really-long file-path.sexp"
;;
|}
      ~next:
        {|
module Server = struct
  let call_the_server () =
    Server.call {
        user;
        password;
        request
    }
  ;;

  let read_the_file () =
    Reader.load_sexp
       "some-really-long file-path.sexp"
  ;;
end

let foo = 3

let bar = 4

let rec test x =
  if x > 0
  then test (x-1)
  else x
;;

let message =
  "This is a message"
;;

include Server
|};
    [%expect
      {|
      -1,26 +1,31
      +|
      +|module Server = struct
      >|  let call_the_server () =
      >|    Server.call {
      >|        user;
      >|        password;
      >|        request
      >|    }
      >|  ;;
      >|
      >|  let read_the_file () =
      >-  Reader.load_sexp "some-really-long file-path.sexp"
      >+    Reader.load_sexp
      >+       "some-really-long file-path.sexp"
      >|  ;;
      +|end

        let foo = 3

        let bar = 4

        let rec test x =
          if x > 0
          then test (x-1)
          else x
        ;;

        let message =
          "This is a message"
        ;;

      <|let call_the_server () =
      <|  Server.call {
      <|      user;
      <|      password;
      <|      request
      <|  }
      <|;;
      <|
      <|let read_the_file () =
      <|  Reader.load_sexp "some-really-long file-path.sexp"
      <|;;
      +|include Server
      |}]
  ;;

  module Action = struct
    type t =
      | Move of
          { start : int
          ; length : int
          ; to_ : int
          }
      | Delete of
          { start : int
          ; length : int
          }
      | Insert of
          { start : int
          ; contents : string
          }
    [@@deriving sexp_of]

    let gen_move ~str_length =
      let open Quickcheck.Generator.Let_syntax in
      let%bind start = Int.gen_incl 0 str_length in
      let%bind length = Int.gen_incl 0 str_length in
      let%bind to_ = Int.gen_incl 0 str_length in
      return (Move { start; length; to_ })
    ;;

    let gen_delete ~str_length =
      let open Quickcheck.Generator.Let_syntax in
      let%bind start = Int.gen_incl 0 str_length in
      let%bind length = Int.gen_incl 0 str_length in
      return (Delete { start; length })
    ;;

    let gen_insert ~str_length =
      let open Quickcheck.Generator.Let_syntax in
      let%bind start = Int.gen_incl 0 str_length in
      let%bind contents = String.gen_nonempty' Char.gen_alpha in
      return (Insert { start; contents })
    ;;

    let gen ~str_length =
      Quickcheck.Generator.union
        [ gen_move ~str_length; gen_delete ~str_length; gen_insert ~str_length ]
    ;;

    (* All of these actions just clamp their indices if the are out of range *)
    let apply str action =
      match action with
      | Move { start; length; to_ } ->
        let prefix = String.prefix str start in
        let suffix = String.drop_prefix str (start + length) in
        let to_move =
          String.drop_prefix str start |> fun str -> String.prefix str length
        in
        let before_insert = prefix ^ suffix in
        let prefix = String.prefix before_insert to_ in
        let suffix = String.drop_prefix before_insert to_ in
        prefix ^ to_move ^ suffix
      | Delete { start; length } ->
        String.prefix str start ^ String.drop_prefix str (start + length)
      | Insert { start; contents } ->
        String.prefix str start ^ contents ^ String.drop_prefix str start
    ;;
  end

  module Test_case = struct
    type t =
      { prev : string array
      ; actions : Action.t list
      }
    [@@deriving sexp_of]
  end

  let%test_unit "ensure we can always recover the original content if there are moves" =
    let test_case_gen =
      let open Quickcheck.Generator.Let_syntax in
      let%bind prev = List.gen_with_length 50 (String.gen_nonempty' Char.gen_alpha) in
      let prev = Array.of_list prev in
      let str_length = Array.sum (module Int) prev ~f:String.length in
      let%bind actions =
        List.gen_with_length 5 (Quickcheck.Generator.union [ Action.gen ~str_length ])
      in
      return { Test_case.prev; actions }
    in
    Quickcheck.test ~sexp_of:Test_case.sexp_of_t test_case_gen ~f:(fun test_case ->
      let prev_string = String.concat ~sep:"\n" (Array.to_list test_case.prev) in
      let next_string = List.fold test_case.actions ~init:prev_string ~f:Action.apply in
      let next = String.split_lines next_string |> Array.of_list in
      let hunks =
        Patdiff_core.diff
          ~context:(-1)
          ~line_big_enough:3
          ~keep_ws:false
          ~find_moves:true
          ~prev:test_case.prev
          ~next
      in
      let recover_prev =
        List.concat_map hunks ~f:(fun hunk ->
          Import.Patience_diff.Range.prev_only hunk.ranges)
        |> List.map ~f:(function
          | Prev (prev, _) -> prev
          | Same same -> Array.map same ~f:fst
          | range -> raise_s [%sexp (range : string Import.Patience_diff.Range.t)])
        |> Array.concat
        |> String.concat_array ~sep:"\n"
      in
      let recover_next =
        List.concat_map hunks ~f:(fun hunk ->
          Import.Patience_diff.Range.next_only hunk.ranges)
        |> List.map ~f:(function
          | Next (next, _) -> next
          | Same same -> Array.map same ~f:snd
          | range -> raise_s [%sexp (range : string Import.Patience_diff.Range.t)])
        |> Array.concat
        |> String.concat_array ~sep:"\n"
      in
      (* Avoid comparing duplicate newlines at the end of the strings *)
      [%test_result: string]
        (String.strip recover_prev)
        ~expect:(String.strip prev_string);
      [%test_result: string]
        (String.strip recover_next)
        ~expect:(String.strip next_string))
  ;;

  let%test_unit "ensure that our refinement heuristic does not drop any changes" =
    (* To trigger the refinement heuristic we need all lines to be different and there is
       at at least one word shared between the two *)
    let test_case_gen =
      let open Quickcheck.Generator.Let_syntax in
      let%bind lines_prev = Int.gen_incl 200 500 in
      let%bind lines_next = Int.gen_incl 200 500 in
      let%bind unique_word_prev_line = Int.gen_incl 0 (lines_prev - 1) in
      let%bind unique_word_next_line = Int.gen_incl 0 (lines_next - 1) in
      let unique_word = "uniqueword" in
      let prev_words = ref 0 in
      let prev =
        Array.init lines_prev ~f:(fun line_idx ->
          List.init 5 ~f:(fun word_idx ->
            if line_idx = unique_word_prev_line && word_idx = 0
            then unique_word
            else (
              let word = "prevword" ^ Int.to_string !prev_words in
              incr prev_words;
              word))
          |> String.concat ~sep:" ")
      in
      let next_words = ref 0 in
      let next =
        Array.init lines_next ~f:(fun line_idx ->
          List.init 5 ~f:(fun word_idx ->
            if line_idx = unique_word_next_line && word_idx = 0
            then unique_word
            else (
              let word = "nextword" ^ Int.to_string !next_words in
              incr next_words;
              word))
          |> String.concat ~sep:" ")
      in
      return (prev, next)
    in
    Quickcheck.test ~trials:50 test_case_gen ~f:(fun (prev, next) ->
      let prev_string = String.concat ~sep:"\n" (Array.to_list prev) in
      let next_string = String.concat ~sep:"\n" (Array.to_list next) in
      let hunks =
        Patdiff_core.diff
          ~context:(-1)
          ~line_big_enough:3
          ~keep_ws:false
          ~find_moves:true
          ~prev
          ~next
        |> Patdiff_core.refine_structured
             ~produce_unified_lines:false
             ~keep_ws:false
             ~split_long_lines:false
             ~interleave:false
             ~word_big_enough:Configuration.default_word_big_enough
      in
      let recover_prev =
        List.concat_map hunks ~f:(fun hunk ->
          Import.Patience_diff.Range.prev_only hunk.ranges)
        |> List.map ~f:(function
          | Prev (prev, _) ->
            Array.map prev ~f:(fun line -> List.map line ~f:snd |> String.concat)
          | Same same ->
            Array.map same ~f:(fun (line, _) -> List.map line ~f:snd |> String.concat)
          | range ->
            raise_s
              [%sexp
                (range
                 : ([ `Next | `Prev | `Same ] * string) list Import.Patience_diff.Range.t)])
        |> Array.concat
        |> String.concat_array ~sep:"\n"
      in
      let recover_next =
        List.concat_map hunks ~f:(fun hunk ->
          Import.Patience_diff.Range.next_only hunk.ranges)
        |> List.map ~f:(function
          | Next (next, _) ->
            Array.map next ~f:(fun line -> List.map line ~f:snd |> String.concat)
          | Same same ->
            Array.map same ~f:(fun (_, line) -> List.map line ~f:snd |> String.concat)
          | range ->
            raise_s
              [%sexp
                (range
                 : ([ `Next | `Prev | `Same ] * string) list Import.Patience_diff.Range.t)])
        |> Array.concat
        |> String.concat_array ~sep:"\n"
      in
      (* Avoid comparing duplicate newlines at the end of the strings *)
      [%test_result: string]
        (String.strip recover_prev)
        ~expect:(String.strip prev_string);
      [%test_result: string]
        (String.strip recover_next)
        ~expect:(String.strip next_string))
  ;;
end
