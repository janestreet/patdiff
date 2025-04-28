open! Core
open! Async
open! Import

let prev =
  {|
    assert (
      Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));
    assert (
      Int.( = )
        (Set.length t.by_varying_usage)
        (Hashtbl.length t.bucket_id_to_keys)))
|}
;;

let next =
  {|
  assert (Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));
  assert (
    Int.( = ) (Set.length t.by_varying_usage) (Hashtbl.length t.bucket_id_to_keys))
|}
;;

let%expect_test "default" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,7 +1,4(-weight) ============================================================
    (bg:gray fg:black) |(bg:default fg:default)
    (bg:green fg:black)+|(bg:default fg:green)  assert (Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));(fg:default)
    (bg:gray fg:black) |(bg:default fg:default)  assert (
    (bg:yellow fg:black)!|(bg:default fg:default)      Int.( = ) (Set.length t.by_varying_usage) ((fg:red)Set.length t.by_constant_usage));(fg:default)
    (bg:yellow fg:black)!|(bg:default fg:red)    assert ((fg:default)
    (bg:yellow fg:black)!|(bg:default fg:red)      Int.( = )(fg:default)
    (bg:yellow fg:black)!|(bg:default fg:red)        (Set.length t.by_varying_usage)(fg:default)
    (bg:yellow fg:black)!|(bg:default fg:red)        ((fg:default)Hashtbl.length t.bucket_id_to_keys(fg:red))(fg:default)))
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

let%expect_test "-no-semantic-cleanup" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[ "-no-semantic-cleanup" ] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,7 +1,4(-weight) ============================================================
    (bg:gray fg:black) |(bg:default fg:default)
    (bg:green fg:black)+|(bg:default fg:green)  assert (Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));(fg:default)
    (bg:gray fg:black) |(bg:default fg:default)  assert (
    (bg:yellow fg:black)!|(bg:default fg:default)      Int.( = ) (Set.length t.by_varying_usage) ((fg:red)Set.length t.by_constant_usage));(fg:default)
    (bg:yellow fg:black)!|(bg:default fg:red)    assert ((fg:default)
    (bg:yellow fg:black)!|(bg:default fg:red)      Int.( = )(fg:default)
    (bg:yellow fg:black)!|(bg:default fg:red)        (Set.length t.by_varying_usage)(fg:default)
    (bg:yellow fg:black)!|(bg:default fg:red)        ((fg:default)Hashtbl.length t.bucket_id_to_keys(fg:red))(fg:default)))
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;

(* The diff gets much better if I add a newline to both inputs. *)

let prev =
  {|
    assert (
      Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));

    assert (
      Int.( = )
        (Set.length t.by_varying_usage)
        (Hashtbl.length t.bucket_id_to_keys)))
|}
;;

let next =
  {|
  assert (Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));

  assert (
    Int.( = ) (Set.length t.by_varying_usage) (Hashtbl.length t.bucket_id_to_keys))
|}
;;

let%expect_test "with extra newlines" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[] in
  [%expect
    {|
    (fg:red)------ (fg:default +bold)prev/file(-weight)
    (fg:green)++++++ (fg:default +bold)next/file(-weight)
    (bg:gray fg:black)@|(bg:default fg:default +bold)-1,8 +1,5(-weight) ============================================================
    (bg:gray fg:black) |(bg:default fg:default)
    (bg:gray fg:black) |(bg:default fg:default)  assert (Int.( = ) (Set.length t.by_varying_usage) (Set.length t.by_constant_usage));
    (bg:gray fg:black) |(bg:default fg:default)
    (bg:gray fg:black) |(bg:default fg:default)  assert (
    (bg:yellow fg:black)!|(bg:default fg:default)      Int.( = )
    (bg:yellow fg:black)!|(bg:default fg:default)        (Set.length t.by_varying_usage)
    (bg:yellow fg:black)!|(bg:default fg:default)        (Hashtbl.length t.bucket_id_to_keys(fg:red))(fg:default)))
    ("Unclean exit" (Exit_non_zero 1))
    |}];
  return ()
;;
