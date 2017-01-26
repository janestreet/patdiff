open! Core
open! Async.Std
open! Expect_test_helpers

(* The patdiff devs don't review the patdiff-git-wrapper script. This test is to warn them
   of obvious mistakes, like silently changing the calling convention without updating the
   script. It does not cover all the code paths that git might exercise. *)

let links =
  [ "../bin/patdiff.exe", `In_path_as, "patdiff"
  ; "../bin/patdiff-git-wrapper", `In_path_as, "patdiff-git-wrapper"
  ]
;;

let%expect_test "patdiff-git-wrapper" = within_temp_dir ~links (fun () ->
  (* Set up repo with a dirty working directory. *)
  let%bind () = run "git" [ "init"; "-q" ] in
  let%bind () = Writer.save "foo" ~contents:"foo bar baz\n" in
  let%bind () = run "git" [ "add"; "foo"; ] in
  let%bind () = run "git" [ "commit"; "-a"; "-m"; "z"; "-q"; ] in
  let%bind () = Writer.save "foo" ~contents:"foo baz quux\n" in

  (* Override whatever patdiff config the user has. *)
  let%bind () = run "patdiff" [ "-make-config"; ".patdiff"; ] in
  let%bind () = [%expect {| Default configuration written to .patdiff |}] in
  Unix.putenv ~key:"HOME" ~data:".";

  (* Standard git diff. *)
  let%bind () = run "git" [ "diff" ] in
  let%bind () =
    [%expect {|
      diff --git a/foo b/foo
      index 1aeaedb..434ebd4 100644
      --- a/foo
      +++ b/foo
      @@ -1 +1 @@
      -foo bar baz
      +foo baz quux |}]
  in

  (* Diff according to instructions in the script. *)
  Unix.putenv ~key:"GIT_EXTERNAL_DIFF" ~data:"patdiff-git-wrapper";
  let%bind () = run "git" [ "diff" ] in
  [%expect {|
      [1mpatdiff -git a/foo b/foo
      [1mindex 1aeaedb..0000000 100644
      [1m[0;31m------ [0m[0;1m a/foo[0m
      [1m[0;32m++++++ [0m[0;1m b/foo[0m
      [0;100;30m@|[0m[0;1m-1,1 +1,1[0m ============================================================
      [0;41;30m-|[0m[0;0m[0;2mfoo [0m[0;31mbar [0m[0;2mbaz[0m[0m
      [0;42;30m+|[0m[0;0mfoo baz [0;32mquux[0m[0m |}])
;;
