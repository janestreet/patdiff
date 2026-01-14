open! Core
open! Async
open! Import

(* This test is to warn of obvious mistakes, like silently changing the calling convention
   without updating the script. It does not cover all the code paths that git might
   exercise. *)

let links = ("../../bin/patdiff-git-wrapper", `In_path_as, "patdiff-git-wrapper") :: links

let%expect_test "patdiff-git-wrapper" =
  within_temp_dir ~links (fun () ->
    (* Set up repo with a dirty working directory. *)
    let%bind () = run "git" [ "init"; "-q" ] in
    (* As we don't pass [--global] to [git config], this git config applies only to this
       repo *)
    let%bind () = run "git" [ "config"; "user.email"; "nobody@localhost" ] in
    let%bind () = run "git" [ "config"; "user.name"; "nobody" ] in
    let%bind () = Writer.save "foo" ~contents:"foo bar baz\n" in
    let%bind () = run "git" [ "add"; "foo" ] in
    let%bind () = run "git" [ "commit"; "-a"; "-m"; "z"; "-q" ] in
    let%bind () = Writer.save "foo" ~contents:"foo baz quux\n" in
    (* Override whatever patdiff config the user has. *)
    let%bind () = run "patdiff" [ "-make-config"; ".patdiff" ] in
    [%expect {| Default configuration written to .patdiff |}];
    (Unix.putenv [@ocaml.alert "-unsafe_multidomain"]) ~key:"HOME" ~data:".";
    (* Standard git diff. *)
    let%bind () = run "git" [ "diff" ] in
    [%expect
      {|
      diff --git a/foo b/foo
      index 1aeaedb..434ebd4 100644
      --- a/foo
      +++ b/foo
      @@ -1 +1 @@
      -foo bar baz
      +foo baz quux
      |}];
    (* Diff according to instructions in the script. *)
    (Unix.putenv [@ocaml.alert "-unsafe_multidomain"])
      ~key:"GIT_EXTERNAL_DIFF"
      ~data:"patdiff-git-wrapper";
    let%bind () = system "git diff | ansi_text visualize -minimize" in
    [%expect
      {|
      (+bold)patdiff -git a/foo b/foo
      (+bold)index 1aeaedb..0000000 100644
      (+bold fg:red)------ (fg:default) a/foo(-weight)
      (+bold fg:green)++++++ (fg:default) b/foo(-weight)
      (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,1(-weight) ============================================================
      (bg:red fg:black)-|(off fg:gray-12)foo(fg:red) bar(fg:gray-12) baz(fg:default)
      (bg:green fg:black)+|(off)foo baz(fg:green) quux(fg:default)
      |}];
    return ())
;;
