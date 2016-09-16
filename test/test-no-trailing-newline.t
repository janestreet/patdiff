Test handling of text files missing a trailing newline.

  $ echo "foo" > with_newline
  $ echo -n "foo" > missing_newline
  $ echo -ne "foo\n\n" > extra_newline

  $ patdiff.exe -default with_newline with_newline

  $ patdiff.exe -default missing_newline missing_newline
  No newline at the end of missing_newline
  No newline at the end of missing_newline

  $ patdiff.exe -default missing_newline missing_newline -warn-if-no-trailing-newline false

  $ patdiff.exe -default missing_newline with_newline
  No newline at the end of missing_newline

  $ patdiff.exe -default missing_newline with_newline -warn-if-no-trailing-newline false
  No newline at the end of missing_newline

  $ patdiff.exe -default -alt-old missing_newline -alt-new extra_newline missing_newline extra_newline | visible_colors
  No newline at the end of missing_newline
  (fg:red)------ (+bold)missing_newline
  (fg:green)++++++ (+bold)extra_newline
  (fg:black)@|(+bold)-1,1 +1,2(off) ============================================================
  (fg:black) |(off)foo
  (fg:black bg:green)+|
