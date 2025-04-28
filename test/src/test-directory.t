Test recursive diff of directories.

  $ mkdir prev
  $ mkdir prev/subdir
  $ mkdir prev/foo
  $ echo . > prev/foo/bar
  $ echo . > prev/this-goes-away
  $ echo prev > prev/this-changes
  $ echo prev > prev/subdir/this-changes-in-subdir

  $ mkdir next
  $ mkdir next/subdir
  $ echo . > next/foo
  $ echo . > next/this-appears
  $ echo next > next/this-changes
  $ echo next > next/subdir/this-changes-in-subdir

  $ patdiff -default prev next | visible_colors
  Only in prev: this-goes-away
  (fg:red)------ (fg:default +bold)prev/this-goes-away(-weight)
  (fg:green)++++++ (fg:default +bold)/dev/null(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,0(-weight) ============================================================
  (bg:red fg:black)-|(bg:default fg:red).(fg:default)
  Only in next: this-appears
  (fg:red)------ (fg:default +bold)/dev/null(-weight)
  (fg:green)++++++ (fg:default +bold)next/this-appears(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,0 +1,1(-weight) ============================================================
  (bg:green fg:black)+|(bg:default fg:green).(fg:default)
  Files prev/foo and next/foo are not the same type
  (fg:red)------ (fg:default +bold)prev/subdir/this-changes-in-subdir(-weight)
  (fg:green)++++++ (fg:default +bold)next/subdir/this-changes-in-subdir(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,1(-weight) ============================================================
  (bg:red fg:black)-|(off fg:red)prev(fg:default)
  (bg:green fg:black)+|(off fg:green)next(fg:default)
  (fg:red)------ (fg:default +bold)prev/this-changes(-weight)
  (fg:green)++++++ (fg:default +bold)next/this-changes(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,1(-weight) ============================================================
  (bg:red fg:black)-|(off fg:red)prev(fg:default)
  (bg:green fg:black)+|(off fg:green)next(fg:default)

Test behavior of -alt-prev and -alt-next.

  $ patdiff -default prev next -alt-prev a -alt-next b | visible_colors
  Only in a: this-goes-away
  (fg:red)------ (fg:default +bold)a/this-goes-away(-weight)
  (fg:green)++++++ (fg:default +bold)/dev/null(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,0(-weight) ============================================================
  (bg:red fg:black)-|(bg:default fg:red).(fg:default)
  Only in b: this-appears
  (fg:red)------ (fg:default +bold)/dev/null(-weight)
  (fg:green)++++++ (fg:default +bold)b/this-appears(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,0 +1,1(-weight) ============================================================
  (bg:green fg:black)+|(bg:default fg:green).(fg:default)
  Files a/foo and b/foo are not the same type
  (fg:red)------ (fg:default +bold)a/subdir/this-changes-in-subdir(-weight)
  (fg:green)++++++ (fg:default +bold)b/subdir/this-changes-in-subdir(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,1(-weight) ============================================================
  (bg:red fg:black)-|(off fg:red)prev(fg:default)
  (bg:green fg:black)+|(off fg:green)next(fg:default)
  (fg:red)------ (fg:default +bold)a/this-changes(-weight)
  (fg:green)++++++ (fg:default +bold)b/this-changes(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,1(-weight) ============================================================
  (bg:red fg:black)-|(off fg:red)prev(fg:default)
  (bg:green fg:black)+|(off fg:green)next(fg:default)

  $ rm -r prev; rm -r next
  $ mkdir dir
  $ echo x > dir/f
  $ echo y > f

  $ patdiff f dir/ |& grep Failure
    (Failure "dir/ is a directory, while f is a file")

  $ patdiff dir/f dir |& grep Failure
    (Failure "dir is a directory, while dir/f is a file")

  $ patdiff missing dir |& grep Failure
    (Failure "dir is a directory, while missing does not exist")
