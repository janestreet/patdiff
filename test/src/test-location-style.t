Test omake-style locations needed for emacs to jump to test failures.
 
  $ cat - > old <<EOF
  > mary had a little lamb
  > its fleece was white as snow
  > hello world
  > bar
  > baz
  > EOF
 
  $ cat - > new <<EOF
  > mary had a little lamb
  > its fleece was white as snow
  > hello
  > bar
  > EOF
 
  $ patdiff -location-style omake old new | visible_colors
  (fg:red)------ (fg:default +bold)old(-weight)
  (fg:green)++++++ (fg:default +bold)new(-weight)
  File "old", line 3, characters 0-1:
  (bg:gray fg:black) |(bg:default fg:default)mary had a little lamb
  (bg:gray fg:black) |(bg:default fg:default)its fleece was white as snow
  (bg:yellow fg:black)!|(bg:default fg:default)hello(fg:red) world(fg:default)
  (bg:gray fg:black) |(bg:default fg:default)bar
  (bg:red fg:black)-|(bg:default fg:red)baz(fg:default)

Test omitting line numbers

  $ patdiff old new | visible_colors
  (fg:red)------ (fg:default +bold)old(-weight)
  (fg:green)++++++ (fg:default +bold)new(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,5 +1,4(-weight) ============================================================
  (bg:gray fg:black) |(bg:default fg:default)mary had a little lamb
  (bg:gray fg:black) |(bg:default fg:default)its fleece was white as snow
  (bg:yellow fg:black)!|(bg:default fg:default)hello(fg:red) world(fg:default)
  (bg:gray fg:black) |(bg:default fg:default)bar
  (bg:red fg:black)-|(bg:default fg:red)baz(fg:default)

  $ patdiff -location-style none old new | visible_colors
  (fg:red)------ (fg:default +bold)old(-weight)
  (fg:green)++++++ (fg:default +bold)new(-weight)
  (bg:gray fg:black)@|(bg:default fg:default -weight) ============================================================
  (bg:gray fg:black) |(bg:default fg:default)mary had a little lamb
  (bg:gray fg:black) |(bg:default fg:default)its fleece was white as snow
  (bg:yellow fg:black)!|(bg:default fg:default)hello(fg:red) world(fg:default)
  (bg:gray fg:black) |(bg:default fg:default)bar
  (bg:red fg:black)-|(bg:default fg:red)baz(fg:default)
