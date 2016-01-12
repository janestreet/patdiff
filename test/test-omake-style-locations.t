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
  [off][red]------ [off][off][bold]old[off]
  [off][green]++++++ [off][off][bold]new[off]
  File "old", line 3, characters 0-1:
  [off][high-intensity:bg:black][black] |[off]mary had a little lamb
  [off][high-intensity:bg:black][black] |[off]its fleece was white as snow
  [off][bg:yellow][black]!|[off][off]hello [off][red]world[off][off]
  [off][high-intensity:bg:black][black] |[off]bar
  [off][bg:red][black]-|[off][off][red]baz[off]
