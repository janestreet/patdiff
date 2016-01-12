Test diffing against empty files.

  $ cat - > prev <<EOF
  > oneline
  > EOF

  $ next=$(mktemp)
  $ cat - > next <<EOF
  > EOF

Expect all red.

  $ patdiff.exe -default prev next | visible_colors
  [off][red]------ [off][off][bold]prev[off]
  [off][green]++++++ [off][off][bold]next[off]
  [off][high-intensity:bg:black][black]@|[off][off][bold]-1,1 +1,0[off] ============================================================
  [off][bg:red][black]-|[off][off][red]oneline[off]

Expect all green.

  $ patdiff.exe -default next prev | visible_colors
  [off][red]------ [off][off][bold]next[off]
  [off][green]++++++ [off][off][bold]prev[off]
  [off][high-intensity:bg:black][black]@|[off][off][bold]-1,0 +1,1[off] ============================================================
  [off][bg:green][black]+|[off][off][green]oneline[off]
