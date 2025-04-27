Test diffing against empty files.

  $ cat - > prev <<EOF
  > oneline
  > EOF

  $ next=$(mktemp)
  $ cat - > next <<EOF
  > EOF

Expect all red.

  $ patdiff -default prev next | visible_colors
  (fg:red)------ (fg:default +bold)prev(-weight)
  (fg:green)++++++ (fg:default +bold)next(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,0(-weight) ============================================================
  (bg:red fg:black)-|(bg:default fg:red)oneline(fg:default)

Expect all green.

  $ patdiff -default next prev | visible_colors
  (fg:red)------ (fg:default +bold)next(-weight)
  (fg:green)++++++ (fg:default +bold)prev(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,0 +1,1(-weight) ============================================================
  (bg:green fg:black)+|(bg:default fg:green)oneline(fg:default)
