Test that we ignore or consider whitespace as directed.

  $ cat - > prev <<EOF
  > this is a file with whitespace variously applied hg across the lines in an arbitrary manner
  > EOF

  $ cat - > next <<EOF
  > 
  >  this is  a file 	with  	  whitespace
  >  variously  applied
  > hg
  > across the
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > lines in an arbitrary
  > manner
  > EOF

Expect that we ignore whitespace changes.

  $ patdiff -default prev next

Expect no spurious diffs.

  $ patdiff -default next next -keep-whitespace

Expect whitespace changes should be detected.

  $ patdiff -default prev next -keep-whitespace | visible_colors
  (fg:red)------ (fg:default +bold)prev(-weight)
  (fg:green)++++++ (fg:default +bold)next(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,17(-weight) ============================================================
  (bg:red fg:black)-|(off fg:gray-12)this is(+invert fg:red) (-invert fg:gray-12)a file(+invert fg:red) (-invert fg:gray-12)with(+invert fg:red) (-invert fg:gray-12)whitespace(+invert fg:red) (-invert fg:gray-12)variously(+invert fg:red) (-invert fg:gray-12)applied(+invert fg:red) (-invert fg:gray-12)hg(+invert fg:red) (-invert fg:gray-12)across the(+invert fg:red) (-invert fg:gray-12)lines in an arbitrary(+invert fg:red) (-invert fg:gray-12)manner(fg:default)
  (bg:green fg:black)+|(off)
  (bg:green fg:black)+|(off +invert fg:green) (-invert fg:default)this is(+invert fg:green)  (-invert fg:default)a file(+invert fg:green) 	(-invert fg:default)with(+invert fg:green)  	  (-invert fg:default)whitespace
  (bg:green fg:black)+|(off +invert fg:green) (-invert fg:default)variously(+invert fg:green)  (-invert fg:default)applied
  (bg:green fg:black)+|(off)hg
  (bg:green fg:black)+|(off)across the
  (bg:green fg:black)+|(off)
  (bg:green fg:black)+|(off)
  (bg:green fg:black)+|(off)
  (bg:green fg:black)+|(off)
  (bg:green fg:black)+|(off)
  (bg:green fg:black)+|(off)
  (bg:green fg:black)+|(off)
  (bg:green fg:black)+|(off)
  (bg:green fg:black)+|(off)
  (bg:green fg:black)+|(off)
  (bg:green fg:black)+|(off)lines in an arbitrary
  (bg:green fg:black)+|(off)manner

Note that some whitespace changes are still ignored, just not those involving substituting a newline for a space.

  $ cat - > prev <<EOF
  > this is a file with whitespace but no newlines
  > EOF

  $ cat - > next <<EOF
  >  this is  a file 	with  	  whitespace    but           no newlines  
  > EOF


  $ patdiff -default prev next

Create some python files with the .py extension
  $ cat - > prev.py <<EOF
  > print("hello")
  > EOF
  $ cat - > next.py <<EOF
  > if True:
  >   print("hello")
  > EOF
  $ patdiff prev.py next.py | visible_colors
  (fg:red)------ (fg:default +bold)prev.py(-weight)
  (fg:green)++++++ (fg:default +bold)next.py(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,1 +1,2(-weight) ============================================================
  (bg:yellow fg:black)!|(bg:default fg:green)if True:(fg:default)
  (bg:yellow fg:black)!|(bg:default +invert fg:green)  (-invert fg:default)print("hello")

Create some python files that get detected with the shebang
  $ cat - > prev <<EOF
  > #!/usr/bin/python
  > print("hello")
  > EOF
  $ cat - > next <<EOF
  > #!/usr/bin/python
  > if True:
  >   print("hello")
  > EOF
  $ patdiff prev next | visible_colors
  (fg:red)------ (fg:default +bold)prev(-weight)
  (fg:green)++++++ (fg:default +bold)next(-weight)
  (bg:gray fg:black)@|(bg:default fg:default +bold)-1,2 +1,3(-weight) ============================================================
  (bg:gray fg:black) |(bg:default fg:default)#!/usr/bin/python
  (bg:yellow fg:black)!|(bg:default fg:green)if True:(fg:default)
  (bg:yellow fg:black)!|(bg:default +invert fg:green)  (-invert fg:default)print("hello")
