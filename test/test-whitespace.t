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

  $ patdiff.exe -default prev next

Expect no spurious diffs.

  $ patdiff.exe -default next next -keep-whitespace

Expect whitespace changes should be detected.

  $ patdiff.exe -default prev next -keep-whitespace | visible_colors
  (fg:red)------ (+bold)prev
  (fg:green)++++++ (+bold)next
  (fg:black)@|(+bold)-1,1 +1,17(off) ============================================================
  (fg:black bg:red)-|(off)this is(fg:red) (off)a file(fg:red) (off)with(fg:red) (off)whitespace(fg:red) (off)variously(fg:red) (off)applied(fg:red) (off)hg(fg:red) (off)across the(fg:red) (off)lines in an arbitrary(fg:red) (off)manner
  (fg:black bg:green)+|
  (fg:black bg:green)+|(fg:green) (off)this is(fg:green)  (off)a file(fg:green) 	(off)with(fg:green)  	  (off)whitespace
  (fg:black bg:green)+|(fg:green) (off)variously(fg:green)  (off)applied
  (fg:black bg:green)+|(off)hg
  (fg:black bg:green)+|(off)across the
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|
  (fg:black bg:green)+|(off)lines in an arbitrary
  (fg:black bg:green)+|(off)manner

Note that some whitespace changes are still ignored, just not those involving substituting a newline for a space.

  $ cat - > prev <<EOF
  > this is a file with whitespace but no newlines
  > EOF

  $ cat - > next <<EOF
  >  this is  a file 	with  	  whitespace    but           no newlines  
  > EOF


  $ patdiff.exe -default prev next

