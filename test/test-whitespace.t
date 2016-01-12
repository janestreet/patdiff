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

Expect whitespace changes should be ignored.

  $ patdiff.exe -default prev next

Expect no spurious diffs.

  $ patdiff.exe -default next next -keep-whitespace

Expect whitespace changes should be detected.

  $ patdiff.exe -default prev next -keep-whitespace | visible_colors
  [off][red]------ [off][off][bold]prev[off]
  [off][green]++++++ [off][off][bold]next[off]
  [off][high-intensity:bg:black][black]@|[off][off][bold]-1,1 +1,17[off] ============================================================
  [off][bg:red][black]-|[off][off][off][dim]this is[off][off][red] [off][off][dim]a file[off][off][red] [off][off][dim]with[off][off][red] [off][off][dim]whitespace variously[off][off][red] [off][off][dim]applied[off][off][dim] hg across the lines in an arbitrary[off][off][dim] manner[off][off]
  [off][bg:green][black]+|[off][off] this is[off][green]  [off]a file[off][green] 	[off]with[off][green]  	  [off]whitespace[off]
  [off][bg:green][black]+|[off][off] variously[off][green]  [off]applied[off][green]	[off][off]
  [off][bg:green][black]+|[off][off]hg[off]
  [off][bg:green][black]+|[off][off]across the[off]
  [off][bg:green][black]+|[off][off][off]
  [off][bg:green][black]+|[off][off][off]
  [off][bg:green][black]+|[off][off][off]
  [off][bg:green][black]+|[off][off][off]
  [off][bg:green][black]+|[off][off][off]
  [off][bg:green][black]+|[off][off][off]
  [off][bg:green][black]+|[off][off][off]
  [off][bg:green][black]+|[off][off][off]
  [off][bg:green][black]+|[off][off][off]
  [off][bg:green][black]+|[off][off][off]
  [off][bg:green][black]+|[off][off]    lines in an arbitrary[off][green]     	[off][off]
  [off][bg:green][black]+|[off][off]manner[off]
