Test recursive diff of directories.

  $ mkdir prev
  $ mkdir prev/foo
  $ echo . > prev/this-goes-away
  $ echo prev > prev/this-changes

  $ mkdir next
  $ echo . > next/foo
  $ echo . > next/this-appears
  $ echo next > next/this-changes


  $ patdiff.exe -default prev next | visible_colors
  Only in prev: this-goes-away (glob)
  [off][red]------ [off][off][bold]prev/this-goes-away[off]
  [off][green]++++++ [off][off][bold]/dev/null[off]
  [off][high-intensity:bg:black][black]@|[off][off][bold]-1,1 +1,0[off] ============================================================
  [off][bg:red][black]-|[off][off][red].[off]
  Only in next: this-appears (glob)
  [off][red]------ [off][off][bold]/dev/null[off]
  [off][green]++++++ [off][off][bold]next/this-appears[off]
  [off][high-intensity:bg:black][black]@|[off][off][bold]-1,0 +1,1[off] ============================================================
  [off][bg:green][black]+|[off][off][green].[off]
  Files prev/foo and next/foo are not the same type (glob)
  [off][red]------ [off][off][bold]prev/this-changes[off]
  [off][green]++++++ [off][off][bold]next/this-changes[off]
  [off][high-intensity:bg:black][black]@|[off][off][bold]-1,1 +1,1[off] ============================================================
  [off][bg:red][black]-|[off][off][off][dim][off][off][red]prev[off][off][dim][off][off]
  [off][bg:green][black]+|[off][off][off][green]next[off][off]
