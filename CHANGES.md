## Release v0.17.0

- Patdiff can now find lines that are moved from the first file to the new.
  If you pass the flag `-find-moves` patdiff will try to find similar ranges
  in render them. You may need to update your `~/.patdiff` config so it renders
  the move ranges correctly. `patdiff -make-config TEST_FILE` will write the
  default patdiff config to `TEST_FILE`. The parameters rules that have been added
  are the following `line_from_old`, `line_to_new`, `line_removed_in_move`, `line_added_in_move`,
  and `line_unified_in_move`.

  - line_from_old is a line that was moved TO somewhere else
  - line_to_new is a line that was moved FROM somewhere else
  - line_removed_in_move is a line that was deleted as part of a larger moved block
  - line_added_in_move is a line that was added as part of a larger moved block
  - line_unified_in_move is a line that slightly modified as part of a large moved block

- Can now specify the output to be Ascii from the ~/.patdiff config using `(output ascii)`

- `Patdiff_core` now exposes the `float_tolerance` paramater

- Fixed a bug where `split-long-lines` adds extra empty lines

- Address an issue where we find moves too aggressively

- Trim the edge of move blocks if the edge was strictly added or strictly removed

- Remove the dependency on Pcre

## git version

- Added a unit argument to `Patience_diff.S.get_hunks` and
  `Patience_diff.S.get_matching_blocks`, to ensure that the optional
  argument can be erased.

- Extract a Js\_of\_ocaml-compatible library called `Patdiff_kernel`.
  `Patdiff_lib` is renamed to simply `Patdiff`.

- Fixed detection heuristic for binary files to function correctly given UTF-8
  input.

## v0.11

- Switch from `Core_extended.Std.Deprecated_command` to `Core.Command`.

## 113.24.00

- `patdiff -location-style omake` should print the line number of the
  first difference in each hunk, skipping context lines.

- Switched to PPX.

- Added binding in patdiff to use the newly minted colors of Ansi\_terminal.  This
  will be used notably by patdiff4 to produce better ddiff.

  Also, have the module `Color` and `Style` implement and export `Comparable.S`.
  This is useful for example to dedup styles from a list of styles without relying
  on the polymorphic equality.

- Make it so that if you pass `-warn-if-no-trailing-newline-in-both false`
  then you get the warning only when one file has a trailing newline and
  the other file does not.

  If you pass `-warn-if-no-trailing-newline-in-both true` or omit this
  flag, then you get the current behavior of warning for each file
  independently.

- Patdiff's unified-tests currently render colors codes in angle
  brackets.  Change them to square brackets.  Square brackets are word
  boundaries, so we'll get more legible diffs when tests fail.

- Simple code change in patdiff to prepare more changes in patdiff4.  This change
  is a pure refactoring and has zero runtime change.  Just moving some functions
  around.

- patdiff_core.ml is a very long module.  start extracting module from it.  start
  with format.  in the process, expose in a private fashion the record `Rule.t`.

- Continue on splitting the file patdiff_core.ml into smaller pieces.
  In this version, we extract each output mode into its own file.

- Kill the generation of html diffs in patdiff.  There are good third party tools
  that can convert efficiently ansi texts to html directly.
  We plan on simplifying a bit the patdiff source code to increase its
  maintainability, and dropping the requirement of producing html output seems a
  step in the right direction.

  Some pointers:

  http://www.pixelbeat.org/scripts/ansi2html.sh

## 112.24.00

Minor update: doc.

## 112.17.00

- The call to Pcre.full_split in patdiff_core.ml rely on a bug of
  pcre-ocaml <= 7.1.2.

  To get the same behavior with pcre-ocaml >= 7.1.3 we need to pass
  ~max:(-1).

  See this bug for more details:

     https://github.com/mmottl/pcre-ocaml/issues/1

## 111.25.00

- add a `?file_names` argument to `Compare_core.diff_strings`

## 111.21.00

- Added `Patdiff_core.iter_ansi`.

        (** Iter along the lines of the diff and the breaks between hunks. Offers more flexibility
            regarding what the caller wants to do with the lines *)
        val iter_ansi
          :  f_hunk_break:((int*int) -> (int*int) -> unit)
          -> f_line:(string -> unit)
          -> string Patience_diff.Hunk.t list
          -> unit

## 111.17.00

- Removed latex output.

## 109.53.00

- Bump version number
