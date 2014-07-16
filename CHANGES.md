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

