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

