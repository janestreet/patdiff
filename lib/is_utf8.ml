open! Core
open! Import
open! Int.Replace_polymorphic_compare

module Byte_class = struct
  type t =
    | Ascii
    | Continuation
    | Seq_2
    | Seq_3
    | Seq_4
    | Invalid
  [@@deriving sexp_of]

  let create string index =
    match String.get string index with
    | '\x00' .. '\x7F' -> Ascii
    | '\x80' .. '\xBF' -> Continuation
    | '\xC0' .. '\xDF' -> Seq_2
    | '\xE0' .. '\xEF' -> Seq_3
    | '\xF0' .. '\xF4' -> Seq_4
    | '\xF5' .. '\xFF' -> Invalid
  ;;
end

(* How many continuation bytes to expect *)
type t =
  | Expect_0
  | Expect_1
  | Expect_2
  | Expect_3
[@@deriving sexp_of]

let pred_exn t =
  match t with
  | Expect_0 -> raise_s [%message "No predecessor." (t : t)]
  | Expect_1 -> Expect_0
  | Expect_2 -> Expect_1
  | Expect_3 -> Expect_2
;;

(* UTF-8 is a byte-based format which loosely has the following classes of bytes:

   00-7F: normal ASCII (call this class 'A' for short)
   80-BF: continuation bytes (call this class 'C')
   C0-DF: 2-byte sequence leading bytes (call this class '2')
   E0-EF: 3-byte sequence leading bytes (call this class '3')
   F0-F4: 4-byte sequence leading bytes (call this class '4')
   F5-FF: invalid in UTF-8

   Valid UTF-8 consists of any combination of the byte-sequences A, 2C, 3CC, 4CCC. There
   are extra constraints which we do not check here (this condition is necessary but not
   sufficient).

   One of the properties of UTF-8 is that it is self-synchronizing, i.e. corruption of the
   data has local effect, and cannot affect non-adjacent valid byte-sequences, regardless
   of substitution/insertion/deletion.

   We compute here a loose upper bound on the number of single-byte substitutions to turn
   the input line into valid UTF-8. Simplistically, we only keep track of how many
   continuation bytes we expect, and whenever we didn't get the byte class we expected,
   summarily change it to what we did expect, then continue.

   The worst case is if the input turns '4CCC' into 'CCCC', which is clearly one error,
   but our algorithm counts 4 unexpected Cs, for a factor of 4. 'AAAA' -> '4AAA' counts 3
   unexpected As instead of 1 error. '4CCC' -> 'ACCC' counts 3 errors.
*)
let rec rough_substitutions_to_utf8 t acc line index =
  match index >= String.length line with
  | true  -> acc
  | false ->
    let t, errors =
      match t, Byte_class.create line index with
      | Expect_0, Ascii -> Expect_0, 0
      | Expect_0, Seq_2 -> Expect_1, 0
      | Expect_0, Seq_3 -> Expect_2, 0
      | Expect_0, Seq_4 -> Expect_3, 0
      | Expect_0, (Continuation | Invalid) -> Expect_0, 1 (* substitute A for C/I byte *)
      | (Expect_1 | Expect_2 | Expect_3), Continuation ->
        pred_exn t, 0
      | (Expect_1 | Expect_2 | Expect_3), (Ascii | Seq_2 | Seq_3 | Seq_4 | Invalid) ->
        pred_exn t, 1  (* substitute C for non-C byte *)
    in
    rough_substitutions_to_utf8 t (acc + errors) line (index + 1)
;;

(* We allow either 4 errors or 4% error, whichever is greater, relative to UTF-8 by the
   simple factor-of-4 upper bound count. Failure of this condition means at least 2
   distinct errors exist in the file, and the fraction of corrupted bytes is at least
   1%. This is probably enough to claim a file isn't UTF-8. *)
let clearly_not_utf8 ~lines =
  let length, errors =
    Array.fold ~init:(0, 0) lines ~f:(fun (length, errors) line ->
      let errors_this_line = rough_substitutions_to_utf8 Expect_0 0 line 0 in
      length + String.length line + 1, errors + errors_this_line)
  in
  errors >= 5 || errors > length / 25

let%test_module "clearly_not_utf8" = (module struct
  module Gen = Quickcheck.Generator

  let bounded_char_gen lo hi = Int.gen_incl lo hi |> Gen.map ~f:Char.of_int_exn

  let uniform_char_gen      = bounded_char_gen 0x00 0xFF
  let ascii_char_gen        = bounded_char_gen 0x00 0x7F
  let utf8_leading_byte_gen = bounded_char_gen 0x00 0xF4
  let continuation_byte_gen = bounded_char_gen 0x80 0xBF
  let post_E0_byte_gen      = bounded_char_gen 0xA0 0xBF
  let post_F0_byte_gen      = bounded_char_gen 0x90 0xBF
  let post_F4_byte_gen      = bounded_char_gen 0x80 0x8F
  let post_ED_byte_gen      = bounded_char_gen 0x80 0x9F

  (* In case we ever actually care about strict UTF-8 validation, here are some further
     restrictions beyond those in [rough_substitutions_to_utf8]:

     - C0 and C1 are never valid UTF-8 bytes because they always generate overlong
       encodings. Note that 'modified' UTF-8 uses C0 80 as a deliberate overlong encoding
       for NULL.

     - E0 can generate overlong encodings, and so the next byte must be A0-BF (third byte
       is unrestricted)

     - F0 can also generate overlong encodings, and so the next byte must be 90-BF
       (3rd/4th bytes unrestricted)

     - F4 can generate encodings beyond the 10FFFF code point limit of unicode, and so the
       next byte must be 80-8F (3rd/4th bytes unrestricted)

     - ED can generate encodings of the D800-DFFF segment of code points, which are
       reserved for UTF-16 surrogate halves, and so the next byte must be 80-9F (3rd byte
       unrestricted). Note that 'wobbly' UTF-8 allows these code points, and so-called
       UTF-8B deliberately generates them to preserve information about errors in input.

     Our quickcheck here will generate strictly valid UTF-8.
  *)
  let rec create_utf8_char_list ~size state =
    if size <= 0 (* with no characters left, return empty list *)
    then []
    else (
      let leading = Gen.generate utf8_leading_byte_gen ~size:0 state in
      match leading with
      | '\x00' .. '\x7F' -> (* pure ASCII *)
        leading :: (create_utf8_char_list ~size:(size - 1) state)
      | '\x80' .. '\xC1' -> (* invalid continuation byte or 2-byte leader, reject *)
        create_utf8_char_list ~size state
      | '\xC2' .. '\xDF' -> (* 2-byte sequence leading byte *)
        if size < 2 (* not enough space to generate 2-byte sequence, reject *)
        then create_utf8_char_list ~size state
        else (
          let cont = Gen.generate continuation_byte_gen ~size:0 state in
          leading :: cont :: (create_utf8_char_list ~size:(size - 2) state))
      | '\xE0' .. '\xEF' -> (* 3-byte sequence leading byte *)
        if size < 3 (* not enough space, reject *)
        then create_utf8_char_list ~size state
        else (
          let cont1_gen =
            match leading with
            | '\xE0' -> post_E0_byte_gen
            | '\xED' -> post_ED_byte_gen
            | _      -> continuation_byte_gen
          in
          let cont1 = Gen.generate cont1_gen             ~size:0 state in
          let cont2 = Gen.generate continuation_byte_gen ~size:0 state in
          leading :: cont1 :: cont2 :: (create_utf8_char_list ~size:(size - 3) state))
      | '\xF0' .. '\xF4' -> (* 4-byte sequence leading byte *)
        if size < 4 (* not enough space, reject *)
        then create_utf8_char_list ~size state
        else (
          let cont1_gen =
            match leading with
            | '\xF0' -> post_F0_byte_gen
            | '\xF4' -> post_F4_byte_gen
            | _      -> continuation_byte_gen
          in
          let cont1 = Gen.generate cont1_gen             ~size:0 state in
          let cont2 = Gen.generate continuation_byte_gen ~size:0 state in
          let cont3 = Gen.generate continuation_byte_gen ~size:0 state in
          leading :: cont1 :: cont2 :: cont3
          :: (create_utf8_char_list ~size:(size - 4) state))
      | _                -> create_utf8_char_list ~size state (* never reached *))

  let utf8_string_gen =
    Gen.create (fun ~size state ->
      (* Adapted from Core_kernel.String.For_quickcheck.default_length *)
      let upper_bound      = size + 1                                      in
      let weighted_low_gen = Int.gen_log_uniform_incl 0 upper_bound        in
      let weighted_low     = Gen.generate weighted_low_gen ~size:0 state   in
      let string_size      = upper_bound - weighted_low                    in

      let char_list        = create_utf8_char_list ~size:string_size state in
      String.of_char_list char_list)

  let%test_unit "purely ASCII strings pass" =
    let ascii_string_gen = String.gen' ascii_char_gen in
    Quickcheck.test ascii_string_gen ~f:(fun line ->
      if clearly_not_utf8 ~lines:[| line |]
      then raise_s [%message "ASCII string failed UTF-8 check" (line: string)])

  let%test_unit "sufficiently long uniformly random byte strings do not pass" =
    let bytestring_gen = String.gen_with_length 48 uniform_char_gen in
    Quickcheck.test bytestring_gen ~f:(fun line ->
      if not (clearly_not_utf8 ~lines:[| line |])
      then raise_s [%message "random string passed UTF-8 check" (line: string)])

  let%test_unit "actual UTF-8 strings pass" =
    Quickcheck.test utf8_string_gen ~f:(fun line ->
      if clearly_not_utf8 ~lines:[| line |]
      then raise_s [%message "generated UTF-8 string failed UTF-8 check" (line: string)])
end)

let%expect_test "box-drawing characters" =
  let lines = Array.of_list (String.split_lines "\
     ┌Signals──┐┌Values───┐┌Waves─────────────────────────────────────────┐
     │clock    ││         ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
     │         ││         ││    └───┘   └───┘   └───┘   └───┘   └───┘   └─│\n")
  in
  print_s [%sexp (clearly_not_utf8 ~lines : bool)];
  [%expect {| false |}]
;;
