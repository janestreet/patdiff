open! Core_kernel
open Expect_test_helpers_base
open Patdiff_kernel

let patdiff ?context ?keep_ws string1 string2 =
  Patdiff_core.Without_unix.patdiff (* turns off ANSI codes for color *)
    ~output:
      Ascii (* without color, cannot produce the "!|" lines that mix add/keep/remove *)
    ~produce_unified_lines:
      false (* line splitting produces confusing output in ASCII format *)
    ~split_long_lines:false
    ?context
    ?keep_ws
    ~prev:{ name = "a"; text = string1 }
    ~next:{ name = "b"; text = string2 }
    ()
;;

let patdiff_s ?context ?keep_ws sexp1 sexp2 =
  patdiff ?context ?keep_ws (sexp_to_string sexp1) (sexp_to_string sexp2)
;;

let print_endline_if_non_empty = function
  | "" -> ()
  | string -> print_endline string
;;

let print_patdiff ?context ?keep_ws string1 string2 =
  print_endline_if_non_empty (patdiff ?context ?keep_ws string1 string2)
;;

let print_patdiff_s ?context ?keep_ws sexp1 sexp2 =
  print_endline_if_non_empty (patdiff_s ?context ?keep_ws sexp1 sexp2)
;;

let diff_printer ?context ?keep_ws initial =
  let print =
    let previous = ref None in
    fun current ->
      (match !previous with
       | None -> print_endline current
       | Some previous -> print_patdiff ?context ?keep_ws previous current);
      previous := Some current
  in
  let () = Option.iter initial ~f:print in
  stage print
;;

let diff_printer_s ?context ?keep_ws initial =
  let diff_printer =
    unstage (diff_printer ?context ?keep_ws (Option.map ~f:sexp_to_string initial))
  in
  stage (fun sexp -> diff_printer (sexp_to_string sexp))
;;
