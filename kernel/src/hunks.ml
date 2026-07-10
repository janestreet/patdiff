open! Core
open! Import

type t = string Patience_diff.Hunk.t list [@@deriving sexp_of]

let iter_sides ~f_hunk_break ~f_line (hunks : _ Patience_diff.Hunk.t list) =
  List.iter hunks ~f:(fun hunk ->
    f_hunk_break hunk;
    let prev_line = ref hunk.prev_start in
    let next_line = ref hunk.next_start in
    let emit kind =
      let side : Format.Annotation_gutter.side =
        match kind with
        | `Prev -> Prev { line = !prev_line }
        | `Next -> Next { line = !next_line }
        | `Same -> Same { prev = !prev_line; next = !next_line }
        | `Unified -> Unified { prev = !prev_line; next = !next_line }
      in
      f_line side;
      match kind with
      | `Prev -> Int.incr prev_line
      | `Next -> Int.incr next_line
      | `Same | `Unified ->
        Int.incr prev_line;
        Int.incr next_line
    in
    List.iter hunk.ranges ~f:(function
      | Same r -> Array.iter r ~f:(fun _ -> emit `Same)
      | Prev (r, _) -> Array.iter r ~f:(fun _ -> emit `Prev)
      | Next (r, _) -> Array.iter r ~f:(fun _ -> emit `Next)
      | Unified (r, _) -> Array.iter r ~f:(fun _ -> emit `Unified)
      | Replace (ar1, ar2, _) ->
        Array.iter ar1 ~f:(fun _ -> emit `Prev);
        Array.iter ar2 ~f:(fun _ -> emit `Next)))
;;

let iter_with_line_info ~f_hunk_break ~f_line (hunks : t) =
  List.iter hunks ~f:(fun hunk ->
    f_hunk_break hunk;
    let prev_line = ref hunk.prev_start in
    let next_line = ref hunk.next_start in
    let emit kind text =
      let side : Format.Annotation_gutter.side =
        match kind with
        | `Prev -> Prev { line = !prev_line }
        | `Next -> Next { line = !next_line }
        | `Same -> Same { prev = !prev_line; next = !next_line }
        | `Unified -> Unified { prev = !prev_line; next = !next_line }
      in
      f_line side text;
      match kind with
      | `Prev -> Int.incr prev_line
      | `Next -> Int.incr next_line
      | `Same | `Unified ->
        Int.incr prev_line;
        Int.incr next_line
    in
    List.iter hunk.ranges ~f:(function
      | Same r -> Array.iter r ~f:(fun (_annotation, text) -> emit `Same text)
      | Prev (r, _) -> Array.iter r ~f:(emit `Prev)
      | Next (r, _) -> Array.iter r ~f:(emit `Next)
      | Unified (r, _) -> Array.iter r ~f:(emit `Unified)
      | Replace (ar1, ar2, _) ->
        Array.iter ar1 ~f:(emit `Prev);
        Array.iter ar2 ~f:(emit `Next)))
;;

let iter' ~f_hunk_break ~f_line hunks =
  iter_with_line_info ~f_hunk_break ~f_line:(fun _side line -> f_line line) hunks
;;
