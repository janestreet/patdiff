open! Core
open! Import

module Range = Patience_diff.Range
module Hunk = Patience_diff.Hunk
module Hunks = Patience_diff.Hunks

module String_with_floats = struct

  type t =
    { floats         : float array
    ; without_floats : string
    }
  [@@deriving sexp]

  let close_enough tolerance =
    let equal f f' =
      Float.(<=)
        (Float.abs (f -. f'))
        (Percent.apply tolerance (Float.min (Float.abs f) (Float.abs f')))
    in
    stage (fun t t' ->
      String.(=) t.without_floats t'.without_floats
      && Array.equal ~equal t.floats t'.floats)
  ;;

  let float_regex = lazy
    (let prefix = "(^|[ ;:,(){}\\[\\]<>=\\+\\-\\*/$])" in
     let suffix = "($|[ ;:,(){}\\[\\]<>=\\+\\-\\*/%]|(bp|s|m|ms)($|[^0-9a-zA-Z_]))" in
     Re2.create_exn (prefix ^ "([\\-]?[0-9]+(\\.[0-9]+)?)" ^ suffix))
  ;;

  let create s =
    let rec aux floats line =
      match Re2.find_first ~sub:(`Index 2) (force float_regex) line with
      | Error _ ->
        { floats         = Array.of_list (List.rev floats)
        ; without_floats = line
        }
      | Ok s ->
        let line =
          Re2.replace_exn (force float_regex) line ~only:0 ~f:(fun re2_match ->
            let prefix = Re2.Match.get_exn re2_match ~sub:(`Index 1) in
            let suffix = Re2.Match.get_exn re2_match ~sub:(`Index 4) in
            prefix ^ suffix)
        in
        aux (Float.of_string s :: floats) line
    in
    aux [] s
  ;;

  include struct
    open! Async
    open! Expect_test_helpers

    let%expect_test _ =
      let prev = create "(dynamic (Ok ((price_range (-18.8305 39.1095)))))\n" in
      let next = create "(dynamic (Ok ((price_range (-18.772 38.988)))))\n"   in
      print_s [%message (prev : t) (next : t)];
      [%expect {|
        ((prev (
           (floats (-18.8305 39.1095))
           (without_floats "(dynamic (Ok ((price_range ( )))))\n")))
         (next (
           (floats (-18.772 38.988))
           (without_floats "(dynamic (Ok ((price_range ( )))))\n")))) |}]
    ;;

    let%expect_test _ =
      let prev = create "(primary_exchange_core_session (09:30:00.000000 16:00:00.000000))" in
      let next = create "(primary_exchange_core_session (09:30:00.000000 15:59:00.000000))" in
      print_s [%message (prev : t) (next : t)];
      [%expect {|
        ((prev (
           (floats (9 30 0 16 0 0))
           (without_floats "(primary_exchange_core_session (:: ::))")))
         (next (
           (floats (9 30 0 15 59 0))
           (without_floats "(primary_exchange_core_session (:: ::))")))) |}]
  end

end

(* If [a = needleman_wunsch xs ys], [a.(i).(j)] is the Levenshtein distance between the
   first [i] elts of [xs] and the first [j] elts of [ys]. This corresponds to
   Needleman-Wunsch where matches are 0, and mismatch, insert, and delete are all 1. *)
let needleman_wunsch xs ys ~equal =
  let min3 a b c = Int.min (Int.min a b) c in
  let rows = Array.length xs in
  let cols = Array.length ys in
  let a =
    let rows = rows + 1 in
    let cols = cols + 1 in
    Array.init rows ~f:(fun _ -> Array.create ~len:cols Int.max_value)
  in
  for i = 0 to rows do
    a.(i).(0) <- i;
  done;
  for j = 0 to cols do
    a.(0).(j) <- j;
  done;
  for i = 1 to rows do
    for j = 1 to cols do
      a.(i).(j) <-
        min3
          (a.(i-1).(j) + 1)
          (a.(i).(j-1) + 1)
          (a.(i-1).(j-1) + (if equal xs.(i-1) ys.(j-1)
                            then 0
                            else 1));
    done;
  done;
  a
;;

(* [recover_ranges a xs ys] does the traceback step of Needleman-Wunsch to find the
   lowest-scoring [Range.t list] that transforms [xs] into [ys]. *)
let recover_ranges xs ys a =
  (* index of smallest element in triple, with ties going to the element with higher
     index *)
  let smallest a b c =
    if a < b
    then if a < c
      then 0
      else 2
    else if b < c
    then 1
    else 2
  in
  let cons_minus_one car cdr ~if_unequal_to =
    if car = if_unequal_to
    then cdr
    else car - 1 :: cdr
  in
  let rec traceback a i j acc =
    if i <= 0 || j <= 0
    then acc
    else
      let i', j', matched =
        match smallest a.(i-1).(j) a.(i-1).(j-1) a.(i).(j-1) with
        | 0 -> (i-1, j, false)
        | 1 -> (i-1, j-1, a.(i).(j) = a.(i-1).(j-1))
        | 2 -> (i, j-1, false)
        | _ ->
          failwith "smallest only returns 0, 1, or 2."
      in
      let acc =
        match matched, acc with
        | true, [] | true, Second _ :: _ ->
          First [ i-1, j-1 ] :: acc
        | false, [] | false, First _ :: _ ->
          Second (cons_minus_one i [] ~if_unequal_to:i',
                  cons_minus_one j [] ~if_unequal_to:j')
          :: acc
        | true, First ijs :: acc ->
          First ((i-1, j-1) :: ijs) :: acc
        | false, Second (is, js) :: acc ->
          Second (cons_minus_one i is ~if_unequal_to:i',
                  cons_minus_one j js ~if_unequal_to:j')
          :: acc
      in
      traceback a i' j' acc
  in
  let elts_of_indices is xs = Array.of_list is |> Array.map ~f:(Array.get xs) in
  traceback a (Array.length xs) (Array.length ys) []
  |> List.map ~f:(function
    | First ijs ->
      let xys = Array.of_list ijs |> Array.map ~f:(fun (i, j) -> xs.(i), ys.(j)) in
      Range.Same xys
    | Second (is, []) ->
      Old (elts_of_indices is xs)
    | Second ([], js) ->
      New (elts_of_indices js ys)
    | Second (is, js) ->
      Replace (elts_of_indices is xs, elts_of_indices js ys))
;;

let do_tolerance ~equal hunks =
  Hunks.concat_map_ranges hunks ~f:(fun range ->
    match (range : string Range.t) with
    | Same _ | Old _ | New _ ->
      [ range ]
    | Unified _ ->
      raise_s [%message
        "Unexpected Unified range."
          ~_:(range : string Range.t)]
    | Replace (prev, next) ->
      needleman_wunsch
        (Array.map prev ~f:String_with_floats.create)
        (Array.map next ~f:String_with_floats.create)
        ~equal
      |> recover_ranges prev next)
;;

module Context_limit : sig
  val enforce : context:int -> string Hunk.t -> string Hunk.t list
end = struct

  module Merged_with_position : sig
    module Position : sig
      type t = Start | Middle | End [@@deriving sexp_of]
    end
    type t = string Range.t * Position.t [@@deriving sexp_of]
    val f : string Range.t list -> t Sequence.t
  end = struct
    module Position = struct
      type t =
        | Start
        | Middle
        | End
      [@@deriving sexp_of]
    end
    open Position

    type t = string Range.t * Position.t [@@deriving sexp_of]

    let f = function
      | [] -> Sequence.empty
      | car :: cdr ->
        Sequence.unfold_with_and_finish
          (Sequence.of_list cdr : string Range.t Sequence.t)
          ~init:(car, Start)
          ~running_step:(fun (car, pos) cadr ->
            match car, cadr with
            | Same car_lines, Same cadr_lines ->
              Skip (Same (Array.concat [ car_lines; cadr_lines ]), pos)
            | Unified _, _ | _, Unified _ ->
              raise_s [%message
                "Unexpected unified range."
                  (car : string Range.t)
                  (cadr : string Range.t)]
            | (Old _ | New _ | Replace _), (Old _ | New _ | Replace _)
            | Same _, (Old _ | New _ | Replace _)
            | (Old _ | New _ | Replace _), Same _ ->
              Yield ((car, pos), (cadr, Middle)))
          ~inner_finished:(fun (last, pos) ->
            match last, pos with
            | Unified _, _ ->
              raise_s [%message "Unexpected unified range." ~_:(last : string Range.t)]
            | _, End ->
              raise_s [%message "Produced End in running step." (last : string Range.t)]
            | Same _, Start ->
              None
            | (Old _ | New _ | Replace _), (Start | Middle)
            | Same _, Middle ->
              Some (last, End))
          ~finishing_step:(function
            | None ->
              Done
            | Some result ->
              Yield (result, None))
    ;;

    include struct
      open Async
      open Expect_test_helpers

      let%expect_test _ =
        let test ranges = print_s [%sexp (f ranges : t Sequence.t)] in
        let same = Range.Same [| "same", "same" |] in
        let not_same = Range.New [| "new" |] in
        test [ same; same ];
        let%bind () = [%expect {| () |}] in
        test [ same; not_same; same; same; not_same; same; same ];
        let%bind () = [%expect {|
        (((Same ((same same))) Start)
         ((New (new)) Middle)
         ((Same (
            (same same)
            (same same)))
          Middle)
         ((New (new)) Middle)
         ((Same (
            (same same)
            (same same)))
          End)) |}] in
        [%expect {| |}]
      ;;

    end
  end

  module Drop_or_keep : sig
    type t =
      | Drop of int   (* drop n lines of extra context *)
      | Keep of string Range.t
    [@@deriving sexp_of]

    val f : context:int -> Merged_with_position.t Sequence.t -> t Sequence.t
  end = struct
    type t =
      | Drop of int
      | Keep of string Range.t
    [@@deriving sexp_of]

    let drop_from_start context lines =
      let extra_context = Array.length lines - context in
      if extra_context <= 0
      then Sequence.singleton (Keep (Same lines))
      else Sequence.of_list
             [ Drop extra_context
             ; Keep (Same (Array.sub ~pos:extra_context ~len:context lines))
             ]
    ;;

    let drop_from_end context lines =
      let extra_context = Array.length lines - context in
      if extra_context <= 0
      then Sequence.singleton (Keep (Same lines))
      else Sequence.singleton (Keep (Same (Array.sub ~pos:0 ~len:context lines)))
    ;;

    let drop_from_middle context lines =
      let extra_context = Array.length lines - 2 * context in
      if extra_context <= 0
      then Sequence.singleton (Keep (Same lines))
      else
        let start_next_context_at = Array.length lines - context in
        Sequence.of_list
          [ Keep (Same (Array.sub ~pos:0 ~len:context lines))
          ; Drop extra_context
          ; Keep (Same (Array.sub ~pos:start_next_context_at ~len:context lines))
          ]
    ;;

    let f ~context (ranges : Merged_with_position.t Sequence.t) =
      Sequence.bind ranges ~f:(fun (range, pos) ->
        match range with
        | Unified _ ->
          raise_s [%message "Unexpected Unified range." ~_:(range : string Range.t)]
        | Old _ | New _ | Replace _ ->
          Sequence.singleton (Keep range)
        | Same lines ->
          match pos with
          | Start  -> drop_from_start  context lines
          | End    -> drop_from_end    context lines
          | Middle -> drop_from_middle context lines)
    ;;

    include struct
      open Async
      open Expect_test_helpers

      let%expect_test _ =
        let test ranges =
          print_s [%sexp (Merged_with_position.f ranges |> f ~context:1 : t Sequence.t)]
        in
        let same = Range.Same [| "same", "same" |] in
        let not_same = Range.New [| "new" |] in
        test [ same; same; ];
        let%bind () = [%expect {| () |}] in
        test [ same; same
             ; not_same
             ; same; same
             ; not_same
             ; same; same; same
             ; not_same
             ; same; same;
             ];
        let%bind () = [%expect {|
        ((Drop 1)
         (Keep (Same ((same same))))
         (Keep (New (new)))
         (Keep (
           Same (
             (same same)
             (same same))))
         (Keep (New (new)))
         (Keep (Same ((same same))))
         (Drop 1)
         (Keep (Same ((same same))))
         (Keep (New (new)))
         (Keep (Same ((same same))))) |}] in
        [%expect {| |}]
      ;;

    end

  end

  module Reconstruct_hunk : sig
    val f
      :  mine_start:int
      -> other_start:int
      -> Drop_or_keep.t Sequence.t
      -> string Hunk.t Sequence.t
  end = struct
    type t =
      { mine_start : int
      ; other_start : int
      ; ranges : string Range.t list
      }
    [@@deriving sexp_of]

    let to_hunk t =
      { Hunk.
        mine_start = t.mine_start
      ; mine_size = List.sum (module Int) t.ranges ~f:Range.mine_size
      ; other_start = t.other_start
      ; other_size = List.sum (module Int) t.ranges ~f:Range.other_size
      ; ranges = List.rev t.ranges
      }

    let f ~mine_start ~other_start drop_or_keeps =
      Sequence.unfold_with_and_finish
        drop_or_keeps
        ~init:{ mine_start; other_start; ranges = [] }
        ~running_step:(fun t drop_or_keep ->
          match (drop_or_keep : Drop_or_keep.t) with
          | Keep range ->
            Skip { t with ranges = range :: t.ranges }
          | Drop n ->
            let hunk = to_hunk t in
            let t =
              { mine_start  = t.mine_start  + hunk.mine_size  + n
              ; other_start = t.other_start + hunk.other_size + n
              ; ranges = []
              }
            in
            if List.is_empty (Hunk.ranges hunk)
            then Skip t
            else Yield (hunk, t))
        ~inner_finished:(fun t ->
          if List.is_empty t.ranges
          then None
          else Some t)
        ~finishing_step:(function
          | None -> Done
          | Some t -> Yield (to_hunk t, None))
    ;;

  end

  let enforce ~context hunk =
    Merged_with_position.f (Hunk.ranges hunk)
    |> Drop_or_keep.f ~context
    |> Reconstruct_hunk.f
         ~mine_start:(Hunk.mine_start hunk)
         ~other_start:(Hunk.other_start hunk)
    |> Sequence.to_list
  ;;

end

let apply hunks tolerance ~context =
  let equal = unstage (String_with_floats.close_enough tolerance) in
  do_tolerance ~equal hunks
  |> List.concat_map ~f:(Context_limit.enforce ~context)
;;
