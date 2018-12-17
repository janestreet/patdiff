open! Core
open! Import
module Format = Patdiff_format
module Output = Output_mode

(* Strip whitespace from a string by stripping and replacing with spaces *)
let ws_rex = Re.compile Re.(rep1 space)
let ws_rex_anchored = Re.compile Re.(seq [ bol; rep space; eol ])
let ws_sub = " "
let remove_ws s = String.strip (Re.replace_string ws_rex s ~by:ws_sub)
let is_ws = Re.execp ws_rex_anchored

(* This regular expression describes the delimiters on which to split the string *)
let words_rex =
  let open Re in
  let delim = set {|"{}[]#,.;()_|} in
  let punct = rep1 (set {|=`+-/!@$%^&*:|}) in
  let space = rep1 space in
  (* We don't want to split up ANSI color sequences, so let's make sure they get through
     intact. *)
  let ansi_sgr_sequence =
    let esc = char '\027' in
    seq [ esc; char '['; rep (alt [ char ';'; digit ]); char 'm' ]
  in
  compile (alt [ delim; punct; space; ansi_sgr_sequence ])
;;

(* Split a string into a list of string options delimited by words_rex
   (delimiters included) *)
let split s ~keep_ws =
  let s = if keep_ws then s else String.rstrip s in
  if String.is_empty s && keep_ws
  then [ "" ]
  else
    Re.split_full words_rex s
    |> List.filter_map ~f:(fun token ->
      let string =
        match token with
        | `Delim d -> Re.Group.get d 0
        | `Text t -> t
      in
      if String.is_empty string then None else Some string)
;;

(* This function ensures that the tokens passed to Patience diff do not include
   whitespace.  Whitespace is appended to words, and then removed by [~transform] later
   on. The point is to make the semantic cleanup go well -- we don't want whitespace
   matches to "count" as part of the length of a match. *)
let whitespace_ignorant_split s =
  if String.is_empty s
  then []
  else (
    let istext s = not (Re.execp ws_rex s) in
    split s ~keep_ws:false
    |> List.group ~break:(fun split_result1 _ -> istext split_result1)
    |> List.map ~f:String.concat)
;;

include struct
  open Expect_test_helpers

  let%expect_test _ =
    print_s ([%sexp_of: string list] (split ~keep_ws:true ""));
    [%expect {| ("") |}]
  ;;
end

module type Output = Output_intf.S

module Ansi = Ansi_output
module Ascii = Ascii_output
module Html = Html_output

module Output_ops = struct
  module Rule = struct
    let apply text ~rule ~output ~refined =
      match (output : Output.t) with
      | Ansi -> Ansi.Rule.apply text ~rule ~refined
      | Ascii -> Ascii.Rule.apply text ~rule ~refined
      | Html -> Html.Rule.apply text ~rule ~refined
    ;;
  end

  module Rules = struct
    let to_string rules output =
      let module Rz = Format.Rules in
      let module R = Patience_diff.Range in
      let apply text ~rule ~refined = Rule.apply text ~rule ~output ~refined in
      function
      | R.Same ar ->
        let formatted_ar =
          Array.map ar ~f:(fun (x, y) ->
            let app = apply ~rule:rules.Rz.line_same ~refined:false in
            app x, app y)
        in
        R.Same formatted_ar
      | R.New ar ->
        R.New (Array.map ar ~f:(apply ~refined:false ~rule:rules.Rz.line_new))
      | R.Old ar ->
        R.Old (Array.map ar ~f:(apply ~refined:false ~rule:rules.Rz.line_old))
      | R.Unified ar ->
        R.Unified (Array.map ar ~f:(apply ~refined:true ~rule:rules.Rz.line_unified))
      | R.Replace (ar1, ar2) ->
        let ar1 = Array.map ar1 ~f:(apply ~refined:true ~rule:rules.Rz.line_old) in
        let ar2 = Array.map ar2 ~f:(apply ~refined:true ~rule:rules.Rz.line_new) in
        R.Replace (ar1, ar2)
    ;;

    let map_ranges hunks ~f =
      let f hunk =
        let module H = Patience_diff.Hunk in
        { hunk with H.ranges = List.map hunk.H.ranges ~f }
      in
      List.map hunks ~f
    ;;

    let apply hunks ~rules ~output = map_ranges hunks ~f:(to_string rules output)
  end

  let print ~print_global_header ~file_names ~rules ~output ~print ~location_style hunks =
    let formatted_hunks = Rules.apply ~rules ~output hunks in
    let f =
      match (output : Output.t) with
      | Ansi -> Ansi.print
      | Ascii -> Ascii.print
      | Html -> Html.print
    in
    f ~print_global_header ~file_names ~rules ~print ~location_style formatted_hunks
  ;;
end

(* Default amount of context shown around each change in the diff *)
let default_context = 16

(* The following constants were all chosen empirically. *)
(* Default cutoff for line-level semantic cleanup.  Any match of [default_line_big_enough]
   or more will not be deleted, even if it's surrounded by large inserts and deletes.
   Raising this quantity can only decrease the number of matches, and lowering it
   can only decrease the number of matches. *)
let default_line_big_enough = 3

(* Analogous to above, but for word-level refinement *)
let default_word_big_enough = 7

(* Governs the behavior of [split_for_readability].  We will only split ranges around
   matches of size greater than [too_short_to_split].  Note that this should always
   be at least 1, otherwise we will split on a single `Newline token.
   Raising this quantity will result in less ranges being split, and setting it to
   infinity is the same as passing in [~interleave:false]. *)
let too_short_to_split = 2

let diff ~context ~line_big_enough ~keep_ws ~mine ~other =
  let transform = if keep_ws then Fn.id else remove_ws in
  Patience_diff.String.get_hunks
    ~transform
    ~context
    ~big_enough:line_big_enough
    ~mine
    ~other
;;

type word_or_newline =
  [ `Newline of int * string option (* (number of newlines, subsequent_whitespace) *)
  | `Word of string ]
[@@deriving sexp_of]

(* Splits an array of lines into an array of pieces (`Newlines and R.Words) *)
let explode ar ~keep_ws =
  let words = Array.to_list ar in
  let words =
    if keep_ws
    then List.map words ~f:(split ~keep_ws)
    else List.map words ~f:whitespace_ignorant_split
  in
  let to_words l = List.map l ~f:(fun s -> `Word s) in
  (*
     [`Newline of (int * string option)]

     can be thought of as:

     [`Newline of
     ([`How_many_consecutive_newlines of int]
   * [`Some_subsequent_whitespace of string
     |`Empty_string
     ])]

     This representation is used to try to collapse consecutive whitespace as tightly as
     possible, but it's not a great abstraction, so some consecutive whitespace does not
     get collapsed.

  *)
  let words =
    List.concat_map words ~f:(fun x ->
      match x with
      | hd :: tl ->
        if keep_ws && not (String.is_empty hd) && is_ws hd
        then `Newline (1, Some hd) :: to_words tl
        else `Newline (1, None) :: `Word hd :: to_words tl
      | [] -> [ `Newline (1, None) ])
  in
  let words =
    List.fold_right words ~init:[] ~f:(fun x acc ->
      (* look back at what we've accumulated so far to see if there's any whitespace that
         can be collapsed. *)
      match acc with
      | `Word s :: tl -> x :: `Word s :: tl
      | `Newline (i, None) :: tl ->
        (match x with
         | `Word s -> `Word s :: `Newline (i, None) :: tl
         | `Newline (j, opt) ->
           (* collapse the whitespace from each [`Newline] by summing
              how_many_consecutive_newlines from each (i+j) *)
           `Newline (i + j, opt) :: tl)
      | `Newline (i, Some s1) :: tl ->
        (match x with
         | `Word s2 -> `Word s2 :: `Newline (i, Some s1) :: tl
         | `Newline (j, opt) ->
           (* collapse the whitespace from each [`Newline] by concatenating any
              subsequent_whitespace (opt ^ s1) and summing how_many_consecutive_newlines
              (i+j) from each. *)
           let s1 = Option.value opt ~default:"" ^ s1 in
           `Newline (i + j, Some s1) :: tl)
      | [] -> [ x ])
  in
  (* Throw away the very first `Newline *)
  let words =
    match words with
    | `Newline (i, opt) :: tl -> `Newline (i - 1, opt) :: tl
    | `Word _ :: _
    | [] ->
      raise_s
        [%message
          "Expected words to start with a `Newline." (words : word_or_newline list)]
  in
  (* Append a newline to the end, if this array has any words *)
  let words =
    match words with
    | [] -> []
    | [ `Newline (0, None) ] -> []
    | list -> List.append list [ `Newline (1, None) ]
  in
  Array.of_list words
;;

(* Takes hunks of `Words and `Newlines and collapses them back into lines,
 * formatting appropriately. *)
let collapse ranges ~rule_same ~rule_old ~rule_new ~kind ~output =
  let module H = Patience_diff.Hunk in
  let module R = Patience_diff.Range in
  (* flag indicates what kind of range is currently being collapsed *)
  let flag = ref `Same in
  (* segment is the current series of words being processed. *)
  let segment = ref [] in
  (* line is the current series of formatted segments *)
  let line = ref [] in
  (* lines is the return array *)
  let lines = ref [] in
  let apply ~rule = function
    | "" -> ""
    | s -> Output_ops.Rule.apply s ~rule ~output ~refined:false
  in
  (*
   * Finish the current segment by applying the appropriate format
   * and popping it on to the end of the current line
  *)
  let finish_segment () =
    let rule =
      match !flag with
      | `Same -> rule_same
      | `Old -> rule_old
      | `New -> rule_new
    in
    let formatted_segment = List.rev !segment |> String.concat |> apply ~rule in
    line := formatted_segment :: !line;
    segment := []
  in
  (*
   * Finish the current segment, apply the reset rule to the line,
   * and pop the finished line onto the return array
  *)
  let newline i =
    for _ = 1 to i do
      finish_segment ();
      lines := String.concat (List.rev !line) :: !lines;
      line := []
    done
  in
  let f range =
    (* Extract the array, set flag appropriately, *)
    let ar =
      match range with
      | R.Same ar ->
        flag := `Same;
        (* R.Same ar is an array of tuples.  The first tuple is an
         * element from the old file, the second tuple, an element
         * from the new file.  Depending on what kind of collapse
         * this is, we want only one or the other. *)
        let f =
          match kind with
          | `Old_only -> fst
          | `New_only -> snd
          | `Unified -> snd
        in
        Array.map ar ~f
      | R.Old ar ->
        flag := `Old;
        ar
      | R.New ar ->
        flag := `New;
        ar
      | R.Replace _ | R.Unified _ ->
        (* When calling collapse, we always call
         * Patience_diff.unified first, which removes all R.Replaces
         * and R.Unifieds. *)
        assert false
    in
    (* Iterate through the elements of the range, appending each `Word to
     * segment and calling newline on each `Newline
    *)
    Array.iter ar ~f:(function
      | `Newline (i, None) -> newline i
      | `Newline (i, Some s) ->
        newline i;
        segment := s :: !segment
      | `Word s -> segment := s :: !segment);
    finish_segment ()
  in
  List.iter ranges ~f;
  (match !line with
   | []
   | [ "" ] -> ()
   | line ->
     let line = String.concat (List.rev line) in
     if is_ws line
     then
       (* This branch was unreachable in our regression tests, but I can't prove it's
          unreachable in all cases. Rather than raise in production, let's drop this
          whitespace. *)
       ()
     else
       raise_s
         [%message
           "Invariant violated: [collapse] got a line not terminated with a newline"
             (line : string)]);
  Array.of_list (List.rev !lines)
;;

(* Get the hunks from two arrays of pieces (`Words and `Newlines) *)
let diff_pieces ~old_pieces ~new_pieces ~keep_ws ~word_big_enough =
  let context = -1 in
  let transform =
    if keep_ws
    then
      function
      | `Word s -> s
      | `Newline (lines, trailing_whitespace) ->
        Option.fold trailing_whitespace ~init:(String.make lines '\n') ~f:String.( ^ )
    else
      function
      | `Word s -> remove_ws s
      | `Newline (0, _) -> ""
      | `Newline (_, _) -> " "
  in
  Patience_diff.String.get_hunks
    ~transform
    ~context
    ~big_enough:word_big_enough
    ~mine:old_pieces
    ~other:new_pieces
;;

let ranges_are_just_whitespace ranges =
  let module R = Patience_diff.Range in
  List.for_all ranges ~f:(function
    | R.Old piece_array
    | R.New piece_array ->
      Array.for_all piece_array ~f:(function
        | `Word s -> String.is_empty (remove_ws s)
        | `Newline _ -> true)
    | _ -> true)
;;

(* Interleaves the display of minus lines and plus lines so that equal words are presented
   close together.  There is some heuristic for when we think doing this improves the
   diff. *)
let split_for_readability rangelist =
  let module R = Patience_diff.Range in
  let ans = ref [] in
  let pending_ranges = ref [] in
  let append_range range = pending_ranges := range :: !pending_ranges in
  List.iter rangelist ~f:(fun range ->
    let split_was_executed =
      match range with
      | R.New _ | R.Old _ | R.Replace _ | R.Unified _ -> false
      | R.Same seq ->
        let first_newline =
          Array.find_mapi seq ~f:(fun i -> function
            | `Word _, _
            | _, `Word _
            | `Newline (0, _), _
            | _, `Newline (0, _) -> None
            | `Newline first_nlA, `Newline first_nlB -> Some (i, first_nlA, first_nlB))
        in
        (match first_newline with
         | None -> false
         | Some (i, first_nlA, first_nlB) ->
           if Array.length seq - i <= too_short_to_split
           then false
           else (
             append_range (R.Same (Array.sub seq ~pos:0 ~len:i));
             (* A non-zero `Newline is required for [collapse] to work properly. *)
             let nl = R.Same [| `Newline (1, None), `Newline (1, None) |] in
             append_range nl;
             ans := List.rev !pending_ranges :: !ans;
             pending_ranges := [];
             let suf = Array.sub seq ~pos:i ~len:(Array.length seq - i) in
             let decr_first (x, y) = x - 1, y in
             suf.(0)
             <- (`Newline (decr_first first_nlA), `Newline (decr_first first_nlB));
             append_range (R.Same suf);
             true))
    in
    if not split_was_executed then append_range range);
  if !pending_ranges <> [] then ans := List.rev !pending_ranges :: !ans;
  List.rev !ans
;;

(* Refines the diff, splitting the lines into smaller arrays and diffing them, then
   collapsing them back into their initial lines after applying a format. *)
let refine
      ~rules
      ~produce_unified_lines
      ~output
      ~keep_ws
      ~split_long_lines
      ~interleave
      ~word_big_enough
      hunks
  =
  let module R = Patience_diff.Range in
  let module H = Patience_diff.Hunk in
  let module Rz = Format.Rules in
  let rule_old = rules.Rz.word_old in
  let rule_new = rules.Rz.word_new in
  let collapse = collapse ~rule_old ~rule_new ~output in
  let () =
    match output with
    | Ansi | Html -> ()
    | Ascii ->
      if produce_unified_lines
      then failwith "produce_unified_lines is not supported in Ascii mode"
  in
  let console_width =
    Memo.unit (fun () ->
      assert split_long_lines;
      match
        Or_error.bind Linux_ext.get_terminal_size ~f:(fun get_size ->
          Or_error.try_with (fun () -> get_size ()))
      with
      | Error _ -> 80
      | Ok pair -> snd pair)
  in
  let aux hunk =
    let aux = function
      | R.Replace (old_ar, new_ar) ->
        (* Explode the arrays *)
        let old_pieces = explode old_ar ~keep_ws in
        let new_pieces = explode new_ar ~keep_ws in
        (* Diff the pieces *)
        let sub_diff = diff_pieces ~old_pieces ~new_pieces ~keep_ws ~word_big_enough in
        (* Smash the hunks' ranges all together *)
        let sub_diff = Patience_diff.Hunks.ranges sub_diff in
        (* Break it up where lines are too long *)
        let sub_diff_pieces =
          if not split_long_lines
          then [ sub_diff ]
          else (
            let max_len = Int.max 20 (console_width () - 2) in
            (* Accumulates the total length of the line so far, summing lengths
               of word tokens but resetting when newlines are hit *)
            let get_new_len_so_far ~len_so_far tokens_arr =
              Array.fold ~init:len_so_far tokens_arr ~f:(fun len_so_far token ->
                match token with
                | `Newline _ -> 0
                | `Word word -> len_so_far + String.length word)
            in
            (* Iteratively split long lines up.
               Produces a list of "range lists", where each range list should be displayed
               all together in one unbroken piece before being followed by the next range
               list, etc. *)
            let rec split_lines len_so_far sub_diff rangeaccum rangelistaccum =
              match sub_diff with
              | [] ->
                (match rangeaccum with
                 | [] -> List.rev rangelistaccum
                 | _ -> List.rev (List.rev rangeaccum :: rangelistaccum))
              (* More tokens ranges left to process *)
              | range :: rest ->
                (match range with
                 | R.Same tokenpairs_arr ->
                   let range_of_tokens tokenpairs = R.Same (Array.of_list tokenpairs) in
                   (* Keep taking tokens until we exceed max_len or hit a newline.
                      Returns (new len_so_far, new range, remaining tokens)*)
                   let rec take_until_max len_so_far tokenpairs accum =
                     match tokenpairs with
                     | [] -> len_so_far, range_of_tokens (List.rev accum), []
                     | ((token, _) as tokenpair) :: rest ->
                       (match token with
                        | `Newline _ ->
                          0, range_of_tokens (List.rev (tokenpair :: accum)), rest
                        | `Word word ->
                          let wordlen = String.length word in
                          if wordlen + len_so_far > max_len && len_so_far > 0
                          then 0, range_of_tokens (List.rev accum), tokenpairs
                          else
                            take_until_max (wordlen + len_so_far) rest (tokenpair :: accum))
                   in
                   let make_newline () =
                     R.Same [| `Newline (1, None), `Newline (1, None) |]
                   in
                   (* Keep taking ranges until all tokens exhausted.
                      Returns (new len_so_far, range list) *)
                   let rec take_ranges_until_exhausted len_so_far tokenpairs accum =
                     match tokenpairs with
                     | [] -> len_so_far, List.rev accum
                     | _ ->
                       let new_len_so_far, new_range, new_tokenpairs =
                         take_until_max len_so_far tokenpairs []
                       in
                       let new_accum = `Range new_range :: accum in
                       (* If there are token pairs left, that means we hit the max_len,
                          so add a break at this point *)
                       let new_accum =
                         if new_tokenpairs <> []
                         then `Break :: `Range (make_newline ()) :: new_accum
                         else new_accum
                       in
                       take_ranges_until_exhausted new_len_so_far new_tokenpairs new_accum
                   in
                   let new_len_so_far, new_ranges =
                     take_ranges_until_exhausted
                       len_so_far
                       (Array.to_list tokenpairs_arr)
                       []
                   in
                   (* Update rangeaccum and rangelistaccum according to the `Ranges and
                      `Breaks. `Ranges accumulate on to the existing range list to be
                      displayed contiguously, `Breaks start a new range list. *)
                   let rangeaccum, rangelistaccum =
                     List.fold
                       new_ranges
                       ~init:(rangeaccum, rangelistaccum)
                       ~f:(fun (rangeaccum, rangelistaccum) r ->
                         match r with
                         | `Break -> [], List.rev rangeaccum :: rangelistaccum
                         | `Range r -> r :: rangeaccum, rangelistaccum)
                   in
                   split_lines new_len_so_far rest rangeaccum rangelistaccum
                 | R.New tokens_arr
                 | R.Old tokens_arr ->
                   let new_len_so_far = get_new_len_so_far ~len_so_far tokens_arr in
                   split_lines new_len_so_far rest (range :: rangeaccum) rangelistaccum
                 | R.Replace (old_arr, new_arr) ->
                   let new_len_so_far =
                     Int.max
                       (get_new_len_so_far ~len_so_far old_arr)
                       (get_new_len_so_far ~len_so_far new_arr)
                   in
                   split_lines new_len_so_far rest (range :: rangeaccum) rangelistaccum
                 | R.Unified _ -> assert false)
            in
            split_lines 0 sub_diff [] [])
        in
        let sub_diff_pieces =
          if interleave
          then List.concat_map sub_diff_pieces ~f:split_for_readability
          else sub_diff_pieces
        in
        List.concat_map sub_diff_pieces ~f:(fun sub_diff ->
          let sub_old = Patience_diff.Range.old_only sub_diff in
          let sub_new = Patience_diff.Range.new_only sub_diff in
          let all_same ranges =
            List.for_all ranges ~f:(fun range ->
              match range with
              | Patience_diff.Range.Same _ -> true
              | Patience_diff.Range.Old a
              | Patience_diff.Range.New a ->
                if keep_ws
                then false
                else
                  Array.for_all a ~f:(function
                    | `Newline _ -> true
                    | `Word _ -> false)
              | _ -> false)
          in
          let old_all_same = all_same sub_old in
          let new_all_same = all_same sub_new in
          let produce_unified_lines =
            produce_unified_lines
            && ((not (ranges_are_just_whitespace sub_old) && new_all_same)
                || (not (ranges_are_just_whitespace sub_new) && old_all_same))
          in
          (* Collapse the pieces back into lines *)
          let old_new_pairs =
            match old_all_same, new_all_same with
            | true, true ->
              let kind = `New_only in
              let rule_same = rules.Rz.word_same_unified in
              let new_ar = collapse sub_new ~rule_same ~kind in
              [ new_ar, new_ar ]
            | false, true ->
              let kind = `Old_only in
              let rule_same =
                if produce_unified_lines
                then rules.Rz.word_same_unified
                else rules.Rz.word_same_old
              in
              let old_ar = collapse sub_old ~rule_same ~kind in
              let kind = `New_only in
              let rule_same = rules.Rz.word_same_new in
              let new_ar = collapse sub_new ~rule_same ~kind in
              [ old_ar, new_ar ]
            | true, false ->
              let kind = `New_only in
              let rule_same =
                if produce_unified_lines
                then rules.Rz.word_same_unified
                else rules.Rz.word_same_new
              in
              let new_ar = collapse sub_new ~rule_same ~kind in
              let kind = `Old_only in
              let rule_same = rules.Rz.word_same_old in
              let old_ar = collapse sub_old ~rule_same ~kind in
              [ old_ar, new_ar ]
            | false, false ->
              let kind = `Old_only in
              let rule_same = rules.Rz.word_same_old in
              let old_ar = collapse sub_old ~rule_same ~kind in
              let kind = `New_only in
              let rule_same = rules.Rz.word_same_new in
              let new_ar = collapse sub_new ~rule_same ~kind in
              [ old_ar, new_ar ]
          in
          List.map old_new_pairs ~f:(fun (old_ar, new_ar) ->
            let range =
              match old_all_same, new_all_same with
              | true, true -> R.Same (Array.map new_ar ~f:(fun x -> x, x))
              | _ ->
                (match old_ar, new_ar with
                 (* Ugly hack that takes care of empty files *)
                 | [| "" |], new_ar -> R.Replace ([||], new_ar)
                 | old_ar, [| "" |] -> R.Replace (old_ar, [||])
                 | old_ar, new_ar ->
                   (match produce_unified_lines, old_all_same, new_all_same with
                    | true, true, false -> R.Unified new_ar
                    | true, false, true -> R.Unified old_ar
                    | false, _, _
                    | _, false, false -> R.Replace (old_ar, new_ar)
                    | _ -> assert false))
            in
            range))
      | R.New a
        when not keep_ws && Array.for_all a ~f:is_ws -> [ R.Same (Array.zip_exn a a) ]
      | R.Old a
        when not keep_ws && Array.for_all a ~f:is_ws -> []
      | (R.New _ | R.Old _ | R.Same _ | R.Unified _) as range -> [ range ]
    in
    let refined_ranges = List.concat_map hunk.H.ranges ~f:aux in
    { hunk with H.ranges = refined_ranges }
  in
  let refined_hunks = List.map hunks ~f:aux in
  List.filter refined_hunks ~f:(fun h -> not (H.all_same h))
;;

let print ~old_file ~new_file ~rules ~output ~location_style hunks =
  Output_ops.print
    hunks
    ~rules
    ~output
    ~file_names:(old_file, new_file)
    ~print:(Printf.printf "%s\n")
    ~location_style
    ~print_global_header:true
;;

let output_to_string
      ?(print_global_header = false)
      ~file_names
      ~rules
      ~output
      ~location_style
      hunks
  =
  let buf = Queue.create () in
  Output_ops.print
    hunks
    ~file_names
    ~location_style
    ~output
    ~print_global_header
    ~print:(Queue.enqueue buf)
    ~rules;
  String.concat (Queue.to_list buf) ~sep:"\n"
;;

let iter_ansi ~rules ~f_hunk_break ~f_line hunks =
  let hunks = Output_ops.Rules.apply hunks ~rules ~output:Ansi in
  Patdiff_hunks.iter ~f_hunk_break ~f_line hunks
;;

type diff_input =
  { name : string
  ; text : string
  }

let patdiff
      ?(context = default_context)
      ?(keep_ws = false)
      ?(rules = Format.Rules.default)
      ?(output = Output.Ansi)
      ?(produce_unified_lines = true)
      ?(split_long_lines = true)
      ?print_global_header
      ?(location_style = Format.Location_style.Diff)
      ?(interleave = true)
      ?(line_big_enough = default_line_big_enough)
      ?(word_big_enough = default_word_big_enough)
      ~from_
      ~to_
      ()
  =
  let hunks =
    diff
      ~context
      ~keep_ws
      ~line_big_enough
      ~mine:(List.to_array (String.split_lines from_.text))
      ~other:(List.to_array (String.split_lines to_.text))
    |> refine
         ~rules
         ~produce_unified_lines
         ~output
         ~keep_ws
         ~split_long_lines
         ~interleave
         ~word_big_enough
  in
  output_to_string
    ?print_global_header
    ~file_names:(from_.name, to_.name)
    ~rules
    ~output
    ~location_style
    hunks
;;

let%test_module _ =
  (module struct
    let from_ = { name = "old"; text = "Foo bar buzz" }
    let to_ = { name = "old"; text = "Foo buzz" }

    let%expect_test "Ansi output generates a single line diff" =
      printf
        "%s\n"
        (patdiff
           ~split_long_lines:false
           ~produce_unified_lines:true
           ~output:Ansi
           ~from_
           ~to_
           ());
      [%expect
        {|
      -1,1 +1,1
      [0;1;33m!|[0m[0mFoo[0;31m bar[0m buzz[0m |}]
    ;;

    let%expect_test "Ascii is supported if [produce_unified_lines] is false" =
      printf
        "%s\n"
        (patdiff
           ~split_long_lines:false
           ~produce_unified_lines:false
           ~output:Ascii
           ~from_
           ~to_
           ());
      [%expect {|
      -1,1 +1,1
      -|Foo bar buzz
      +|Foo buzz |}]
    ;;

    let%test "Ascii is not supported if [produce_unified_lines] is true" =
      match
        patdiff
          ~split_long_lines:false
          ~produce_unified_lines:true
          ~output:Ascii
          ~from_
          ~to_
          ()
      with
      | exception _ -> true
      | (_ : string) -> false
    ;;
  end)
;;
