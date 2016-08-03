open! Core.Std
open! Import

module Format = Patdiff_format
module Output = Output_mode

(* Strip whitespace from a string by stripping and replacing with spaces *)
let ws_rex = Pcre.regexp "[\\s]+"
let ws_sub = Pcre.subst " "
let remove_ws s = String.strip (Pcre.replace ~rex:ws_rex ~itempl:ws_sub s)

(* This regular expression describes the delimiters on which to split the string *)
let words_rex = Pcre.regexp
  "\\\"|\\{|\\}|\\[|\\]|[\\=\\`\\+\\-\\/\\!\\@\\$\\%\\^\\&\\*\\:]+|\\#|,|\\.|;|\\)|\\(|\\s+"

(* Split a string into a list of string options delimited by words_rex
   (delimiters included) *)
let split s ~keep_ws =
  let s = if keep_ws then s else String.rstrip s in
  let list = Pcre.full_split ~max:(-1) ~rex:words_rex s in
  List.filter_map list ~f:
    (fun split_result ->
      match split_result with
      | Pcre.Text s -> Some s
      | Pcre.Delim s -> Some s
      | Pcre.Group _ -> assert false   (* Irrelevant, since rex has no groups *)
      | Pcre.NoGroup -> assert false ) (* Ditto *)
;;

module type Output = Output_intf.S

module Ansi  = Ansi_output
module Ascii = Ascii_output
module Html  = Html_output

module Output_ops = struct

  module Rule = struct

    let apply text ~rule ~output ~refined =
      match (output : Output.t) with
      | Ansi  -> Ansi.Rule.apply  text ~rule ~refined
      | Ascii -> Ascii.Rule.apply text ~rule ~refined
      | Html  -> Html.Rule.apply  text ~rule ~refined
    ;;

  end

  module Rules = struct

    let to_string rules output =
      let module Rz = Format.Rules in
      let module R = Patience_diff.Range in
      let apply text ~rule ~refined = Rule.apply text ~rule ~output ~refined in
      function
      | R.Same ar ->
        let formatted_ar = Array.map ar
          ~f:(fun (x,y) ->
            let app = apply ~rule:rules.Rz.line_same ~refined:false in
            (app x, app y))
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

    let map_ranges hunks ~f =
      let f hunk =
        let module H = Patience_diff.Hunk in
        { hunk with
          H.ranges = List.map hunk.H.ranges ~f;
        }
      in
      List.map hunks ~f

    let apply hunks ~rules ~output =
      map_ranges hunks ~f:(to_string rules output)

  end

  let print ~print_global_header ~file_names ~rules ~output ~print ~location_style hunks =
    let formatted_hunks = Rules.apply ~rules ~output hunks in
    let f =
      match (output : Output.t) with
      | Ansi  -> Ansi.print
      | Ascii -> Ascii.print
      | Html  -> Html.print
    in
    f ~print_global_header ~file_names ~rules ~print ~location_style formatted_hunks
  ;;
end

let default_context = 6

let diff ~context ~compare ~keep_ws ~mine ~other =
  if keep_ws then
    let transform = fun x -> x in
    Patience_diff.get_hunks ~mine ~other ~transform ~context ~compare
  else
    let compare = fun x y -> compare (remove_ws x)  (remove_ws y) in
    let transform = remove_ws in
    Patience_diff.get_hunks ~mine ~other ~transform ~context ~compare
;;

(* Splits an array of lines into an array of pieces (`Newlines and R.Words) *)
let explode ar ~keep_ws =
  let words = Array.to_list ar in
  let words = List.map words ~f:(split ~keep_ws) in
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
        if (Pcre.pmatch ~rex:ws_rex hd) then
          `Newline (1, (Some hd)) :: (to_words tl)
        else
          `Newline (1, None) :: `Word hd :: (to_words tl)
      | [] -> [`Newline (1, None)])
  in
  let words =
    List.fold_right words ~init:[] ~f:(fun x acc ->
      (* look back at what we've accumulated so far to see if there's any whitespace that
         can be collapsed. *)
      match acc with
      | `Word s :: tl -> x :: `Word s :: tl
      | `Newline (i, None) :: tl ->
        begin match x with
        | `Word s -> `Word s :: `Newline (i, None) :: tl
        | `Newline (j, opt) ->
          (* collapse the whitespace from each [`Newline] by summing
             how_many_consecutive_newlines from each (i+j) *)
          `Newline (i+j, opt) :: tl
        end
      | `Newline (i, Some s1) :: tl ->
        begin match x with
        | `Word s2 -> `Word s2 :: `Newline (i, Some s1) :: tl
        | `Newline (j, opt) ->
          (* collapse the whitespace from each [`Newline] by concatenating any
             subsequent_whitespace (opt ^ s1) and summing how_many_consecutive_newlines
             (i+j) from each. *)
          let s1 = (Option.value opt ~default:"") ^ s1 in
          `Newline (i+j, Some s1) :: tl
        end
      | [] -> [x])
  in
  (* Throw away the very first `Newline *)
  let words =
    match words with
    | `Newline (_i, opt) :: tl -> `Newline (0, opt) :: tl
    | `Word _ :: _ | [] -> assert false (* hd is always `Newline *)
  in
  (* Append a newline to the end, if this array has any words *)
  let words =
    match words with
    | [] -> []
    | [`Newline (0, None)] -> []
    | list -> List.append list [`Newline (1, None)]
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
  let apply = Output_ops.Rule.apply ~output ~refined:false in
  (*
   * Finish the current segment by applying the appropriate format
   * and popping it on to the end of the current line
   *)
  let finish_segment () =
    let rule = match !flag with
      | `Same -> rule_same
      | `Old -> rule_old
      | `New -> rule_new in
    let formatted_segment = List.rev !segment |> String.concat |> apply ~rule in
    line := formatted_segment :: !line ;
    segment := [] in
  (*
   * Finish the current segment, apply the reset rule to the line,
   * and pop the finished line onto the return array
   *)
  let newline i =
    for _ = 1 to i do
      finish_segment ();
      lines := (String.concat (List.rev !line)) :: !lines;
      line := []
    done in
  let f range =
    (* Extract the array, set flag appropriately, *)
    let ar = match range with
      | R.Same ar ->
        flag := `Same;
        (* R.Same ar is an array of tuples.  The first tuple is an
         * element from the old file, the second tuple, an element
         * from the new file.  Depending on what kind of collapse
         * this is, we want only one or the other. *)
        let f = match kind with
          | `Old_only -> fst
          | `New_only -> snd
          | `Unified -> snd in
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
        assert false in
    (* Iterate through the elements of the range, appending each `Word to
     * segment and calling newline on each `Newline
     *)
    Array.iter ar ~f:(function
    | `Newline (i, None) -> newline i;
    | `Newline (i, Some s) -> newline i; segment := s :: !segment
    | `Word s -> segment := s :: !segment);
    finish_segment ()
  in
  List.iter ranges ~f;
  Array.of_list (List.rev !lines)
;;

let piece_transform = function
  | `Word s -> s
  | `Newline _ -> " "
;;

(* Get the hunks from two arrays of pieces (`Words and `Newlines) *)
let diff_pieces ~old_pieces ~new_pieces ~keep_ws =
  let context = -1 in
  let transform =
    if keep_ws then piece_transform
    else fun x -> remove_ws (piece_transform x) in
  let compare = String.compare in
  Patience_diff.get_hunks ~mine:old_pieces ~other:new_pieces ~transform ~context ~compare
;;

let ranges_are_just_whitespace ranges =
  let module R = Patience_diff.Range in
  List.for_all ranges ~f:(function
  | R.Old piece_array | R.New piece_array ->
    Array.for_all piece_array ~f:(fun piece ->
      let s = piece_transform piece in
      remove_ws s = "")
  | _ -> true)
;;

(* Refines the diff, splitting the lines into smaller arrays and diffing them, then
   collapsing them back into their initial lines after applying a format. *)
let refine ~rules ~produce_unified_lines ~output ~keep_ws ~split_long_lines hunks =
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
          Or_error.try_with get_size)
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
        let sub_diff = diff_pieces ~old_pieces ~new_pieces ~keep_ws in

        (* Smash the hunks' ranges all together *)
        let sub_diff = Patience_diff.ranges sub_diff in

        (* Break it up where lines are too long *)
        let sub_diff_pieces =
          if not split_long_lines
          then [sub_diff]
          else
            let max_len = Int.max 20 (console_width () - 2) in

            (* Accumulates the total length of the line so far, summing lengths
               of word tokens but resetting when newlines are hit *)
            let get_new_len_so_far ~len_so_far tokens_arr =
              Array.fold ~init:len_so_far tokens_arr ~f:(fun len_so_far token ->
                match token with
                | `Newline _ -> 0
                | `Word word -> len_so_far + String.length word
              )
            in

            (* Iteratively split long lines up.
               Produces a list of "range lists", where each range list should be displayed
               all together in one unbroken piece before being followed by the next range
               list, etc. *)
            let rec split_lines len_so_far sub_diff rangeaccum rangelistaccum =
              match sub_diff with
              | [] -> begin
                match rangeaccum with
                | [] -> List.rev rangelistaccum
                | _ -> List.rev (List.rev rangeaccum :: rangelistaccum)
              end
              (* More tokens ranges left to process *)
              | range :: rest ->
                match range with
                | R.Same tokenpairs_arr ->

                  let range_of_tokens tokenpairs =
                    R.Same (Array.of_list tokenpairs)
                  in

                  (* Keep taking tokens until we exceed max_len or hit a newline.
                     Returns (new len_so_far, new range, remaining tokens)*)
                  let rec take_until_max len_so_far tokenpairs accum =
                    match tokenpairs with
                    | [] -> (len_so_far, range_of_tokens (List.rev accum), [])
                    | ((token,_) as tokenpair) :: rest ->
                      match token with
                      | `Newline _ ->
                        (0, range_of_tokens (List.rev (tokenpair :: accum)), rest)
                      | `Word word ->
                        let wordlen = String.length word in
                        if wordlen + len_so_far > max_len && len_so_far > 0
                        then (0, range_of_tokens (List.rev accum), tokenpairs)
                        else take_until_max (wordlen + len_so_far) rest (tokenpair :: accum)
                  in

                  let make_newline () =
                    R.Same [|`Newline (1,None), `Newline (1,None)|]
                  in

                  (* Keep taking ranges until all tokens exhausted.
                     Returns (new len_so_far, range list) *)
                  let rec take_ranges_until_exhausted len_so_far tokenpairs accum =
                    match tokenpairs with
                    | [] -> (len_so_far, List.rev accum)
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
                    take_ranges_until_exhausted len_so_far (Array.to_list tokenpairs_arr) []
                  in

                  (* Update rangeaccum and rangelistaccum according to the `Ranges and
                     `Breaks. `Ranges accumulate on to the existing range list to be
                     displayed contiguously, `Breaks start a new range list. *)
                  let (rangeaccum, rangelistaccum) =
                    List.fold new_ranges ~init:(rangeaccum, rangelistaccum)
                      ~f:(fun (rangeaccum, rangelistaccum) r ->
                        match r with
                        | `Break -> ([], List.rev rangeaccum :: rangelistaccum)
                        | `Range r -> (r :: rangeaccum, rangelistaccum)
                      )
                  in
                  split_lines new_len_so_far rest rangeaccum rangelistaccum
                | R.New tokens_arr
                | R.Old tokens_arr ->
                  let new_len_so_far = get_new_len_so_far ~len_so_far tokens_arr in
                  split_lines new_len_so_far rest (range :: rangeaccum) rangelistaccum
                | R.Replace (old_arr,new_arr) ->
                  let new_len_so_far = Int.max
                    (get_new_len_so_far ~len_so_far old_arr)
                    (get_new_len_so_far ~len_so_far new_arr)
                  in
                  split_lines new_len_so_far rest (range :: rangeaccum) rangelistaccum
                | R.Unified _ -> assert false
            in
            split_lines 0 sub_diff [] []
        in

        List.concat_map sub_diff_pieces ~f:(fun sub_diff ->
          let sub_old = Patience_diff.Range.old_only sub_diff in
          let sub_new = Patience_diff.Range.new_only sub_diff in
          let old_all_same = Patience_diff.Range.all_same sub_old in
          let new_all_same = Patience_diff.Range.all_same sub_new in

          let produce_unified_lines =
            produce_unified_lines &&
              ((not (ranges_are_just_whitespace sub_old) && new_all_same) ||
                  (not (ranges_are_just_whitespace sub_new) && old_all_same))
          in

          (* Collapse the pieces back into lines *)
          let old_new_pairs =
            begin match old_all_same, new_all_same with
            | true, true ->
              let kind = `New_only in
              let rule_same = rules.Rz.word_same_unified in
              let new_ar = collapse sub_new ~rule_same ~kind in
              [ (new_ar, new_ar) ]
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
              [ (old_ar, new_ar) ]
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
              [ (old_ar, new_ar) ]
            | false, false ->
              let kind = `Old_only in
              let rule_same = rules.Rz.word_same_old in
              let old_ar = collapse sub_old ~rule_same ~kind in
              let kind = `New_only in
              let rule_same = rules.Rz.word_same_new in
              let new_ar = collapse sub_new ~rule_same ~kind in
              [ (old_ar, new_ar) ]
            end
          in

          List.map old_new_pairs ~f:(fun (old_ar, new_ar) ->
            let range =
              match old_all_same, new_all_same with
              | true, true -> R.Same (Array.map new_ar ~f:(fun x -> (x,x)))
              | _ -> match old_ar, new_ar with
                (* Ugly hack that takes care of empty files *)
                | [|""|], new_ar -> R.Replace ([||], new_ar)
                | old_ar, [|""|] -> R.Replace (old_ar, [||])
                | old_ar, new_ar ->
                  match produce_unified_lines, old_all_same, new_all_same with
                  | true, true, false -> R.Unified new_ar
                  | true, false, true -> R.Unified old_ar
                  | false, _, _ | _, false, false -> R.Replace (old_ar, new_ar)
                  | _ -> assert false
            in
            range
          )
        )
      | range -> [ range ]
    in
    let refined_ranges = List.concat_map hunk.H.ranges ~f:aux in
    { hunk with
      H.ranges = refined_ranges;
    }
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
      ?(print_global_header = false) ~file_names ~rules ~output ~location_style hunks =
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
      ~from_ ~to_ () =
  let hunks =
    diff ~context ~compare:String.compare ~keep_ws
      ~mine: (List.to_array (String.split_lines from_.text))
      ~other:(List.to_array (String.split_lines to_.text))
    |> refine ~rules ~produce_unified_lines ~output ~keep_ws ~split_long_lines
  in
  output_to_string
    ?print_global_header
    ~file_names:(from_.name, to_.name)
    ~rules
    ~output
    ~location_style
    hunks
;;

let%test_module _ = (module struct
  let from_ = { name = "old"; text = "Foo bar buzz" }
  let to_ = { name = "old"; text = "Foo buzz" }

  let%expect_test "Ansi output generates a single line diff" =
    printf "%s\n"
      (patdiff
         ~split_long_lines:false
         ~produce_unified_lines:true
         ~output:Ansi
         ~from_
         ~to_
         ());
    [%expect {|
      -1,1 +1,1
      [0;1;33m!|[0m[0;0mFoo [0;31mbar [0mbuzz[0m |}]
  ;;

  let%expect_test "Ascii is supported if [produce_unified_lines] is false" =
    printf "%s\n"
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
