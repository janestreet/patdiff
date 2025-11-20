open! Core
open! Import
include Patdiff_core_intf

let default_double_column_width = 121
let default_refine_line_length = 78
let min_col_width = 60

include struct
  open Configuration

  let default_context = default_context
  let default_line_big_enough = default_line_big_enough
  let default_word_big_enough = default_word_big_enough
end

(* Strip whitespace from a string by stripping and replacing with spaces *)
let ws_rex = lazy (Re.compile Re.(rep1 space))
let ws_rex_anchored = lazy (Re.compile Re.(seq [ bol; rep space; eol ]))

let remove_ws =
  (* This is often called over and over on each line so try to reuse the same array every
     time *)
  let arr = ref (Array.create ~len:500 ' ') in
  fun s ->
    if Array.length !arr < String.length s
    then arr := Array.create ~len:(String.length s) ' ';
    let i = ref 0 in
    let in_ws = ref false in
    let found_char = ref false in
    String.iter s ~f:(fun ch ->
      if Char.is_whitespace ch
      then in_ws := true
      else (
        if !in_ws && !found_char
        then (
          !arr.(!i) <- ' ';
          !arr.(!i + 1) <- ch;
          i := !i + 2)
        else (
          !arr.(!i) <- ch;
          Int.incr i);
        in_ws := false;
        found_char := true));
    String.init !i ~f:(fun i -> !arr.(i))
;;

let is_ws s = Re.execp (force ws_rex_anchored) s

(* This regular expression describes the delimiters on which to split the string *)
let words_rex =
  lazy
    (let open Re in
     let delim = set {|"{}[]#,.;()_|} in
     let punct = rep1 (set {|=`+-/!@$%^&*:|<>|}) in
     let space = rep1 space in
     (* We don't want to split up ANSI color sequences, so let's make sure they get
        through intact. *)
     let ansi_sgr_sequence =
       let esc = char '\027' in
       seq [ esc; char '['; rep (alt [ char ';'; digit ]); char 'm' ]
     in
     compile (alt [ delim; punct; space; ansi_sgr_sequence ]))
;;

(* Split a string into a list of string options delimited by words_rex (delimiters
   included) *)
let split s ~keep_ws =
  let s = if keep_ws then s else String.rstrip s in
  if String.is_empty s && keep_ws
  then [ "" ]
  else
    Re.split_full (force words_rex) s
    |> List.filter_map ~f:(fun token ->
      let string =
        match token with
        | `Delim d -> Re.Group.get d 0
        | `Text t -> t
      in
      if String.is_empty string then None else Some string)
;;

(* This function ensures that the tokens passed to Patience diff do not include
   whitespace. Whitespace is appended to words, and then removed by [~transform] later on.
   The point is to make the semantic cleanup go well -- we don't want whitespace matches
   to "count" as part of the length of a match. *)
let whitespace_ignorant_split s =
  if String.is_empty s
  then []
  else (
    let istext s = not (Re.execp (force ws_rex) s) in
    split s ~keep_ws:false
    |> List.group ~break:(fun split_result1 _ -> istext split_result1)
    |> List.map ~f:String.concat)
;;

include struct
  let%expect_test _ =
    print_s ([%sexp_of: string list] (split ~keep_ws:true ""));
    [%expect {| ("") |}]
  ;;
end

module Make (Output_impls : Output_impls) = struct
  module Output_ops = struct
    module Rule = struct
      let apply ~rule ~output ~refined text =
        let (module O) = Output_impls.implementation output in
        O.Rule.apply ~rule ~refined text
      ;;
    end

    let output_width ?width_override () =
      match width_override, Output_impls.console_width (), am_running_test with
      (* Since we often render diffs indented-by-one (whenever we have multiple hunks to
         display), subtracting 1 helps to ensure that we don't exceed the width of the
         terminal we're printing to. *)
      | Some override, Ok console, false when override >= console -> override - 1
      | Some override, _, _ -> override
      | None, Ok console, false -> console - 1
      | None, Ok console, true -> console
      | None, Error _, _ -> default_double_column_width
    ;;

    module Single_column = struct
      let to_string (rules : Format.Rules.t) output
        : string Patience_diff.Range.t -> string Patience_diff.Range.t
        =
        let apply text ~rule ~refined = Rule.apply text ~rule ~output ~refined in
        function
        | Same ar ->
          let formatted_ar =
            Array.map ar ~f:(fun (x, y) ->
              let app = apply ~rule:rules.line_same ~refined:false in
              app x, app y)
          in
          Same formatted_ar
        | Next (ar, move_kind) ->
          Next
            ( Array.map
                ar
                ~f:
                  (apply
                     ~refined:false
                     ~rule:
                       (match move_kind with
                        | Some (Move _) -> rules.moved_to_next
                        | Some (Within_move _) -> rules.added_in_move
                        | None -> rules.line_next))
            , move_kind )
        | Prev (ar, move_kind) ->
          Prev
            ( Array.map
                ar
                ~f:
                  (apply
                     ~refined:false
                     ~rule:
                       (match move_kind with
                        | Some (Move _) -> rules.moved_from_prev
                        | Some (Within_move _) -> rules.removed_in_move
                        | None -> rules.line_prev))
            , move_kind )
        | Unified (ar, move_id) ->
          Unified
            ( Array.map
                ar
                ~f:
                  (apply
                     ~refined:true
                     ~rule:
                       (match move_id with
                        | None -> rules.line_unified
                        | Some _ -> rules.line_unified_in_move))
            , move_id )
        | Replace (ar1, ar2, move_id) ->
          let prev_rule, next_rule =
            match move_id with
            | None -> rules.line_prev, rules.line_next
            | Some _ -> rules.removed_in_move, rules.added_in_move
          in
          let ar1 = Array.map ar1 ~f:(apply ~refined:true ~rule:prev_rule) in
          let ar2 = Array.map ar2 ~f:(apply ~refined:true ~rule:next_rule) in
          Replace (ar1, ar2, move_id)
      ;;

      let ranges_to_strings ~rules ~output (hunks : _ Patience_diff.Hunk.t list) =
        List.map hunks ~f:(fun hunk ->
          { hunk with ranges = List.map hunk.ranges ~f:(to_string rules output) })
      ;;

      let build_unified ~rules ~output (hunks : _ Patience_diff.Hunk.t list) =
        let formatted_hunks = ranges_to_strings ~rules ~output hunks in
        let blocks_rev = ref [] in
        let f_line line =
          match !blocks_rev with
          | hd :: tl -> blocks_rev := (line :: hd) :: tl
          | _ -> blocks_rev := [ line ] :: !blocks_rev
        in
        let f_hunk_break _ _ = blocks_rev := [] :: !blocks_rev in
        Hunks.iter ~f_hunk_break ~f_line formatted_hunks;
        List.rev_map !blocks_rev ~f:List.rev
      ;;

      let print_unified
        ~print_global_header
        ~file_names
        ~rules
        ~output
        ~print
        ~location_style
        hunks
        =
        let formatted_hunks = ranges_to_strings ~rules ~output hunks in
        let (module O) = Output_impls.implementation output in
        O.print
          ~print_global_header
          ~file_names
          ~rules
          ~print
          ~location_style
          formatted_hunks
      ;;
    end

    module Double_column = struct
      (* Converts a number to a right-aligned string followed by a space. *)
      let line_num_text ~len line_num =
        let n =
          match line_num with
          | None -> ""
          | Some n -> Int.to_string n
        in
        String.pad_left n ~char:' ' ~len ^ " "
      ;;

      let max_line_number (hunks : 'a Patience_diff.Hunk.t list) =
        List.fold hunks ~init:0 ~f:(fun m hunk ->
          max m (hunk.prev_start + hunk.prev_size - 1)
          |> max (hunk.next_start + hunk.next_size - 1))
      ;;

      let create_padding len = String.init len ~f:(Fn.const ' ')

      let get_gutters
        ?(wrapped = false)
        ~(rules : Format.Rules.t)
        ~output
        (line : Side_by_side.Line_info.t)
        =
        let prefix_column_width = String.length rules.line_same.pre.text in
        let empty = create_padding prefix_column_width in
        if wrapped
        then empty, empty (* No gutter if the text wrapped from a previous line. *)
        else (
          let gutter_for rule = Rule.apply "" ~rule ~output ~refined:false in
          match line with
          | Same (prev, next) ->
            let unified_line =
              Side_by_side.Line.any_non_same prev || Side_by_side.Line.any_non_same next
            in
            let gutter =
              if unified_line
              then gutter_for rules.line_unified
              else gutter_for rules.line_same
            in
            gutter, gutter
          | Prev (_, None) -> gutter_for rules.line_prev, empty
          | Prev (_, _) -> gutter_for rules.moved_from_prev, empty
          | Next (_, None) -> empty, gutter_for rules.line_next
          | Next (_, _) -> empty, gutter_for rules.moved_to_next)
      ;;

      let get_diff_style
        ~(rules : Format.Rules.t)
        ~output
        (line : Side_by_side.Line_info.t)
        =
        let apply ~rule str = Rule.apply ~rule ~output ~refined:false str in
        let basic_diff_style = function
          | `Prev -> apply ~rule:rules.word_prev
          | `Same -> apply ~rule:rules.word_same_unified
          | `Next -> apply ~rule:rules.word_next
        in
        match line with
        | Same _ | Prev (_, None) | Next (_, None) -> basic_diff_style
        | Prev (_, Some _move_id) ->
          (function
            | `Prev -> apply ~rule:rules.word_prev
            | `Same -> apply ~rule:(Format.Rule.strip_prefix rules.moved_from_prev)
            | `Next -> Fn.id (* Shouldn't be called, but returning the word seems safe. *))
        | Next (_, Some _move_id) ->
          (function
            | `Prev -> Fn.id (* Shouldn't be called, but returning the word seems safe. *)
            | `Same -> apply ~rule:(Format.Rule.strip_prefix rules.moved_to_next)
            | `Next -> apply ~rule:rules.word_next)
      ;;

      let render_side_by_side_line
        ?(line_num_width = 0)
        ~(rules : Format.Rules.t)
        ~output
        ~wrap_or_truncate
        ~pane_cols
        (line : Side_by_side.Line_info.t)
        =
        let prefix_column_width = String.length rules.line_same.pre.text in
        let width =
          if line_num_width <= 0
          then pane_cols - prefix_column_width
          else pane_cols - prefix_column_width - line_num_width - 1
        in
        let lines =
          match wrap_or_truncate with
          | `wrap ->
            Side_by_side.Line_info.wrap line ~width
            |> List.map ~f:Side_by_side.Line_info.lines
          | `truncate ->
            let left, right =
              Side_by_side.Line_info.truncate line ~width |> Side_by_side.Line_info.lines
            in
            [ left, right ]
          | `neither ->
            let left, right = Side_by_side.Line_info.lines line in
            [ left, right ]
        in
        let style = get_diff_style line ~rules ~output in
        List.mapi lines ~f:(fun i (left, right) ->
          let lnum, rnum =
            if line_num_width <= 0
            then "", ""
            else
              ( line_num_text (Side_by_side.Line.line_number left) ~len:line_num_width
              , line_num_text (Side_by_side.Line.line_number right) ~len:line_num_width )
          in
          let lgut, rgut = get_gutters ~wrapped:(i > 0) ~rules ~output line in
          let lpad = create_padding (Int.max 0 (width - Side_by_side.Line.width left)) in
          let ltext = Side_by_side.Line.styled_string ~output ~style left in
          let rtext = Side_by_side.Line.styled_string ~output ~style right in
          let left = lnum ^ lgut ^ ltext ^ lpad in
          let right = rnum ^ rgut ^ rtext |> String.rstrip in
          left, right)
      ;;

      let build_side_by_side
        ?width_override
        ?(include_line_numbers = true)
        ~(rules : Format.Rules.t)
        ~wrap_or_truncate
        ~output
        hunks
        =
        let blocks_rev = ref [] in
        (* Need one character for the center divider *)
        let pane_cols = (output_width ?width_override () - 1) / 2 in
        let pane_cols = max pane_cols min_col_width in
        let line_num_width =
          match include_line_numbers with
          | true -> String.length (Int.to_string (max_line_number hunks))
          | false -> 0
        in
        Array.iter (Side_by_side.hunks_to_lines hunks) ~f:(fun hunk ->
          let hunk_lines_rev = ref [] in
          Array.iter hunk ~f:(fun line ->
            (* We may get back multiple lines if we are wrapping long lines. *)
            let new_lines =
              render_side_by_side_line
                line
                ~rules
                ~output
                ~pane_cols
                ~line_num_width
                ~wrap_or_truncate
            in
            hunk_lines_rev := List.rev_append new_lines !hunk_lines_rev);
          blocks_rev := List.rev !hunk_lines_rev :: !blocks_rev);
        List.rev !blocks_rev
      ;;

      let make_divider = function
        | Output.Ansi | Html -> "│"
        | Ascii -> "|"
      ;;

      let render_filenames
        ~prev_file
        ~next_file
        ~pane_cols
        ~output
        ~(rules : Format.Rules.t)
        ~line_num_width
        =
        let cols_available = pane_cols - line_num_width in
        let line_num_space = String.make line_num_width ' ' in
        let middle_divider = make_divider output in
        let render_file_name ~rule file =
          let filename = File_name.display_name file in
          String.concat [ Rule.apply ~rule ~output ~refined:false ""; filename ]
        in
        (* Add a header block with the file names. *)
        let prev = render_file_name ~rule:rules.line_prev prev_file in
        let next = render_file_name ~rule:rules.line_next next_file in
        match output with
        | Output.Ansi | Output.Ascii ->
          List.map
            (Ansi_text.to_double_column ~width:cols_available ~left:prev ~right:next)
            ~f:(fun (left, right) ->
              String.concat
                [ line_num_space; left; middle_divider; line_num_space; right ])
        | Output.Html ->
          let buff =
            String.make (pane_cols - String.length (File_name.display_name prev_file)) ' '
          in
          (* Don't try to use ansi_text to wrap HTML! *)
          [ String.concat
              [ line_num_space; prev; buff; middle_divider; line_num_space; next ]
          ]
      ;;

      let print_side_by_side
        ?width_override
        ?file_names
        ~(rules : Format.Rules.t)
        ~wrap_or_truncate
        ~output
        ~print
        original_hunks
        =
        let insert_filenames () =
          match file_names with
          | None -> ()
          | Some (prev_file, next_file) ->
            let line_num_width =
              String.length (Int.to_string (max_line_number original_hunks)) + 1
            in
            let pane_cols = (output_width ?width_override () - 1) / 2 in
            let filename_lines =
              render_filenames
                ~prev_file
                ~next_file
                ~pane_cols
                ~output
                ~rules
                ~line_num_width
            in
            List.iter filename_lines ~f:print
        in
        let lines =
          build_side_by_side
            ?width_override
            ~rules
            ~wrap_or_truncate
            ~output
            original_hunks
          |> List.concat
        in
        (match output with
         | Output.Ansi | Ascii -> ()
         | Html -> print "<pre style=\"font-family:consolas,monospace\">");
        let middle_divider = make_divider output in
        insert_filenames ();
        List.iter lines ~f:(fun (left, right) ->
          String.concat [ left; middle_divider; right ] |> print);
        match output with
        | Output.Ansi | Ascii -> ()
        | Html -> print "</pre>"
      ;;
    end
  end

  let indentation line =
    let rec loop line len i n =
      if i >= len
      then n, i
      else (
        match line.[i] with
        | ' ' -> loop line len (i + 1) (n + 1)
        (* tabs count for 4 spaces *)
        | '\t' -> loop line len (i + 1) (n + 4)
        | _ -> n, i)
    in
    loop line (String.length line) 0 0
  ;;

  let score_line (side : [ `left | `right ]) line1 line2 : int =
    let i1, start_of_1 = indentation line1 in
    let i2, start_of_2 = indentation line2 in
    (* Order of priority is roughly:
       1. low indentation for second line
       2. lower indentation for second line than first
       3. bonus points for certain patterns at the (non-whitespace) start of the line

       But it isn’t priority, we just add things. So a failure case may cause us to pick a
       boundary like this:
       {v
               (some subfield)))
        ------------- BOUNDARY ----------------
          (other_field .))
         ((new entry)
          ....)
       v}
       instead of between other_field and new entry.

       We try to counteract that for the case where the line below the boundary starts
       with e.g. ‘;;’ by removing the ‘decreasing indentation bonus’ in that case.
       Plausibly we could do something by taking into account the count of closing parens
       minus opening parens.

       A secondary issue is that [line1] or [line2] may be entirely whitespace which can
       be misleading (e.g. editors will typically remove indentation from pure-whitespace
       lines) and cause us to miss good information (e.g. for an added function we would
       like to have bonus points for the boundaries being just before ‘let ...’ and just
       after ‘;;’ but typically there is some whitespace added to be put before/after the
       function instead and that is what we score) *)
    let some_lines_are_blank = String.length line1 = 0 || String.length line2 = 0 in
    let base_score =
      let i2 = if some_lines_are_blank then max i1 i2 else i2 in
      max (-90) (90 - (i2 * 2))
    in
    let decreasing_indentation_bonus =
      if some_lines_are_blank
      then 0
      else
        (if i1 = i2
            (* This funky thing hopefully us to prefer a diff like ‘end
               [ module ... end ]’ to one like ‘[ end module ... ] end’, where [] marks
               the boundary of the diff. *)
         then (
           match side with
           | `left -> 1
           | `right -> 0)
         else i1 - i2)
        |> Int.clamp_exn ~min:(-2) ~max:3
    in
    let bonus_for_chars =
      (* [bonus n line sides str] returns [n] if a bonus score applies, or 0 otherwise.

         [line] can be [`above] or [`below]. [sides] can be [`left], [`right], or [`any].

         The bonus score applies if [str] is found at the beginning of the line
         immediately [`above] or [`below] the boundary of the inserted/deleted region.

         If [sides] is [`left] or [`right], the bonus score only applies to that boundary
         of the diff region.

         So for example, [bonus 5 `above `any "</"] would add a bonus score of 5 if either
         boundary is immediately before a closing XML tag. *)
      let bonus n line sides str =
        let line, i =
          match line with
          | `above -> line1, start_of_1
          | `below -> line2, start_of_2
        in
        match sides, side with
        | `any, _ | `left, `left | `right, `right ->
          if String.is_substring_at line ~substring:str ~pos:i then n else 0
        | _ -> 0
      in
      bonus 1 `below `any "((" (* start of record bonus *)
      + bonus 3 `below `any "("
      + bonus 1 `above `right "}"
      + bonus (-1) `below `any "}"
      + bonus 1 `below `any "{"
      (* XML. Big bonus here as we prefer to break between </ and < despite equal
         indentation. *)
      + bonus 5 `above `any "</"
      + bonus (-4) `below `left "</" (* discount for starting diff on a </...> *)
      + bonus 3 `below `any "<"
      + bonus 2 `below `any "*" (* heading *)
      + bonus 1 `below `any "-" (* bullet point *)
      + bonus 3 `above `right ";;"
      + bonus 1 `above `left ";;"
      + bonus 4 `below `left "let"
      + bonus (-1) `below `left "let%"
      + bonus 2 `below `left "let%test"
      + bonus 2 `below `left "let%expect"
      + bonus 2 `below `right "let"
      + bonus 1 `above `any "in"
      + bonus 4 `below `left "module"
      + bonus 3 `above `right "end"
      (* In these cases, we typically get decreasing indentation but we want the ending
         token (e.g. ;;) above the boundaries *)
      + bonus (min (-1) (-decreasing_indentation_bonus)) `below `any ";;"
      + bonus (min (-1) (-decreasing_indentation_bonus)) `below `any "end"
      (* starting on a blank line gives bonus *)
      + if start_of_2 >= String.length line2 then 2 else 0
    in
    base_score + decreasing_indentation_bonus + bonus_for_chars
  ;;

  module Range_info = struct
    module T = struct
      type t =
        { range_index : int
        ; size_of_range : int
        ; replace_id : int option
        }
      [@@deriving compare ~localize, hash, sexp_of, fields ~getters]

      let compare_by_size = Comparable.lift Int.compare ~f:size_of_range
    end

    include T
    include Hashable.Make_plain (T)
  end

  (* Used to track the state of [Replace] ranges when we explode them into a [Prev] and a
     [Next] *)
  module Range_with_replaces_info = struct
    type t =
      { hunk_index : int
      ; range_type : [ `Original | `Former_replace of int | `Move ]
      }
  end

  let%template find_moves ~line_big_enough ~keep_ws (hunks : Hunks.t) =
    let minimum_match_perc = 0.7 in
    let minimum_lines = 3 in
    (* Rewrite [Replace] ranges as a [Prev] and [Next] so we can consider them for moves.
       Extract all ranges from the hunks so they are easier to work with *)
    let all_ranges = Queue.create () in
    let replace_id = ref 0 in
    List.iteri hunks ~f:(fun hunk_index hunk ->
      List.iter hunk.ranges ~f:(fun range ->
        match range with
        | Replace (prev, next, None) ->
          Queue.enqueue
            all_ranges
            ( { Range_with_replaces_info.hunk_index
              ; range_type = `Former_replace !replace_id
              }
            , Patience_diff.Range.Prev (prev, None) );
          Queue.enqueue
            all_ranges
            ( { Range_with_replaces_info.hunk_index
              ; range_type = `Former_replace !replace_id
              }
            , Patience_diff.Range.Next (next, None) );
          Int.incr replace_id
        | _ ->
          Queue.enqueue
            all_ranges
            ({ Range_with_replaces_info.hunk_index; range_type = `Original }, range)));
    let prev_ranges = Queue.create () in
    let next_ranges =
      Pairing_heap.create ~cmp:(Comparable.lift Range_info.compare_by_size ~f:fst) ()
    in
    Queue.iteri all_ranges ~f:(fun range_index (replace_info, range) ->
      let replace_id =
        match replace_info.range_type with
        | `Former_replace id -> Some id
        | `Move | `Original -> None
      in
      match range with
      | Prev (range_contents, None) when Array.length range_contents >= minimum_lines ->
        Queue.enqueue
          prev_ranges
          ( { Range_info.range_index
            ; size_of_range = Array.sum (module Int) ~f:String.length range_contents
            ; replace_id
            }
          , range_contents )
      | Next (range_contents, None) when Array.length range_contents >= minimum_lines ->
        Pairing_heap.add
          next_ranges
          ( { Range_info.range_index
            ; size_of_range = Array.sum (module Int) ~f:String.length range_contents
            ; replace_id
            }
          , range_contents )
      | _ -> ());
    if Queue.length prev_ranges * Pairing_heap.length next_ranges > 40_000
    then (* bail out if too expensive *)
      hunks
    else (
      let prevs_used = Range_info.Table.create () in
      let nexts_to_replace = Range_info.Table.create () in
      (* Find ranges that are similar enough to be moves *)
      let next_ranges =
        Array.init (Pairing_heap.length next_ranges) ~f:(fun _ ->
          Pairing_heap.pop_exn next_ranges)
      in
      let move_id = ref Patience_diff.Move_id.zero in
      Queue.iter prev_ranges ~f:(fun (prev_location, prev_contents) ->
        let starting_index =
          Array.binary_search
            next_ranges
            ~compare:(fun (next_range_info, _next_contents) prev_range_info ->
              Range_info.compare_by_size next_range_info prev_range_info)
            `Last_less_than_or_equal_to
            prev_location
          |> (Option.value [@mode local]) ~default:(Array.length next_ranges - 1)
        in
        let starting_index = if starting_index < 0 then 0 else starting_index in
        let left_index = ref starting_index in
        let right_index = ref (starting_index + 1) in
        let max_similarity range_a range_b =
          let a_size = Int.to_float range_a.Range_info.size_of_range in
          let b_size = Int.to_float range_b.Range_info.size_of_range in
          Float.min a_size b_size /. Float.max a_size b_size
        in
        let next_closest_range () =
          let left_range =
            if !left_index < 0 || !left_index >= Array.length next_ranges
            then None
            else Some next_ranges.(!left_index)
          in
          let right_range =
            if !right_index < 0 || !right_index >= Array.length next_ranges
            then None
            else Some next_ranges.(!right_index)
          in
          match left_range, right_range with
          | None, None -> None
          | Some left_range, None ->
            Int.decr left_index;
            Some left_range
          | None, Some right_range ->
            Int.incr right_index;
            Some right_range
          | Some (left_info, left_range), Some (right_info, right_range) ->
            if Float.compare
                 (max_similarity left_info prev_location)
                 (max_similarity right_info prev_location)
               >= 0
            then (
              Int.decr left_index;
              Some (left_info, left_range))
            else (
              Int.incr right_index;
              Some (right_info, right_range))
        in
        let rec find_best_next_range best_match_so_far =
          let finish () =
            match best_match_so_far with
            | None -> ()
            | Some (_, select_hunk) -> select_hunk ()
          in
          match next_closest_range () with
          | None -> finish ()
          | Some (next_location, next_contents) ->
            let max_similarity = max_similarity prev_location next_location in
            (* If this range can't possibly have the required similarity then none of the
               subsequent ranges can either so stop our search here *)
            if Float.(max_similarity < minimum_match_perc)
               ||
               match best_match_so_far with
               | None -> false
               | Some (best_match_ratio, _) -> Float.(max_similarity < best_match_ratio)
            then finish ()
            else if Hashtbl.mem nexts_to_replace next_location
                    (* Don't use the two parts of a the same replace for moves *)
                    ||
                    match next_location.replace_id, prev_location.replace_id with
                    | Some next_id, Some prev_id when next_id = prev_id -> true
                    | _ -> false
            then find_best_next_range best_match_so_far
            else (
              let match_ratio =
                (* [match_ratio] just does naive string comparisons per line so strip
                   leading and trailing whitespace. *)
                Patience_diff.String.match_ratio
                  (Array.map ~f:String.strip prev_contents)
                  (Array.map ~f:String.strip next_contents)
              in
              let select_hunk () =
                let hunk =
                  let transform = if keep_ws then Fn.id else remove_ws in
                  Patience_diff.String.get_hunks
                    ~transform
                    ~context:(-1)
                    ~big_enough:line_big_enough
                    ~max_slide:100
                    ~score:score_line
                    ~prev:prev_contents
                    ~next:next_contents
                    ()
                  (* Negative [context] returns a singleton hunk *)
                  |> List.hd_exn
                in
                let move_index = !move_id in
                Hashtbl.add_exn
                  prevs_used
                  ~key:prev_location
                  ~data:(move_index, None, None);
                move_id := Patience_diff.Move_id.succ !move_id;
                let num_ranges = List.length hunk.ranges in
                let range_index_is_on_edge range_index =
                  range_index = 0 || range_index = num_ranges - 1
                in
                Hashtbl.add_exn
                  nexts_to_replace
                  ~key:next_location
                  ~data:
                    (List.filter_mapi hunk.ranges ~f:(fun range_index_within_move range ->
                       match range with
                       | Same contents ->
                         Some
                           (Patience_diff.Range.Next
                              (Array.map ~f:snd contents, Some (Move move_index)))
                       | Replace (prev, next, _) ->
                         Some (Replace (prev, next, Some move_index))
                       | Prev (prev, _) ->
                         if range_index_is_on_edge range_index_within_move
                         then (
                           Hashtbl.update prevs_used prev_location ~f:(function
                             | Some (move_index, beg_lines, end_lines) ->
                               ( move_index
                               , (if range_index_within_move = 0
                                  then Some (Array.length prev)
                                  else beg_lines)
                               , if range_index_within_move = num_ranges - 1
                                 then Some (Array.length prev)
                                 else end_lines )
                             (* We should have added this prev range above *)
                             | None -> assert false);
                           None)
                         else Some (Prev (prev, Some (Within_move move_index)))
                       | Next (next, _) ->
                         Some
                           (Next
                              ( next
                              , if range_index_is_on_edge range_index_within_move
                                then None
                                else Some (Within_move move_index) ))
                       | Unified (contents, _) ->
                         Some (Unified (contents, Some move_index))))
              in
              let best_match_so_far =
                match best_match_so_far with
                | None when Float.(match_ratio >= minimum_match_perc) ->
                  Some (match_ratio, select_hunk)
                | None -> None
                | Some (best_match_ratio, _) ->
                  if Float.(match_ratio > best_match_ratio)
                  then Some (match_ratio, select_hunk)
                  else best_match_so_far
              in
              find_best_next_range best_match_so_far)
        in
        find_best_next_range None);
      let prevs_by_range_index =
        Hashtbl.to_alist prevs_used
        |> List.map ~f:(fun (range_info, move_info) ->
          range_info.Range_info.range_index, move_info)
        |> Int.Table.of_alist_exn
      in
      let nexts_by_range_index =
        Hashtbl.to_alist nexts_to_replace
        |> List.map ~f:(fun (range_info, ranges_to_insert) ->
          range_info.Range_info.range_index, ranges_to_insert)
        |> Int.Table.of_alist_exn
      in
      (* update the [Next] ranges *)
      let ranges =
        Queue.mapi all_ranges ~f:(fun range_index (range_data, range) ->
          match
            ( Hashtbl.find prevs_by_range_index range_index
            , Hashtbl.find nexts_by_range_index range_index )
          with
          (* This means we think the range is both a next and prev which is impossible *)
          | Some _, Some _ -> assert false
          | None, None -> [ range_data, range ]
          | Some (move_id, lines_to_trim_at_beg, lines_to_trim_at_end), None ->
            (match range with
             | Patience_diff.Range.Prev (contents, None) ->
               let lines_to_trim_at_beg = Option.value lines_to_trim_at_beg ~default:0 in
               let lines_to_trim_at_end = Option.value lines_to_trim_at_end ~default:0 in
               List.filter_opt
                 [ (if lines_to_trim_at_beg = 0
                    then None
                    else
                      Some
                        ( { range_data with range_type = `Original }
                        , Patience_diff.Range.Prev
                            (Array.sub contents ~pos:0 ~len:lines_to_trim_at_beg, None) ))
                 ; Some
                     ( { range_data with range_type = `Move }
                     , Patience_diff.Range.Prev
                         ( Array.sub
                             contents
                             ~pos:lines_to_trim_at_beg
                             ~len:
                               (Array.length contents
                                - lines_to_trim_at_beg
                                - lines_to_trim_at_end)
                         , Some (Move move_id) ) )
                 ; (if lines_to_trim_at_end = 0
                    then None
                    else
                      Some
                        ( { range_data with range_type = `Original }
                        , Patience_diff.Range.Prev
                            ( Array.sub
                                contents
                                ~pos:(Array.length contents - lines_to_trim_at_end)
                                ~len:lines_to_trim_at_end
                            , None ) ))
                 ]
             | _ ->
               (* we should never reference anything except a [Prev] that hasn't been
                  moved *)
               assert false)
          | None, Some ranges_to_replace ->
            let range_data = { range_data with range_type = `Move } in
            List.map ranges_to_replace ~f:(fun range -> range_data, range))
        |> Queue.to_list
        |> List.concat
      in
      (* Recover any [Replace] ranges we broke up if we didn't use them for moves. *)
      let final_ranges = Queue.create () in
      let rec recover_replaces = function
        | ( { Range_with_replaces_info.range_type = `Former_replace _; hunk_index }
          , Patience_diff.Range.Prev (prev, None) )
          :: ( { Range_with_replaces_info.range_type = `Former_replace _; hunk_index = _ }
             , Next (next, None) )
          :: rest_ranges ->
          Queue.enqueue
            final_ranges
            ( { Range_with_replaces_info.range_type = `Original; hunk_index }
            , Patience_diff.Range.Replace (prev, next, None) );
          recover_replaces rest_ranges
        | range :: rest_ranges ->
          Queue.enqueue final_ranges range;
          recover_replaces rest_ranges
        | [] -> ()
      in
      recover_replaces ranges;
      (* Place the ranges in the correct hunks *)
      let final_hunks =
        List.mapi hunks ~f:(fun hunk_index hunk ->
          let ranges =
            let hunk_ranges = Queue.create () in
            Queue.drain
              final_ranges
              ~f:(fun (_, range) -> Queue.enqueue hunk_ranges range)
              ~while_:(fun (range_data, _) ->
                range_data.Range_with_replaces_info.hunk_index = hunk_index);
            Queue.to_list hunk_ranges
          in
          { hunk with ranges })
      in
      final_hunks)
  ;;

  let diff ~context ~line_big_enough ~keep_ws ~find_moves:should_find_moves ~prev ~next =
    let transform = if keep_ws then Fn.id else remove_ws in
    Patience_diff.String.get_hunks
      ~transform
      ~context
      ~big_enough:line_big_enough
      ~max_slide:100
      ~score:score_line
      ~prev
      ~next
      ()
    |> fun hunks ->
    if should_find_moves then find_moves ~line_big_enough ~keep_ws hunks else hunks
  ;;

  type word_or_newline =
    [ `Newline of int * string option (* (number of newlines, subsequent_whitespace) *)
    | `Word of string
    ]
  [@@deriving sexp_of]

  (* Splits an array of lines into an array of pieces (`Newlines and R.Words) *)
  let explode_internal words ~keep_ws =
    let to_words l = List.map l ~f:(fun s -> `Word s) in
    (* [`Newline of (int * string option)]

       can be thought of as:

       [`Newline of ([`How_many_consecutive_newlines of int] * [`Some_subsequent_whitespace of string |`Empty_string ])]

       This representation is used to try to collapse consecutive whitespace as tightly as
       possible, but it's not a great abstraction, so some consecutive whitespace does not
       get collapsed.
    *)
    let words =
      List.concat_map words ~f:(fun x ->
        match x with
        | hd :: tl ->
          if keep_ws && (not (String.is_empty hd)) && is_ws hd
          then `Newline (1, Some hd) :: to_words tl
          else `Newline (1, None) :: `Word hd :: to_words tl
        | [] -> [ `Newline (1, None) ])
    in
    let words =
      List.fold_right words ~init:[] ~f:(fun x acc ->
        (* look back at what we've accumulated so far to see if there's any whitespace
           that can be collapsed. *)
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
      | _ -> words
    in
    (* Append a newline to the end, if this array has any words *)
    let words =
      match words with
      | [] -> []
      (* Don't drop this section if it includes only a single newline. *)
      | list -> List.append list [ `Newline (1, None) ]
    in
    Array.of_list words
  ;;

  let explode ar ~keep_ws =
    let ar = Array.to_list ar in
    let words =
      if keep_ws
      then List.map ar ~f:(split ~keep_ws)
      else List.map ar ~f:whitespace_ignorant_split
    in
    explode_internal words ~keep_ws
  ;;

  let collapse_structured ranges ~kind =
    (* flag indicates what kind of range is currently being collapsed *)
    let flag = ref `Same in
    (* segment is the current series of words being processed. *)
    let segment = ref [] in
    (* line is the current series of formatted segments *)
    let line = ref [] in
    (* lines is the return array *)
    let lines = ref [] in
    (*
       * Finish the current segment by applying the appropriate format
     * and popping it on to the end of the current line
    *)
    let finish_segment () =
      let rule =
        match !flag with
        | `Same -> fun str -> `Same, str
        | `Prev -> fun str -> `Prev, str
        | `Next -> fun str -> `Next, str
      in
      let formatted_segment = List.rev !segment |> String.concat |> rule in
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
        lines := List.rev !line :: !lines;
        line := []
      done
    in
    let f range =
      (* Extract the array, set flag appropriately, *)
      let ar =
        match (range : _ Patience_diff.Range.t) with
        | Same ar ->
          flag := `Same;
          (* R.Same ar is an array of tuples.  The first tuple is an
           * element from the old file, the second tuple, an element
           * from the new file.  Depending on what kind of collapse
           * this is, we want only one or the other. *)
          let f =
            match kind with
            | `Prev_only -> fst
            | `Next_only -> snd
            | `Unified -> snd
          in
          Array.map ar ~f
        | Prev (ar, _) ->
          flag := `Prev;
          ar
        | Next (ar, _) ->
          flag := `Next;
          ar
        | Replace _ | Unified _ ->
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
     | [] | [ (_, "") ] -> ()
     | line ->
       let line = List.rev line in
       if is_ws (List.map ~f:snd line |> String.concat)
       then
         (* This branch was unreachable in our regression tests, but I can't prove it's
            unreachable in all cases. Rather than raise in production, let's drop this
            whitespace. *)
         ()
       else
         raise_s
           [%message
             "Invariant violated: [collapse] got a line not terminated with a newline"
               (line : ([ `Next | `Prev | `Same ] * string) list)]);
    Array.of_list (List.rev !lines)
  ;;

  (* Takes hunks of `Words and `Newlines and collapses them back into lines,
   * formatting appropriately. *)
  let collapse ranges ~rule_same ~rule_prev ~rule_next ~kind ~output =
    let collapsed = collapse_structured ranges ~kind in
    let apply ~rule = function
      | "" -> ""
      | s -> Output_ops.Rule.apply s ~rule ~output ~refined:false
    in
    Array.map collapsed ~f:(function line ->
      List.map line ~f:(function
        | `Same, word -> apply ~rule:rule_same word
        | `Prev, word -> apply ~rule:rule_prev word
        | `Next, word -> apply ~rule:rule_next word)
      |> String.concat)
  ;;

  (* Get the hunks from two arrays of pieces (`Words and `Newlines) *)
  let diff_pieces ~prev_pieces ~next_pieces ~keep_ws ~word_big_enough =
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
      ~max_slide:0
      ~prev:prev_pieces
      ~next:next_pieces
      ()
  ;;

  let ranges_are_just_whitespace (ranges : _ Patience_diff.Range.t list) =
    List.for_all ranges ~f:(function
      | Prev (piece_array, _) | Next (piece_array, _) ->
        Array.for_all piece_array ~f:(function
          | `Word s -> String.is_empty (remove_ws s)
          | `Newline _ -> true)
      | _ -> true)
  ;;

  (* Interleaves the display of minus lines and plus lines so that equal words are
     presented close together. There is some heuristic for when we think doing this
     improves the diff. *)
  let split_for_readability rangelist =
    let ans : _ Patience_diff.Range.t list list ref = ref [] in
    let pending_ranges : _ Patience_diff.Range.t list ref = ref [] in
    let append_range range = pending_ranges := range :: !pending_ranges in
    List.iter rangelist ~f:(fun range ->
      let split_was_executed =
        match (range : _ Patience_diff.Range.t) with
        | Next _ | Prev _ | Replace _ | Unified _ -> false
        | Same seq ->
          let first_newline =
            Array.find_mapi seq ~f:(fun i -> function
              | `Word _, _ | _, `Word _ | `Newline (0, _), _ | _, `Newline (0, _) -> None
              | `Newline first_nlA, `Newline first_nlB -> Some (i, first_nlA, first_nlB))
          in
          (match first_newline with
           | None -> false
           | Some (i, first_nlA, first_nlB) ->
             if Array.length seq - i <= Configuration.too_short_to_split
             then false
             else (
               append_range (Same (Array.sub seq ~pos:0 ~len:i));
               (* A non-zero `Newline is required for [collapse] to work properly. *)
               append_range (Same [| `Newline (1, None), `Newline (1, None) |]);
               ans := List.rev !pending_ranges :: !ans;
               pending_ranges := [];
               let suf = Array.sub seq ~pos:i ~len:(Array.length seq - i) in
               let decr_first (x, y) = x - 1, y in
               suf.(0) <- `Newline (decr_first first_nlA), `Newline (decr_first first_nlB);
               append_range (Same suf);
               true))
      in
      if not split_was_executed then append_range range);
    List.rev
      (match !pending_ranges with
       | [] -> !ans
       | _ :: _ as ranges -> List.rev ranges :: !ans)
  ;;

  (* Turn lines into words and apply a heuristic to break up large blocks of lines by
     relatively unique words. *)
  let words_break_hueristic prev_ar next_ar ~keep_ws =
    let target_block_size = 100 in
    let target_num_blocks =
      min (Array.length prev_ar) (Array.length next_ar) / target_block_size
    in
    let prev_words =
      if keep_ws
      then Array.map prev_ar ~f:(split ~keep_ws)
      else Array.map prev_ar ~f:whitespace_ignorant_split
    in
    let next_words =
      if keep_ws
      then Array.map next_ar ~f:(split ~keep_ws)
      else Array.map next_ar ~f:whitespace_ignorant_split
    in
    if Array.length prev_words < target_block_size * 2
       && Array.length next_words < target_block_size
    then
      (* The Replace range is small enough don't apply the heuristic. *)
      [ prev_words, next_words ]
    else (
      (* Count word frequencies. *)
      let word_freq = String.Table.create () in
      Array.iter prev_words ~f:(fun line ->
        List.iter line ~f:(fun word ->
          Hashtbl.update word_freq word ~f:(function
            | None -> 1, 0
            | Some (cnt, other) -> cnt + 1, other)));
      Array.iter next_words ~f:(fun line ->
        List.iter line ~f:(fun word ->
          Hashtbl.update word_freq word ~f:(function
            | None -> 0, 1
            | Some (other, cnt) -> other, cnt + 1)));
      let prev_word_count = Array.sum (module Int) prev_words ~f:List.length in
      let next_word_count = Array.sum (module Int) next_words ~f:List.length in
      let prev_freq_cutoff = Int.of_float (sqrt (Int.to_float prev_word_count)) in
      let next_freq_cutoff = Int.of_float (sqrt (Int.to_float next_word_count)) in
      (* Find all words shared between the two files and sort by least common *)
      let word_breaks_to_consider =
        Hashtbl.to_alist word_freq
        |> List.filter ~f:(fun (_, (cnt_prev, cnt_next)) ->
          cnt_prev > 0
          && cnt_next > 0
          (* Don't break on words that are too common *)
          && cnt_prev < prev_freq_cutoff
          && cnt_next < next_freq_cutoff)
        |> List.sort
             ~compare:
               (Comparable.lift Int.compare ~f:(fun (_, (cnt_prev, cnt_next)) ->
                  cnt_prev + cnt_next))
        |> (fun list -> List.take list target_num_blocks)
        |> List.map ~f:fst
        |> String.Set.of_list
      in
      let line_breaks_in_prev =
        Array.filter_mapi prev_words ~f:(fun i line ->
          match List.find line ~f:(Set.mem word_breaks_to_consider) with
          | None -> None
          | Some word -> Some (word, i))
        |> Array.to_list
        |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
      in
      let line_breaks_in_next =
        Array.filter_mapi next_words ~f:(fun i line ->
          match List.find line ~f:(Set.mem word_breaks_to_consider) with
          | None -> None
          | Some word -> Some (word, i))
        |> Array.to_list
        |> String.Map.of_alist_reduce ~f:(fun first _ -> first)
      in
      let line_breaks_to_consider =
        Map.merge line_breaks_in_prev line_breaks_in_next ~f:(fun ~key:_ merge_element ->
          match merge_element with
          | `Both (prev, next) -> Some (prev, next)
          | `Left _ | `Right _ -> None)
        |> Map.data
        |> List.sort ~compare:(Comparable.lift Int.compare ~f:snd)
      in
      (* Find the actual line numbers to split on *)
      let line_breaks =
        let last_prev = ref (-1) in
        let last_next = ref (-1) in
        List.filter line_breaks_to_consider ~f:(fun (prev, next) ->
          (* Avoid overlapping indices and don't make small segments *)
          if prev > !last_prev
             && prev - !last_prev > target_block_size / 2
             && next - !last_next > target_block_size / 2
          then (
            last_prev := prev;
            last_next := next;
            true)
          else false)
      in
      (* Break up the arrays *)
      let arrays_rev = ref [] in
      let last_prev_i = ref 0 in
      let last_next_i = ref 0 in
      List.iter
        (line_breaks @ [ Array.length prev_ar, Array.length next_ar ])
        ~f:(fun (prev_i, next_i) ->
          arrays_rev
          := ( Array.sub prev_words ~pos:!last_prev_i ~len:(prev_i - !last_prev_i)
             , Array.sub next_words ~pos:!last_next_i ~len:(next_i - !last_next_i) )
             :: !arrays_rev;
          last_prev_i := prev_i;
          last_next_i := next_i);
      List.rev !arrays_rev)
  ;;

  let refine_internal
    (type a)
    ?(mark_newline_changes = false)
    ~map_non_replace
    ~line_is_ws
    ~collapse_range
    ~produce_unified_lines
    ~keep_ws
    ~split_long_lines
    ~interleave
    ~word_big_enough
    (hunks : string Patience_diff.Hunk.t list)
    : a Patience_diff.Hunk.t list
    =
    let handle_replace prev_ar next_ar move_kind =
      (* Explode the arrays *)
      let prev_pieces = explode_internal prev_ar ~keep_ws in
      let next_pieces = explode_internal next_ar ~keep_ws in
      (* Diff the pieces *)
      let sub_diff = diff_pieces ~prev_pieces ~next_pieces ~keep_ws ~word_big_enough in
      (* Smash the hunks' ranges all together *)
      let sub_diff = Patience_diff.Hunks.ranges sub_diff in
      (* Break it up where lines are too long *)
      let sub_diff_pieces =
        if not split_long_lines
        then [ sub_diff ]
        else (
          let max_len = default_refine_line_length in
          (* Accumulates the total length of the line so far, summing lengths of word
             tokens but resetting when newlines are hit *)
          let get_new_len_so_far ~len_so_far tokens_arr =
            Array.fold ~init:len_so_far tokens_arr ~f:(fun len_so_far token ->
              match token with
              | `Newline _ -> 0
              | `Word word -> len_so_far + String.length word)
          in
          (* Iteratively split long lines up. Produces a list of "range lists", where each
             range list should be displayed all together in one unbroken piece before
             being followed by the next range list, etc. *)
          let rec split_lines len_so_far sub_diff rangeaccum rangelistaccum =
            match sub_diff with
            | [] ->
              (match rangeaccum with
               | [] -> List.rev rangelistaccum
               | _ -> List.rev (List.rev rangeaccum :: rangelistaccum))
            (* More tokens ranges left to process *)
            | range :: rest ->
              (match (range : _ Patience_diff.Range.t) with
               | Same tokenpairs_arr ->
                 let range_of_tokens tokenpairs =
                   Patience_diff.Range.Same (Array.of_list tokenpairs)
                 in
                 (* Keep taking tokens until we exceed max_len or hit a newline. Returns
                    (new len_so_far, new range, remaining tokens, hit newline)
                 *)
                 let rec take_until_max len_so_far tokenpairs accum =
                   match tokenpairs with
                   | [] -> len_so_far, range_of_tokens (List.rev accum), [], false
                   | ((token, _) as tokenpair) :: rest ->
                     (match token with
                      | `Newline _ ->
                        0, range_of_tokens (List.rev (tokenpair :: accum)), rest, true
                      | `Word word ->
                        let wordlen = String.length word in
                        if wordlen + len_so_far > max_len && len_so_far > 0
                        then 0, range_of_tokens (List.rev accum), tokenpairs, false
                        else
                          take_until_max (wordlen + len_so_far) rest (tokenpair :: accum))
                 in
                 let make_newline () =
                   Patience_diff.Range.Same [| `Newline (1, None), `Newline (1, None) |]
                 in
                 (* Keep taking ranges until all tokens exhausted. Returns (new
                    len_so_far, range list) *)
                 let rec take_ranges_until_exhausted len_so_far tokenpairs accum =
                   match tokenpairs with
                   | [] -> len_so_far, List.rev accum
                   | _ ->
                     let new_len_so_far, new_range, new_tokenpairs, hit_newline =
                       take_until_max len_so_far tokenpairs []
                     in
                     let new_accum = `Range new_range :: accum in
                     (* If there are token pairs left, that means we hit the max_len, so
                        add a break at this point *)
                     let new_accum =
                       match new_tokenpairs with
                       | _ :: _ when not hit_newline ->
                         `Break :: `Range (make_newline ()) :: new_accum
                       | _ -> new_accum
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
               | Next (tokens_arr, _) | Prev (tokens_arr, _) ->
                 let new_len_so_far = get_new_len_so_far ~len_so_far tokens_arr in
                 split_lines new_len_so_far rest (range :: rangeaccum) rangelistaccum
               | Replace (prev_arr, next_arr, _move_kind) ->
                 let new_len_so_far =
                   Int.max
                     (get_new_len_so_far ~len_so_far prev_arr)
                     (get_new_len_so_far ~len_so_far next_arr)
                 in
                 split_lines new_len_so_far rest (range :: rangeaccum) rangelistaccum
               | Unified _ -> assert false)
          in
          split_lines 0 sub_diff [] [])
      in
      let sub_diff_pieces =
        if interleave
        then List.concat_map sub_diff_pieces ~f:split_for_readability
        else sub_diff_pieces
      in
      List.concat_map sub_diff_pieces ~f:(fun sub_diff ->
        let sub_prev = Patience_diff.Range.prev_only sub_diff in
        let sub_next = Patience_diff.Range.next_only sub_diff in
        let all_same ranges =
          List.for_all ranges ~f:(fun range ->
            match (range : _ Patience_diff.Range.t) with
            | Same _ -> true
            | Prev (a, _) | Next (a, _) ->
              if keep_ws
              then false
              else
                Array.for_all a ~f:(function
                  | `Newline _ -> true
                  | `Word _ -> false)
            | _ -> false)
        in
        let prev_all_same = all_same sub_prev in
        let next_all_same = all_same sub_next in
        let produce_unified_lines =
          produce_unified_lines
          && (((not (ranges_are_just_whitespace sub_prev)) && next_all_same)
              || ((not (ranges_are_just_whitespace sub_next)) && prev_all_same))
        in
        (* Collapse the pieces back into lines *)
        let prev_next_pairs =
          collapse_range ~prev_all_same ~next_all_same ~sub_prev ~sub_next ~move_kind
        in
        List.map prev_next_pairs ~f:(fun (prev_ar, next_ar) ->
          let range : _ Patience_diff.Range.t =
            match prev_all_same, next_all_same, move_kind with
            (* Don't create a same range inside of a move *)
            | true, true, None ->
              (* Changes that are purely newlines are ignored when [keep_ws] is [False],
                 but codemirror needs this info to track line numbers in side by side
                 diffs. *)
              if mark_newline_changes
                 && not (equal (Array.length prev_ar) (Array.length next_ar))
              then Replace (prev_ar, next_ar, move_kind)
              else Same (Array.map next_ar ~f:(fun x -> x, x))
            | _ ->
              (match prev_ar, next_ar with
               (* Ugly hack that takes care of empty files *)
               | [| line |], next_ar when line_is_ws line ->
                 Replace ([||], next_ar, move_kind)
               | prev_ar, [| line |] when line_is_ws line ->
                 Replace (prev_ar, [||], move_kind)
               | prev_ar, next_ar ->
                 (match produce_unified_lines, prev_all_same, next_all_same with
                  | true, true, false -> Unified (next_ar, move_kind)
                  | true, false, true -> Unified (prev_ar, move_kind)
                  | false, _, _ | _, false, false -> Replace (prev_ar, next_ar, move_kind)
                  | _ -> assert false))
          in
          range))
    in
    let refine_range : _ Patience_diff.Range.t -> a Patience_diff.Range.t list = function
      | (Next _ | Prev _ | Same _ | Unified _) as range -> map_non_replace ~keep_ws range
      | Replace (prev_ar, next_ar, move_kind) ->
        let arrays = words_break_hueristic prev_ar next_ar ~keep_ws in
        List.concat_map arrays ~f:(fun (prev_ar, next_ar) ->
          handle_replace (Array.to_list prev_ar) (Array.to_list next_ar) move_kind)
    in
    hunks
    |> List.map ~f:(fun hunk ->
      { hunk with ranges = List.concat_map hunk.ranges ~f:refine_range })
    |> List.filter ~f:(not << Patience_diff.Hunk.all_same)
  ;;

  let refine_structured
    ?mark_newline_changes
    ~produce_unified_lines
    ~keep_ws
    ~split_long_lines
    ~interleave
    ~word_big_enough
    (hunks : string Patience_diff.Hunk.t list)
    =
    refine_internal
      ?mark_newline_changes
      ~produce_unified_lines
      ~line_is_ws:(const false)
      ~keep_ws
      ~split_long_lines
      ~interleave
      ~word_big_enough
      hunks
      ~map_non_replace:(fun ~keep_ws range ->
        match range with
        | Next (a, None) when (not keep_ws) && Array.for_all a ~f:is_ws ->
          [ Same (Array.map a ~f:(fun line -> [ `Same, line ], [ `Same, line ])) ]
        | Prev (a, None) when (not keep_ws) && Array.for_all a ~f:is_ws -> []
        | Next (lines, move_info) ->
          [ Next (Array.map lines ~f:(fun line -> [ `Same, line ]), move_info) ]
        | Prev (lines, move_info) ->
          [ Prev (Array.map lines ~f:(fun line -> [ `Same, line ]), move_info) ]
        | Same lines ->
          [ Same
              (Array.map lines ~f:(fun (prev, next) -> [ `Same, prev ], [ `Same, next ]))
          ]
        | Unified (lines, move_info) ->
          [ Unified (Array.map lines ~f:(fun line -> [ `Same, line ]), move_info) ]
        | Replace _ -> assert false)
      ~collapse_range:
        (fun
          ~prev_all_same ~next_all_same ~sub_prev ~sub_next ~move_kind:_ ->
        match prev_all_same, next_all_same with
        | true, true ->
          let prev_ar = collapse_structured sub_prev ~kind:`Prev_only in
          let next_ar = collapse_structured sub_next ~kind:`Next_only in
          [ prev_ar, next_ar ]
        | true, false | false, true | false, false ->
          let kind = `Prev_only in
          let prev_ar = collapse_structured sub_prev ~kind in
          let kind = `Next_only in
          let next_ar = collapse_structured sub_next ~kind in
          [ prev_ar, next_ar ])
  ;;

  let unrefined_structured (hunks : string Patience_diff.Hunk.t list) =
    let tag_array tag arr = Array.map arr ~f:(fun line -> [ tag, line ]) in
    List.map hunks ~f:(fun hunk ->
      { hunk with
        ranges =
          List.map hunk.ranges ~f:(function
            | Same same ->
              Patience_diff.Range.Same
                (Array.map same ~f:(fun (prev, next) -> [ `Same, prev ], [ `Same, next ]))
            | Prev (prev, move) -> Prev (tag_array `Prev prev, move)
            | Next (next, move) -> Next (tag_array `Next next, move)
            | Replace (prev, next, move) ->
              Replace (tag_array `Prev prev, tag_array `Next next, move)
            | Unified (lines, move) -> Unified (tag_array `Same lines, move))
      })
  ;;

  (* Refines the diff, splitting the lines into smaller arrays and diffing them, then
     collapsing them back into their initial lines after applying a format. *)
  let refine
    ~(rules : Format.Rules.t)
    ~produce_unified_lines
    ~output
    ~keep_ws
    ~split_long_lines
    ~interleave
    ~word_big_enough
    (hunks : string Patience_diff.Hunk.t list)
    =
    let rule_prev = rules.word_prev in
    let rule_next = rules.word_next in
    let collapse = collapse ~rule_prev ~rule_next ~output in
    let () =
      match output with
      | Ansi | Html -> ()
      | Ascii ->
        if produce_unified_lines
        then failwith "produce_unified_lines is not supported in Ascii mode"
    in
    refine_internal
      ~produce_unified_lines
      ~line_is_ws:(fun line -> String.equal line "")
      ~keep_ws
      ~split_long_lines
      ~interleave
      ~word_big_enough
      hunks
      ~map_non_replace:(fun ~keep_ws range ->
        match range with
        | Next (a, _) when (not keep_ws) && Array.for_all a ~f:is_ws ->
          [ Same (Array.zip_exn a a) ]
        | Prev (a, _) when (not keep_ws) && Array.for_all a ~f:is_ws -> []
        | (Next _ | Prev _ | Same _ | Unified _) as range -> [ range ]
        | Replace _ -> assert false)
      ~collapse_range:(fun ~prev_all_same ~next_all_same ~sub_prev ~sub_next ~move_kind ->
        match prev_all_same, next_all_same with
        | true, true ->
          let rule_same =
            match move_kind with
            | None -> rules.word_same_unified
            | Some _ -> rules.word_same_unified_in_move
          in
          (match move_kind with
           | None ->
             let next_ar = collapse sub_next ~rule_same ~kind:`Next_only in
             [ next_ar, next_ar ]
           | Some _ ->
             let prev_ar = collapse sub_prev ~rule_same ~kind:`Prev_only in
             let next_ar = collapse sub_next ~rule_same ~kind:`Next_only in
             [ prev_ar, next_ar ])
        | false, true ->
          let kind = `Prev_only in
          let rule_same =
            if produce_unified_lines
            then (
              match move_kind with
              | None -> rules.word_same_unified
              | Some _ -> rules.word_same_unified_in_move)
            else rules.word_same_prev
          in
          let prev_ar = collapse sub_prev ~rule_same ~kind in
          let kind = `Next_only in
          let rule_same = rules.word_same_next in
          let next_ar = collapse sub_next ~rule_same ~kind in
          [ prev_ar, next_ar ]
        | true, false ->
          let kind = `Next_only in
          let rule_same =
            if produce_unified_lines
            then (
              match move_kind with
              | None -> rules.word_same_unified
              | Some _ -> rules.word_same_unified_in_move)
            else rules.word_same_next
          in
          let next_ar = collapse sub_next ~rule_same ~kind in
          let kind = `Prev_only in
          let rule_same = rules.word_same_prev in
          let prev_ar = collapse sub_prev ~rule_same ~kind in
          [ prev_ar, next_ar ]
        | false, false ->
          let kind = `Prev_only in
          let rule_same = rules.word_same_prev in
          let prev_ar = collapse sub_prev ~rule_same ~kind in
          let kind = `Next_only in
          let rule_same = rules.word_same_next in
          let next_ar = collapse sub_next ~rule_same ~kind in
          [ prev_ar, next_ar ])
  ;;

  let print_unified ~file_names ~rules ~output ~location_style hunks =
    Output_ops.Single_column.print_unified
      hunks
      ~rules
      ~output
      ~file_names
      ~print:(Printf.printf "%s\n")
      ~location_style
      ~print_global_header:true
  ;;

  let build_unified = Output_ops.Single_column.build_unified
  let build_side_by_side = Output_ops.Double_column.build_side_by_side

  let print_side_by_side
    ?width_override
    ~file_names
    ~rules
    ~wrap_or_truncate
    ~output
    hunks
    =
    Output_ops.Double_column.print_side_by_side
      ?width_override
      ~file_names
      ~rules
      ~wrap_or_truncate
      ~output
      ~print:(Printf.printf "%s\n")
      hunks
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
    Output_ops.Single_column.print_unified
      hunks
      ~file_names
      ~location_style
      ~output
      ~print_global_header
      ~print:(Queue.enqueue buf)
      ~rules;
    String.concat (Queue.to_list buf) ~sep:"\n"
  ;;

  let output_width = Output_ops.output_width

  let output_to_string_side_by_side
    ?width_override
    ~file_names
    ~rules
    ~wrap_or_truncate
    ~output
    hunks
    =
    let prev_file, next_file = file_names in
    let line_num_width =
      String.length (Int.to_string (Output_ops.Double_column.max_line_number hunks)) + 1
    in
    let pane_cols = (output_width ?width_override () - 1) / 2 in
    let filename_lines =
      Output_ops.Double_column.render_filenames
        ~prev_file
        ~next_file
        ~pane_cols
        ~output
        ~rules
        ~line_num_width
    in
    let lines =
      Output_ops.Double_column.build_side_by_side
        ?width_override
        hunks
        ~rules
        ~wrap_or_truncate
        ~output
      |> List.concat
    in
    let middle_divider = Output_ops.Double_column.make_divider output in
    let body_lines =
      List.map lines ~f:(fun (left, right) ->
        String.concat [ left; middle_divider; right ])
    in
    String.concat ~sep:"\n" (filename_lines @ body_lines)
  ;;

  let patdiff
    ?(context = Configuration.default_context)
    ?(keep_ws = false)
    ?(find_moves = false)
    ?(rules = Format.Rules.default)
    ?(output = Output.Ansi)
    ?(produce_unified_lines = true)
    ?(split_long_lines = true)
    ?print_global_header
    ?(location_style = Format.Location_style.Diff)
    ?(interleave = true)
    ?float_tolerance
    ?(line_big_enough = Configuration.default_line_big_enough)
    ?(word_big_enough = Configuration.default_word_big_enough)
    ~(prev : Diff_input.t)
    ~(next : Diff_input.t)
    ()
    =
    let keep_ws = keep_ws || Should_keep_whitespace.for_diff ~prev ~next in
    let hunks =
      diff
        ~context
        ~keep_ws
        ~find_moves
        ~line_big_enough
        ~prev:(List.to_array (String.split_lines prev.text))
        ~next:(List.to_array (String.split_lines next.text))
      |> refine
           ~rules
           ~produce_unified_lines
           ~output
           ~keep_ws
           ~split_long_lines
           ~interleave
           ~word_big_enough
    in
    let hunks =
      match float_tolerance with
      | None -> hunks
      | Some tolerance -> Float_tolerance.apply hunks tolerance ~context
    in
    output_to_string
      ?print_global_header
      ~file_names:(Fake prev.name, Fake next.name)
      ~rules
      ~output
      ~location_style
      hunks
  ;;
end

module Without_unix = Make (struct
    let console_width () = Ok default_double_column_width

    let implementation : Output.t -> (module Output.S) = function
      | Ansi -> (module Ansi_output)
      | Ascii -> (module Ascii_output)
      | Html -> (module Html_output.Without_mtime)
    ;;
  end)

module Private = struct
  module Make = Make
end
