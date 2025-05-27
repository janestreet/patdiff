open! Core
open Patience_diff_lib.Patience_diff

type tag =
  [ `Next
  | `Prev
  | `Same
  ]
[@@deriving sexp_of]

module Line = struct
  type t =
    { line_number : int option
    ; contents : (tag * Ansi_text.t) list
    }
  [@@deriving sexp_of, fields ~getters]

  let to_string t =
    List.map t.contents ~f:(fun (_, text) -> Ansi_text.to_unstyled text)
    |> Core.String.concat
  ;;

  let styled_string ?(output = Output.Ansi) ~style (t : t) =
    let contents =
      match output with
      | Output.Ansi ->
        List.map t.contents ~f:(fun (tag, text) -> style tag (Ansi_text.to_string text))
      | Ascii | Html ->
        List.map t.contents ~f:(fun (tag, text) -> style tag (Ansi_text.to_unstyled text))
    in
    Core.String.concat contents
  ;;

  let any_non_same (t : t) =
    List.exists t.contents ~f:(fun (tag, _) ->
      match tag with
      | `Next | `Prev -> true
      | `Same -> false)
  ;;

  let empty = { line_number = None; contents = [] }

  let width t =
    List.sum (module Int) t.contents ~f:(fun (_, text) -> Ansi_text.width text)
  ;;

  let wrap t ~width:max_width =
    if max_width <= 1 then raise_s [%sexp "width is too narrow"];
    (* Lines are built in reverse order, as is each individual line. *)
    let rec wrap_ansi_rev text acc =
      if Ansi_text.width text <= max_width
      then text :: acc
      else (
        let prefix, suffix = Ansi_text.split ~pos:max_width text in
        wrap_ansi_rev suffix (prefix :: acc))
    in
    let _, last_line, earlier_lines =
      List.fold
        t.contents
        ~init:(0, [], [])
        ~f:(fun (curr_len, curr_line, prev_lines) (tag, text) ->
          let word_len = Ansi_text.width text in
          if curr_len + word_len <= max_width
          then curr_len + word_len, (tag, text) :: curr_line, prev_lines
          else (
            let word_start, word_rest =
              Ansi_text.split ~pos:(max_width - curr_len) text
            in
            let curr_line = (tag, word_start) :: curr_line in
            let wrapped_rest = wrap_ansi_rev word_rest [] in
            let prev_lines = curr_line :: prev_lines in
            match wrapped_rest with
            | [] -> 0, [], prev_lines
            | [ new_line ] -> Ansi_text.width new_line, [ tag, new_line ], prev_lines
            | newest_line :: rest ->
              let rest = List.map rest ~f:(fun line -> [ tag, line ]) in
              Ansi_text.width newest_line, [ tag, newest_line ], rest @ prev_lines))
    in
    let lines =
      if List.is_empty last_line then earlier_lines else last_line :: earlier_lines
    in
    List.rev lines
    |> List.mapi ~f:(fun i line ->
      let line_number = if i = 0 then t.line_number else None in
      { line_number; contents = List.rev line })
  ;;

  let truncate (t : t) ~width =
    let _, line =
      List.fold_until
        t.contents
        ~init:(0, [])
        ~f:(fun (len, line) (tag, text) ->
          if Ansi_text.width text + len > width
          then (
            let before = Ansi_text.truncate ~width:(width - len) text in
            Stop (len, (tag, before) :: line))
          else Continue (len + Ansi_text.width text, (tag, text) :: line))
        ~finish:Fn.id
    in
    { t with contents = List.rev line }
  ;;
end

module Line_info = struct
  type t =
    | Same of Line.t * Line.t
    | Prev of Line.t * Move_id.t option
    | Next of Line.t * Move_id.t option
  [@@deriving sexp_of]

  let wrap ~width = function
    | Prev (line, mv) -> Line.wrap line ~width |> List.map ~f:(fun ln -> Prev (ln, mv))
    | Next (line, mv) -> Line.wrap line ~width |> List.map ~f:(fun ln -> Next (ln, mv))
    | Same (prev, next) ->
      let prev, next = Line.wrap prev ~width, Line.wrap next ~width in
      let prev_len = List.length prev in
      let next_len = List.length next in
      let extra_prev = Int.max 0 (prev_len - next_len) in
      let extra_next = Int.max 0 (next_len - prev_len) in
      let prev = prev @ List.init extra_next ~f:(fun _ -> Line.empty) in
      let next = next @ List.init extra_prev ~f:(fun _ -> Line.empty) in
      List.map2_exn prev next ~f:(fun prev next -> Same (prev, next))
  ;;

  let truncate ~width = function
    | Same (prev, next) -> Same (Line.truncate prev ~width, Line.truncate next ~width)
    | Prev (prev, move_id) -> Prev (Line.truncate prev ~width, move_id)
    | Next (next, move_id) -> Next (Line.truncate next ~width, move_id)
  ;;

  let lines = function
    | Same (prev, next) -> prev, next
    | Prev (prev, _) -> prev, Line.empty
    | Next (next, _) -> Line.empty, next
  ;;

  let numbers = function
    | Same (prev, next) -> prev.line_number, next.line_number
    | Prev (prev, _) -> prev.line_number, None
    | Next (next, _) -> None, next.line_number
  ;;
end

module Line_index = struct
  module T = struct
    type t =
      { hunk_index : int
      ; line_index : int
      }
    [@@deriving compare ~localize, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

let align_replace_lines
  ~get_prev_line_number
  ~get_next_line_number
  current_hunk_queue
  prev_lines
  next_lines
  =
  let rec loop ~prev_index ~next_index ~num_same_words_prev ~num_same_words_next =
    let prev_line =
      if prev_index < Array.length prev_lines then Some prev_lines.(prev_index) else None
    in
    let next_line =
      if next_index < Array.length next_lines then Some next_lines.(next_index) else None
    in
    match prev_line, next_line with
    | None, None -> ()
    | Some contents, None ->
      Queue.enqueue
        current_hunk_queue
        (Line_info.Prev ({ line_number = get_prev_line_number (); contents }, None));
      loop
        ~prev_index:(prev_index + 1)
        ~next_index
        ~num_same_words_prev
        ~num_same_words_next
    | None, Some contents ->
      Queue.enqueue
        current_hunk_queue
        (Line_info.Next ({ line_number = get_next_line_number (); contents }, None));
      loop
        ~prev_index
        ~next_index:(next_index + 1)
        ~num_same_words_prev
        ~num_same_words_next
    | Some prev_contents, Some next_contents ->
      let num_same contents =
        List.fold contents ~init:0 ~f:(fun cnt (tag, text) ->
          match tag with
          | `Same when not (Ansi_text.is_empty text) -> cnt + 1
          | _ -> cnt)
      in
      let num_same_words_prev_line = num_same prev_contents in
      let num_same_words_next_line = num_same next_contents in
      let new_num_same_words_prev = num_same_words_prev + num_same_words_prev_line in
      let new_num_same_words_next = num_same_words_next + num_same_words_next_line in
      if new_num_same_words_prev <= num_same_words_next
      then (
        Queue.enqueue
          current_hunk_queue
          (Line_info.Prev
             ({ line_number = get_prev_line_number (); contents = prev_contents }, None));
        loop
          ~prev_index:(prev_index + 1)
          ~next_index
          ~num_same_words_prev:new_num_same_words_prev
          ~num_same_words_next)
      else if new_num_same_words_next <= num_same_words_prev
      then (
        Queue.enqueue
          current_hunk_queue
          (Line_info.Next
             ({ line_number = get_next_line_number (); contents = next_contents }, None));
        loop
          ~prev_index
          ~next_index:(next_index + 1)
          ~num_same_words_prev
          ~num_same_words_next:new_num_same_words_next)
      else (
        Queue.enqueue
          current_hunk_queue
          (Line_info.Same
             ( { line_number = get_prev_line_number (); contents = prev_contents }
             , { line_number = get_next_line_number (); contents = next_contents } ));
        loop
          ~prev_index:(prev_index + 1)
          ~next_index:(next_index + 1)
          ~num_same_words_prev:new_num_same_words_prev
          ~num_same_words_next:new_num_same_words_next)
  in
  loop ~prev_index:0 ~next_index:0 ~num_same_words_prev:0 ~num_same_words_next:0
;;

let hunks_to_lines
  (hunks : (tag * string) list Patience_diff_lib.Patience_diff.Hunk.t list)
  =
  let result = Deque.create () in
  let prev_line_number = ref 1 in
  let next_line_number = ref 1 in
  let start_of_move_in_prev = ref Move_id.Map.empty in
  let start_of_move_in_next = ref Move_id.Map.empty in
  let start_non_same_ranges = ref Line_index.Set.empty in
  let in_same = ref false in
  let current_line_index () =
    { Line_index.hunk_index = Deque.length result - 1
    ; line_index =
        Deque.peek_back result
        |> Option.value_map ~default:0 ~f:(fun queue -> Queue.length queue)
    }
  in
  let record_in_same () = in_same := true in
  let record_in_non_same () =
    if !in_same
    then start_non_same_ranges := Set.add !start_non_same_ranges (current_line_index ());
    in_same := false
  in
  let record_move_in_prev move_id =
    start_of_move_in_prev
    := Map.update !start_of_move_in_prev move_id ~f:(function
         | Some start -> start
         | None -> current_line_index ())
  in
  let record_move_in_next move_id =
    start_of_move_in_next
    := Map.update !start_of_move_in_next move_id ~f:(function
         | Some start -> start
         | None -> current_line_index ())
  in
  let get_and_bump line_number_ref =
    let value = !line_number_ref in
    Int.incr line_number_ref;
    value |> Some
  in
  (* Since moves may have been refined we need to remember the next parts of moves so we
     can rewrite the prev part with the changes that we found during refinement. *)
  let nexts_by_move_id = ref Move_id.Map.empty in
  let record_next_move move_id range =
    nexts_by_move_id
    := Map.update !nexts_by_move_id move_id ~f:(function
         | None -> Queue.singleton range
         | Some queue ->
           Queue.enqueue queue range;
           queue)
  in
  let same_to_prev =
    List.map ~f:(fun (tag, word) ->
      match tag with
      | `Same -> `Prev, word
      | _ -> tag, word)
  in
  let same_to_next =
    List.map ~f:(fun (tag, word) ->
      match tag with
      | `Same -> `Next, word
      | _ -> tag, word)
  in
  let to_contents line =
    List.map line ~f:(fun (tag, text) -> tag, Ansi_text.parse text)
  in
  List.iter hunks ~f:(fun hunk ->
    prev_line_number := hunk.prev_start;
    next_line_number := hunk.next_start;
    Deque.enqueue result `back (Queue.create ());
    List.iter hunk.ranges ~f:(fun range ->
      let current_hunk_queue = Deque.peek_back_exn result in
      match range with
      | Same lines ->
        record_in_same ();
        Array.iter lines ~f:(fun (prev_line, next_line) ->
          Queue.enqueue
            current_hunk_queue
            (Line_info.Same
               ( { line_number = get_and_bump prev_line_number
                 ; contents = to_contents prev_line
                 }
               , { line_number = get_and_bump next_line_number
                 ; contents = to_contents next_line
                 } )))
      | Prev (lines, move_kind) ->
        record_in_non_same ();
        (match move_kind with
         | None | Some (Move _) -> ()
         | Some (Within_move move_id) -> record_next_move move_id range);
        Array.iter lines ~f:(fun contents ->
          let contents = to_contents contents in
          match move_kind with
          | None ->
            Queue.enqueue
              current_hunk_queue
              (Line_info.Prev
                 ( { line_number = get_and_bump prev_line_number
                   ; contents = same_to_prev contents
                   }
                 , None ))
          | Some (Move move_id) ->
            record_move_in_prev move_id;
            Queue.enqueue
              current_hunk_queue
              (Line_info.Prev
                 ({ line_number = get_and_bump prev_line_number; contents }, Some move_id))
          | Some (Within_move move_id) -> record_move_in_next move_id)
      | Next (lines, move_kind) ->
        record_in_non_same ();
        (match move_kind with
         | None | Some (Within_move _) -> ()
         | Some (Move move_id) -> record_next_move move_id range);
        Array.iter lines ~f:(fun contents ->
          let contents = to_contents contents in
          Queue.enqueue
            current_hunk_queue
            (match move_kind with
             | None ->
               Line_info.Next
                 ( { line_number = get_and_bump next_line_number
                   ; contents = same_to_next contents
                   }
                 , None )
             | Some (Move move_id) ->
               record_move_in_next move_id;
               Line_info.Next
                 ({ line_number = get_and_bump next_line_number; contents }, Some move_id)
             | Some (Within_move move_id) ->
               record_move_in_next move_id;
               Line_info.Next
                 ( { line_number = get_and_bump next_line_number
                   ; contents = same_to_next contents
                   }
                 , Some move_id )))
      | Replace (prev_lines, next_lines, move_kind) ->
        record_in_non_same ();
        (match move_kind with
         | None ->
           align_replace_lines
             ~get_prev_line_number:(fun () -> get_and_bump prev_line_number)
             ~get_next_line_number:(fun () -> get_and_bump next_line_number)
             current_hunk_queue
             (Array.map prev_lines ~f:to_contents)
             (Array.map next_lines ~f:to_contents)
         | Some move_id ->
           record_next_move move_id range;
           Array.iter next_lines ~f:(fun contents ->
             let contents = to_contents contents in
             record_move_in_next move_id;
             Queue.enqueue
               current_hunk_queue
               (Line_info.Next
                  ({ line_number = get_and_bump next_line_number; contents }, Some move_id)));
           ())
      | Unified _ -> raise_s [%sexp "Cannot turn unified ranges into side by side view"]));
  let hunks = Deque.to_array result |> Array.map ~f:Queue.to_array in
  (* Rewrite prevs that were refined in moves *)
  Map.iteri !nexts_by_move_id ~f:(fun ~key:move_id ~data:ranges ->
    let line_index_to_ammend = ref (Map.find_exn !start_of_move_in_prev move_id) in
    Queue.iter ranges ~f:(fun range ->
      match range with
      | Prev (prev_lines, Some _) | Replace (prev_lines, _, Some _) ->
        (* Replace ranges have been refined already while Prev ranges haven't. We need to
           turn the `Same tags into `Prevs *)
        let rewrite_tags =
          match range with
          | Replace _ -> false
          | _ -> true
        in
        Array.iter prev_lines ~f:(fun line_contents ->
          let line_contents = to_contents line_contents in
          hunks.(!line_index_to_ammend.hunk_index).(!line_index_to_ammend.line_index)
          <- (match
                hunks.(!line_index_to_ammend.hunk_index).(!line_index_to_ammend.line_index)
              with
              | Prev (line, move_info) ->
                Prev
                  ( { line with
                      contents =
                        (if rewrite_tags
                         then same_to_prev line_contents
                         else line_contents)
                    }
                  , move_info )
              | line -> line);
          line_index_to_ammend
          := { !line_index_to_ammend with
               line_index = !line_index_to_ammend.line_index + 1
             })
      | Next (lines, move_kind) ->
        (match move_kind with
         | Some (Move _) ->
           line_index_to_ammend
           := { !line_index_to_ammend with
                line_index = !line_index_to_ammend.line_index + Array.length lines
              }
         | _ -> ())
      | _ -> assert false));
  hunks
;;
