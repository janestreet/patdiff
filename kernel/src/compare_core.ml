open! Core
open! Import
include Compare_core_intf

module Make (Patdiff_core_arg : Patdiff_core.S) = struct
  (* Returns a Hunk.t list, ready to be printed *)
  let compare_lines (config : Configuration.t) ~prev ~next =
    (* Create the diff *)
    let context = config.context in
    let keep_ws = config.keep_ws in
    let split_long_lines = config.split_long_lines in
    let line_big_enough = config.line_big_enough in
    let hunks =
      Patdiff_core_arg.diff
        ~context
        ~line_big_enough
        ~keep_ws
        ~find_moves:config.find_moves
        ~prev
        ~next
    in
    let hunks =
      match config.float_tolerance with
      | None -> hunks
      | Some tolerance -> Float_tolerance.apply hunks tolerance ~context
    in
    (* Refine if desired *)
    if config.unrefined
    then (
      (* Turn `Replace ranges into `Prev and `Next ranges.
         `Replace's would otherwise be later interpreted as refined output *)
      let hunks = Patience_diff.Hunks.unified hunks in
      match config.side_by_side with
      | Some _ -> `Structured_hunks (Patdiff_core_arg.unrefined_structured hunks)
      | None -> `Hunks hunks)
    else (
      let rules = config.rules in
      let output = config.output in
      let produce_unified_lines = config.produce_unified_lines in
      let interleave = config.interleave in
      let word_big_enough = config.word_big_enough in
      match config.side_by_side with
      | Some _ ->
        `Structured_hunks
          (Patdiff_core_arg.refine_structured
             ~keep_ws
             ~produce_unified_lines:false
             ~split_long_lines
             ~interleave
             hunks
             ~word_big_enough)
      | None ->
        `Hunks
          (Patdiff_core_arg.refine
             ~rules
             ~output
             ~keep_ws
             ~produce_unified_lines
             ~split_long_lines
             ~interleave
             hunks
             ~word_big_enough))
  ;;

  let diff_strings
    ?print_global_header
    (config : Configuration.t)
    ~(prev : Diff_input.t)
    ~(next : Diff_input.t)
    =
    let lines { Diff_input.name = _; text } = String.split_lines text |> Array.of_list in
    let hunks =
      Comparison_result.create
        config
        ~prev
        ~next
        ~compare_assuming_text:(fun config ~prev ~next ->
          compare_lines config ~prev:(lines prev) ~next:(lines next))
    in
    if Comparison_result.has_no_diff hunks
    then `Same
    else
      `Different
        (match hunks with
         | Binary_same -> assert false
         | Binary_different { prev_is_binary; next_is_binary } ->
           File_helpers.binary_different_message
             ~config
             ~prev_file:(Fake prev.name)
             ~prev_is_binary
             ~next_file:(Fake next.name)
             ~next_is_binary
         | Hunks hunks ->
           Patdiff_core_arg.output_to_string
             hunks
             ?print_global_header
             ~file_names:(Fake prev.name, Fake next.name)
             ~output:config.output
             ~rules:config.rules
             ~location_style:config.location_style
         | Structured_hunks hunks ->
           Patdiff_core_arg.output_to_string_side_by_side
             hunks
             ~file_names:(Fake prev.name, Fake next.name)
             ~output:config.output
             ~rules:config.rules
             ~wrap_or_truncate:(Option.value config.side_by_side ~default:`wrap))
  ;;

  module Private = struct
    let compare_lines = compare_lines
  end
end

module Without_unix = Make (Patdiff_core.Without_unix)
