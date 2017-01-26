open! Core
open Patdiff_lib
module Shell = Core_extended.Std.Shell

let bench_core_list config =
  let old  = { Patdiff_core.name = "old"; text = Benchmark_inputs.old  } in
  let new_ = { Patdiff_core.name = "new"; text = Benchmark_inputs.new_ } in
  fun () -> Compare_core.diff_strings config ~old ~new_
;;

let%bench_fun "diff_strings no-unified unrefined" =
  Lazy.force Configuration.dark_bg
  |> Configuration.override ~unrefined:true ~produce_unified_lines:false
  |> bench_core_list
;;

let%bench_fun "diff_strings unified unrefined" =
  Lazy.force Configuration.dark_bg
  |> Configuration.override ~unrefined:true ~produce_unified_lines:true
  |> bench_core_list
;;

let%bench_fun "diff_strings unified refined" =
  Lazy.force Configuration.dark_bg
  |> Configuration.override ~unrefined:false ~produce_unified_lines:true
  |> bench_core_list
;;
