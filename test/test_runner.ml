open Core.Std let _ = _squelch_unused_module_warning_
open OUnit

let () = ignore (run_test_tt_main Test.all)
