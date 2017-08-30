open! Core
open! Async

include module type of struct include Expect_test_helpers end

val patdiff
  :  extra_flags : string list
  -> mine : string
  -> other : string
  -> unit Deferred.t
