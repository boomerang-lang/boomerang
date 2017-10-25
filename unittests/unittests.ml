open OUnit2
open Ounit_general_extensions

let test_othertest _ =
  assert_true
    true

let counters_suite = "test Unit Tests" >:::
  ["test_othertest" >:: test_othertest;
  ]

let _ = run_test_tt_main counters_suite
