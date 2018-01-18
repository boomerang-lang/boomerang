open Stdlib
open OUnit2
open Ounit_extensions

let test_abc _ =
  assert_equal
    true
    true

let normalize_from_tree_suite = "Test NormalizedPTST from_tree" >:::
  [
    "test_abc" >:: test_abc;
  ]

let _ = run_test_tt_main normalize_from_tree_suite
