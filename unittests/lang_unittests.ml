open MyStdlib
open OUnit2
open Ounit_general_extensions
open Ounit_extensions
open Optician
open Star_semiring_tree_alignment
open Synth_structs
open Lang
open Lenscontext
open Regex_utilities
open Normalized_lang

let test_regex_compare_empty _ =
  assert_regex_equal
    Regex.empty
    Regex.empty

let test_regex_compare_base _ =
  assert_regex_equal
    (Regex.make_base "s")
    (Regex.make_base "s")

let test_regex_compare_concat _ =
  assert_regex_equal
    (Regex.make_concat (Regex.make_base "a") (Regex.make_base "b"))
    (Regex.make_concat (Regex.make_base "a") (Regex.make_base "b"))

let test_regex_compare_or _ =
  assert_regex_equal
    (Regex.make_or (Regex.make_base "a") (Regex.make_base "b"))
    (Regex.make_or (Regex.make_base "a") (Regex.make_base "b"))

let test_regex_compare_star _ =
  assert_regex_equal
    (Regex.make_star (Regex.make_base "a"))
    (Regex.make_star (Regex.make_base "a"))

let test_regex_compare_closed _ =
  assert_regex_equal
    (Regex.make_closed (Regex.make_base "a"))
    (Regex.make_closed (Regex.make_base "a"))

let regex_compare_suite = "Test Regex compare" >:::
  [
    "test_regex_compare_empty" >:: test_regex_compare_empty;
    "test_regex_compare_base" >:: test_regex_compare_base;
    "test_regex_compare_concat" >:: test_regex_compare_concat;
    "test_regex_compare_or" >:: test_regex_compare_or;
    "test_regex_compare_star" >:: test_regex_compare_star;
    "test_regex_compare_closed" >:: test_regex_compare_closed;
  ]

let _ = run_test_tt_main regex_compare_suite
