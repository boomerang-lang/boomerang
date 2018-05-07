open MyStdlib
open OUnit2
open Ounit_general_extensions
open Ounit_extensions
open Optician
open Star_semiring_tree_alignment_optimal
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

let test_regex_info_content_base _ =
  assert_float_equal
    0.
    (Regex.information_content (Regex.make_base "a"))

let test_regex_info_content_or _ =
  assert_float_equal
    1.0
    (Regex.information_content (Regex.make_or (Regex.make_base "a") (Regex.make_base "b")))

let test_regex_info_content_concat _ =
  assert_float_equal
    2.0
    (Regex.information_content
       (Regex.make_concat
          (Regex.make_or (Regex.make_base "a") (Regex.make_base "b"))
          (Regex.make_or (Regex.make_base "a") (Regex.make_base "b"))))

let test_regex_info_content_star_contentless_underneath _ =
  assert_float_equal
    3.6096404744368
    (Regex.information_content (Regex.make_star (Regex.make_base "a")))

let test_regex_info_content_star_or_underneath _ =
  assert_float_equal
    7.6096404744368
    (Regex.information_content
       (Regex.make_star
          (Regex.make_or (Regex.make_base "a") (Regex.make_base "b"))))

let test_regex_info_content_closed_or_underneath _ =
  assert_float_equal
    1.
    (Regex.information_content
       (Regex.make_closed
          (Regex.make_or (Regex.make_base "a") (Regex.make_base "b"))))

let regex_info_content_suite = "Test Regex Information Content" >:::
  [
    "test_regex_info_content_base" >:: test_regex_info_content_base;
    "test_regex_info_content_or" >:: test_regex_info_content_or;
    "test_regex_info_content_concat" >:: test_regex_info_content_concat;
    "test_regex_info_content_star_contentless_underneath" >:: test_regex_info_content_star_contentless_underneath;
    "test_regex_info_content_star_or_underneath" >:: test_regex_info_content_star_or_underneath;
    "test_regex_info_content_closed_or_underneath" >:: test_regex_info_content_closed_or_underneath;
  ]

let test_stochastic_regex_from_regex_multior _ =
  assert_stochastic_regex_equal
    (StochasticRegex.make_or
          (StochasticRegex.make_or
             (StochasticRegex.make_or
                (StochasticRegex.make_base "a")
                (StochasticRegex.make_base "b")
                0.5)
             (StochasticRegex.make_base "c")
             (2. /. 3.))
          (StochasticRegex.make_or
             (StochasticRegex.make_or
                (StochasticRegex.make_base "d")
                (StochasticRegex.make_base "e")
                0.5)
             (StochasticRegex.make_base "f")
             (2. /. 3.))
          0.5)
    (StochasticRegex.from_regex
       (Regex.make_or
          (Regex.make_or
             (Regex.make_or
                (Regex.make_base "a")
                (Regex.make_base "b"))
             (Regex.make_base "c"))
          (Regex.make_or
             (Regex.make_or
                (Regex.make_base "d")
                (Regex.make_base "e"))
             (Regex.make_base "f"))))


let stochastic_regex_from_regex_suite = "Test StochasticRegex From Regex" >:::
  [
    "test_stochastic_regex_from_regex_multior" >:: test_stochastic_regex_from_regex_multior
  ]

let _ = run_test_tt_main stochastic_regex_from_regex_suite
