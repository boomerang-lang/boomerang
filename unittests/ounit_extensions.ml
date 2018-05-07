open MyStdlib
open Ounit_general_extensions
open Optician
open Star_semiring_tree
open Star_semiring_alignment_greedy
open Synth_structs
open Lang
open Normalized_lang

module BaseIntModule =
struct
  include IntModule
  module Alignment =
  struct
    include IntModule
    let cost c =
      1. -.
      (1. /. (Float.of_int c))
  end
  let get_alignment i1 i2 =
    if i1 = 0 || i2 = 0 then
      None
    else if i1 mod i2 = 0 then
      Some (i1 / i2)
    else if i2 mod i1 = 0 then
      Some (i2 / i1)
    else
      None

  let requires_mapping v = v mod 100 = 0
  module Default = IntModule
  let extract_default _ = failwith "TODO"
  let information_content v = Float.of_int @$ v mod 123
end

module CompatibilityIntModule =
struct
  include IntModule
  let compare x y = compare (x mod 123) (y mod 123)
  let hash_fold_t s x = hash_fold_t s (x mod 123)
  let hash x = hash (x mod 123)
  let are_compatible x y = is_equal (compare (x mod 7) (y mod 7))
  let requires_mapping v = v mod 246 = 0
  module Default = IntModule
  let extract_default _ = failwith "TODO"
end

module IntNormalizedPTST =
  NormalizedPlusTimesStarTreeOf
    (CompatibilityIntModule)
    (CompatibilityIntModule)
    (CompatibilityIntModule)
    (BaseIntModule)

module IPTST = IntNormalizedPTST.NonNormalizedTree

module FromTreeResponse =
  PairOf
    (IntNormalizedPTST)
    (IntNormalizedPTST.NormalizationScript)

let assert_normalized_tree_script_equal =
  assert_equal
    ~printer:(FromTreeResponse.show)
    ~cmp:(FromTreeResponse.compare)

module IntPTSTAlignment =
  PlusTimesStarTreeAlignmentOf
    (CompatibilityIntModule)
    (CompatibilityIntModule)
    (CompatibilityIntModule)
    (BaseIntModule)

module IntPTSTAlignmentOption = OptionOf(IntPTSTAlignment)
module AlignmentOptionCost = PairOf(IntPTSTAlignmentOption)(FloatModule)

let assert_alignment_option_equal =
  assert_equal
    ~printer:(IntPTSTAlignmentOption.show)
    ~cmp:(IntPTSTAlignmentOption.compare)

let assert_alignment_option_cost_equal =
  assert_equal
    ~printer:(AlignmentOptionCost.show)
    ~cmp:(pair_compare
          IntPTSTAlignmentOption.compare
          (fun f1 f2 -> if Float.abs (f1 -. f2) <. 0.000001 then 0 else 1))

let assert_rxtree_equal =
  assert_equal
    ~printer:StarSemiringTreeRep.Tree.show
    ~cmp:StarSemiringTreeRep.Tree.compare

let assert_alignment_equal =
  assert_equal
    ~printer:StarSemiringTreeRep.OptimalAlignment.show
    ~cmp:StarSemiringTreeRep.OptimalAlignment.compare

let assert_exampled_dnf_regex_equal =
  assert_equal
    ~printer:show_exampled_dnf_regex
    ~cmp:compare_exampled_dnf_regex

let assert_exampled_dnf_regex_option_equal =
  assert_equal
    ~printer:(string_of_option show_exampled_dnf_regex)
    ~cmp:(compare_option compare_exampled_dnf_regex)

let assert_lens_equal =
  assert_equal
    ~printer:Lens.show
    ~cmp:(fun l1 l2 -> if Lens.is_eq l1 l2 then 0 else 1)

module LensOption = OptionOf(Lens)

let assert_lens_option_equal =
  assert_equal
    ~printer:LensOption.show
    ~cmp:(fun l1o l2o ->
        begin match (l1o,l2o) with
          | (None,None) -> 0
          | (Some l1, Some l2) ->
            if Lens.is_eq l1 l2 then
              0
            else
              1
          | _ -> 1
        end)

module LensFloatOption = OptionOf(PairOf(Lens)(FloatModule))

let assert_lens_float_option_equal =
  assert_equal
    ~printer:LensFloatOption.show
    ~cmp:(fun l1o l2o ->
        begin match (l1o,l2o) with
          | (None,None) -> 0
          | (Some (l1,f1), Some (l2,f2)) ->
            if Lens.is_eq l1 l2 then
              (if Float.abs (f1 -. f2) <. 0.000001 then 0 else 1)
            else
              1
          | _ -> 1
        end)

let assert_regex_equal =
  assert_equal
    ~printer:Regex.show
    ~cmp:Regex.compare

let assert_stochastic_regex_equal =
  assert_equal
    ~printer:StochasticRegex.show
    ~cmp:StochasticRegex.compare
