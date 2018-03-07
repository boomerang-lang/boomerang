open Stdlib
open Ounit_general_extensions
open Optician
open Star_semiring_tree
open Star_semiring_tree_alignment
open Synth_structs
open Lang

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
end

module CompatibilityIntModule =
struct
  include IntModule
  let are_compatible = is_equal %% compare
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
    ~cmp:(AlignmentOptionCost.compare)

let assert_rxtree_equal =
  assert_equal
    ~printer:StarSemiringTreeRep.Tree.show
    ~cmp:StarSemiringTreeRep.Tree.compare

let assert_alignment_equal =
  assert_equal
    ~printer:StarSemiringTreeRep.Alignment.show
    ~cmp:StarSemiringTreeRep.Alignment.compare

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
              Float.compare f1 f2
            else
              1
          | _ -> 1
        end)
