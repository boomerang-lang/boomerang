open Stdlib
open Ounit_general_extensions
open Symsynth
open Star_semiring_tree
open Star_semiring_tree_alignment

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

module IntNormalizedPTST =
  NormalizedPlusTimesStarTreeOf
    (IntModule)
    (IntModule)
    (IntModule)
    (BaseIntModule)

module FromTreeResponse =
  PairOf
    (IntNormalizedPTST)
    (IntNormalizedPTST.NormalizationScript)

let assert_normalized_tree_script_equal =
  assert_equal
    ~printer:(FromTreeResponse.show)
    ~cmp:(FromTreeResponse.compare)

module IntNormalizedPTSTAlignment =
  PlusTimesStarTreeAlignmentOf
    (IntModule)
    (IntModule)
    (IntModule)
    (BaseIntModule)
