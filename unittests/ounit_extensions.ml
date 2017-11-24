open Stdlib
open Ounit_general_extensions
open Symsynth
open Star_semiring_tree

module IntNormalizedPTST =
  NormalizedPlusTimesStarTreeOf
    (IntModule)
    (IntModule)
    (IntModule)
    (IntModule)

module FromTreeResponse =
  PairOf
    (IntNormalizedPTST)
    (IntNormalizedPTST.NormalizationScript)

let assert_normalized_tree_script_equal =
  assert_equal
    ~printer:(FromTreeResponse.show)
    ~cmp:(FromTreeResponse.compare)
