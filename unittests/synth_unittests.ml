open MyStdlib
open OUnit2
open Ounit_general_extensions
open Ounit_extensions
open Optician
open Star_semiring_alignment_greedy
open Synth_structs
open Lang
open Lenscontext
open Regex_utilities
open Normalized_lang
open Converter

let test_normalize_tree_empty _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Empty,IntNormalizedPTST.NormalizationScript.Empty)
    (IntNormalizedPTST.from_tree IntNormalizedPTST.NonNormalizedTree.Empty)

let test_normalize_tree_base _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_base 12345)
    ,IntNormalizedPTST.NormalizationScript.Nonempty
        (IntNormalizedPTST.NormalizationScript.Base 12345))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Base 12345)))

let test_normalize_tree_star _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_star
          54321
          (IntNormalizedPTST.Nonempty.mk_base 12345,1))
    ,IntNormalizedPTST.NormalizationScript.Nonempty
        (IntNormalizedPTST.NormalizationScript.Star
           (54321
           ,IntNormalizedPTST.NormalizationScript.Base 12345)))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Star
             (54321
             ,IntNormalizedPTST.NonNormalizedTree.Base 12345))))

let test_normalize_tree_plus_nodupes _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_plus
          54321
          [((IntNormalizedPTST.Nonempty.mk_base 12345,1),1.)])
    ,IntNormalizedPTST.NormalizationScript.Nonempty
        (IntNormalizedPTST.NormalizationScript.Plus
           (IntNormalizedPTST.NormalizationScript.PD_NormalizationLabel.make
              ~label:54321
              ~perm:([
                  CountedPermutation.make_element
                    ~old_index:0
                    ~new_index:(0,0)])
           ,[(IntNormalizedPTST.NormalizationScript.Base 12345,1.)])))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Plus
             (54321
             ,[(IntNormalizedPTST.NonNormalizedTree.Base 12345,1.)]))))

let test_normalize_tree_plus_dupes _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_plus
          54321
          [((IntNormalizedPTST.Nonempty.mk_base 12345,2),1. /. 2.)])
    ,IntNormalizedPTST.NormalizationScript.Nonempty
        (IntNormalizedPTST.NormalizationScript.Plus
           (IntNormalizedPTST.NormalizationScript.PD_NormalizationLabel.make
              ~label:54321
              ~perm:([CountedPermutation.make_element
                        ~old_index:0
                        ~new_index:(0,0)
                     ;CountedPermutation.make_element
                         ~old_index:1
                         ~new_index:(0,1)])
           ,[(IntNormalizedPTST.NormalizationScript.Base 12345,1. /. 2.)
            ;(IntNormalizedPTST.NormalizationScript.Base 12345,1. /. 2.)])))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Plus
             (54321
             ,[(IntNormalizedPTST.NonNormalizedTree.Base 12345,1. /. 2.)
              ;(IntNormalizedPTST.NonNormalizedTree.Base 12345,1. /. 2.)]))))

let test_normalize_tree_plus_many_dupes_with_times _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_plus
          54321
          [((IntNormalizedPTST.Nonempty.mk_times 123 [],7),1. /. 7.)])
    ,IntNormalizedPTST.NormalizationScript.Nonempty
        (IntNormalizedPTST.NormalizationScript.Plus
           (IntNormalizedPTST.NormalizationScript.PD_NormalizationLabel.make
              ~label:54321
              ~perm:([CountedPermutation.make_element
                        ~old_index:0
                        ~new_index:(0,0)
                     ;CountedPermutation.make_element
                         ~old_index:1
                         ~new_index:(0,1)
                     ;CountedPermutation.make_element
                         ~old_index:2
                         ~new_index:(0,2)
                     ;CountedPermutation.make_element
                         ~old_index:3
                         ~new_index:(0,3)
                     ;CountedPermutation.make_element
                         ~old_index:4
                         ~new_index:(0,4)
                     ;CountedPermutation.make_element
                         ~old_index:5
                         ~new_index:(0,5)
                     ;CountedPermutation.make_element
                         ~old_index:6
                         ~new_index:(0,6)])
           ,[(IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:123 ~perm:[])
                []
             ,1. /. 7.)
            ;(IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:246 ~perm:[])
                []
             ,1. /. 7.)
            ;(IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:369 ~perm:[])
                []
             ,1. /. 7.)
            ;(IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:492 ~perm:[])
                []
             ,1. /. 7.)
            ;(IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:615 ~perm:[])
                []
             ,1. /. 7.)
            ;(IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:738 ~perm:[])
                []
             ,1. /. 7.)
            ;(IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:861 ~perm:[])
                []
             ,1. /. 7.)])))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Plus
             (54321
             ,[(IntNormalizedPTST.NonNormalizedTree.mk_times 123 [],1. /. 7.)
              ;(IntNormalizedPTST.NonNormalizedTree.mk_times 246 [],1. /. 7.)
              ;(IntNormalizedPTST.NonNormalizedTree.mk_times 369 [],1. /. 7.)
              ;(IntNormalizedPTST.NonNormalizedTree.mk_times 492 [],1. /. 7.)
              ;(IntNormalizedPTST.NonNormalizedTree.mk_times 615 [],1. /. 7.)
              ;(IntNormalizedPTST.NonNormalizedTree.mk_times 738 [],1. /. 7.)
              ;(IntNormalizedPTST.NonNormalizedTree.mk_times 861 [],1. /. 7.)]))))

let test_normalize_tree_plus_complex_perm _ =
  let b1 = ((IntNormalizedPTST.Nonempty.mk_base 12346,2),1. /. 3.) in
  let b2 = ((IntNormalizedPTST.Nonempty.mk_base 12345,1),1. /. 3.) in
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_plus
          54321
          [b2
          ;b1])
    ,IntNormalizedPTST.NormalizationScript.Nonempty
        (IntNormalizedPTST.NormalizationScript.Plus
           (IntNormalizedPTST.NormalizationScript.PD_NormalizationLabel.make
              ~label:54321
              ~perm:([CountedPermutation.make_element
                        ~old_index:1
                        ~new_index:(0,0)
                     ;CountedPermutation.make_element
                         ~old_index:0
                         ~new_index:(1,0)
                     ;CountedPermutation.make_element
                         ~old_index:2
                         ~new_index:(1,1)])
           ,[(IntNormalizedPTST.NormalizationScript.Base 12346,1. /. 3.)
            ;(IntNormalizedPTST.NormalizationScript.Base 12345,1. /. 3.)
            ;(IntNormalizedPTST.NormalizationScript.Base 12346,1. /. 3.)])))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Plus
             (54321
             ,[(IntNormalizedPTST.NonNormalizedTree.Base 12346,1. /. 3.)
              ;(IntNormalizedPTST.NonNormalizedTree.Base 12345,1. /. 3.)
              ;(IntNormalizedPTST.NonNormalizedTree.Base 12346,1. /. 3.)]))))

let test_normalize_tree_times_nodupes _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_times
          54321
          [(IntNormalizedPTST.Nonempty.mk_base 12345,1)])
    ,IntNormalizedPTST.NormalizationScript.Nonempty
        (IntNormalizedPTST.NormalizationScript.Times
           (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make
              ~label:54321
              ~perm:([
                  CountedPermutation.make_element
                    ~old_index:0
                    ~new_index:(0,0)])
           ,[IntNormalizedPTST.NormalizationScript.Base 12345])))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Times
             (54321
             ,[IntNormalizedPTST.NonNormalizedTree.Base 12345]))))

let test_normalize_tree_times_dupes _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_times
          54321
          [(IntNormalizedPTST.Nonempty.mk_base 12345,2)])
    ,IntNormalizedPTST.NormalizationScript.Nonempty
        (IntNormalizedPTST.NormalizationScript.Times
           (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make
              ~label:54321
              ~perm:([CountedPermutation.make_element
                        ~old_index:0
                        ~new_index:(0,0)
                     ;CountedPermutation.make_element
                         ~old_index:1
                         ~new_index:(0,1)])
           ,[IntNormalizedPTST.NormalizationScript.Base 12345
            ;IntNormalizedPTST.NormalizationScript.Base 12345])))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Times
             (54321
             ,[IntNormalizedPTST.NonNormalizedTree.Base 12345
              ;IntNormalizedPTST.NonNormalizedTree.Base 12345]))))

let test_normalize_tree_times_complex_perm _ =
  let b2 = (IntNormalizedPTST.Nonempty.mk_base 12345,1) in
  let b1 = (IntNormalizedPTST.Nonempty.mk_base 12346,2) in
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_times
          54321
          [b2
          ;b1])
    ,IntNormalizedPTST.NormalizationScript.Nonempty
        (IntNormalizedPTST.NormalizationScript.Times
           (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make
              ~label:54321
              ~perm:([CountedPermutation.make_element
                        ~old_index:1
                        ~new_index:(0,0)
                     ;CountedPermutation.make_element
                         ~old_index:0
                         ~new_index:(1,0)
                     ;CountedPermutation.make_element
                         ~old_index:2
                         ~new_index:(1,1)])
           ,[IntNormalizedPTST.NormalizationScript.Base 12346
            ;IntNormalizedPTST.NormalizationScript.Base 12345
            ;IntNormalizedPTST.NormalizationScript.Base 12346])))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Times
             (54321
             ,[IntNormalizedPTST.NonNormalizedTree.Base 12346
              ;IntNormalizedPTST.NonNormalizedTree.Base 12345
              ;IntNormalizedPTST.NonNormalizedTree.Base 12346]))))

let normalize_from_tree_suite = "Test NormalizedPTST from_tree" >:::
  [
    "test_normalize_tree_empty" >:: test_normalize_tree_empty;
    "test_normalize_tree_base" >:: test_normalize_tree_base;
    "test_normalize_tree_star" >:: test_normalize_tree_star;
    "test_normalize_tree_plus_nodupes" >:: test_normalize_tree_plus_nodupes;
    "test_normalize_tree_plus_dupes" >:: test_normalize_tree_plus_dupes;
    "test_normalize_tree_plus_many_dupes_with_times" >:: test_normalize_tree_plus_many_dupes_with_times;
    "test_normalize_tree_plus_complex_perm" >:: test_normalize_tree_plus_complex_perm;
    "test_normalize_tree_times_nodupes" >:: test_normalize_tree_times_nodupes;
    "test_normalize_tree_times_dupes" >:: test_normalize_tree_times_dupes;
    "test_normalize_tree_times_complex_perm" >:: test_normalize_tree_times_complex_perm;
  ]

let _ = run_test_tt_main normalize_from_tree_suite

module IPTSTA = IntPTSTAlignment
module IPTSTANE = IntPTSTAlignment.NonemptyNormalizedPlusStarTreeAlignment

let test_cost_base _ =
  assert_float_equal
    0.5
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_base 2))

let test_cost_star _ =
  assert_float_equal
    0.25
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_star
          2
          2
          (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_base 4)))

let test_cost_times_singleton _ =
  assert_float_equal
    0.0
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_times
          5
          5
          []
          []
          []))

let test_cost_times_unmapped_left _ =
  assert_float_equal
    5.
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_times
          5
          5
          []
          [((0,0),5.)]
          []))

let test_cost_times_unmapped_right _ =
  assert_float_equal
    5.
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_times
          5
          5
          []
          []
          [((0,0),5.)]))

let test_cost_times_recursive _ =
  assert_float_equal
    5.
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_times
          5
          5
          [((0,0),(0,0),IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_times
              5
              5
              []
              []
              [])]
          []
          [((1,0),5.)]))

let test_cost_times_imperfect_rec _ =
  assert_float_equal
    8.
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_times
          5
          5
          [((0,0)
           ,(0,0)
           ,IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_times
              8
              8
              []
              [((0,0),8.)]
              [])]
          []
          []))

let test_cost_plus_singleton _ =
  assert_float_equal
    0.
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
          5
          5
          []
          []
          []))

let test_cost_plus_single_rec _ =
  assert_float_equal
    0.
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
          5
          5
          [(0,0),(0,0),(IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
                          5
                          5
                          []
                          []
                          [])]
          [((0,0),(0,0))]
          [((0,0),(0,0))]))

let test_cost_plus_double_rec _ =
  assert_float_equal
    0.
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
          5
          5
          [(0,0),(0,0),(IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
                          5
                          5
                          []
                          []
                          [])]
          [((0,0),(0,0))]
          [((0,0),(0,0))]))

let test_cost_plus_double_rec _ =
  assert_float_equal
    0.
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
          5
          5
          [(0,0),(1,0),(IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
                          5
                          5
                          []
                          []
                          [])
          ;(1,0),(0,0),(IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
                          8
                          8
                          []
                          []
                          [])]
          [((0,0),(1,0));((1,0),(0,0))]
          [((0,0),(1,0));((1,0),(0,0))]))

let test_cost_plus_double_rec _ =
  assert_float_equal
    0.
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
          5
          5
          [(0,0),(1,0),(IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
                          5
                          5
                          []
                          []
                          [])
          ;(1,0),(0,0),(IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
                          8
                          8
                          []
                          []
                          [])]
          [((0,0),(1,0));((1,0),(0,0))]
          [((0,0),(1,0));((1,0),(0,0))]))

let test_cost_plus_single_merge _ =
  assert_float_equal
    1.
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
          5
          5
          [(0,0),(0,0),(IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
                          5
                          5
                          []
                          []
                          [])
          ;(0,0),(0,1),(IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
                          5
                          5
                          []
                          []
                          [])]
          [((0,0),(0,1))]
          [((0,0),(0,0));((0,1),(0,0))]))

let test_cost_plus_crossing_merge _ =
  assert_float_equal
    2.
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
          5
          5
          [(0,0),(0,0),(IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
                          5
                          5
                          []
                          []
                          [])
          ;(0,0),(0,1),(IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
                          5
                          5
                          []
                          []
                          [])
          ;(0,1),(0,0),(IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
                          8
                          8
                          []
                          []
                          [])
          ;(0,1),(0,1),(IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_plus
                          8
                          8
                          []
                          []
                          [])]
          [((0,0),(0,0));((0,1),(0,1))]
          [((0,0),(0,0));((0,1),(0,1))]))

let cost_suite = "Test PTSTAlignment cost" >:::
  [
    "test_cost_base" >:: test_cost_base;
    "test_cost_times_singleton" >:: test_cost_times_singleton;
    "test_cost_times_unmapped_left" >:: test_cost_times_unmapped_left;
    "test_cost_times_unmapped_right" >:: test_cost_times_unmapped_right;
    "test_cost_times_recursive" >:: test_cost_times_recursive;
    "test_cost_times_imperfect_rec" >:: test_cost_times_imperfect_rec;
    "test_cost_plus_singleton" >:: test_cost_plus_singleton;
    "test_cost_plus_single_rec" >:: test_cost_plus_single_rec;
    "test_cost_plus_double_rec" >:: test_cost_plus_double_rec;
    "test_cost_plus_single_merge" >:: test_cost_plus_single_merge;
    "test_cost_plus_crossing_merge" >:: test_cost_plus_crossing_merge;
  ]

let _ = run_test_tt_main cost_suite


let test_get_minimal_alignment_and_cost_empty _ =
  assert_alignment_option_cost_equal
    (Some IPTSTA.Empty,0.)
    (IPTSTA.get_minimal_alignment_and_cost
       IPTST.Empty
       IPTST.Empty)

let test_get_minimal_alignment_and_cost_empty_nonempty _ =
  assert_alignment_option_cost_equal
    (None,1.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Plus (1,[])))
       IPTST.Empty)

let test_get_minimal_alignment_and_cost_times_plus _ =
  assert_alignment_option_cost_equal
    (None,1.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Times (1,[])))
       (IPTST.Nonempty
          (IPTST.Plus (1,[]))))

let test_get_minimal_alignment_and_cost_emptytimes_emptytimes _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Times
             (1
             ,1
             ,[]
             ,[]
             ,[]))))
    ,0.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Times (1,[])))
       (IPTST.Nonempty
          (IPTST.Times (1,[]))))

let test_get_minimal_alignment_and_cost_times_emptytimes _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Times
             (1
             ,1
             ,[]
             ,[0]
             ,[]))))
    ,2.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Base (2)])))
       (IPTST.Nonempty
          (IPTST.Times (1,[]))))

let test_get_minimal_alignment_and_cost_easy_times_bijection _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Times
             (1
             ,1
             ,[(0
               ,0
               ,IPTSTA.Nonempty.Base 1)]
             ,[]
             ,[]))))
    ,0.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Base 3])))
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Base 3]))))

let test_get_minimal_alignment_and_cost_easy_times_project_left _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Times
             (1
             ,1
             ,[(1
               ,0
               ,IPTSTA.Nonempty.Base 3)]
             ,[0]
             ,[]))))
    ,1. +. (2. /. 3.))
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Base 1;IPTST.Base 3])))
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Base 9]))))

let test_get_minimal_alignment_and_cost_easy_times_project_from_space _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Times
             (1
             ,1
             ,[(0
               ,0
               ,IPTSTA.Nonempty.Base
                   2)]
             ,[1]
             ,[]))))
    ,7.5)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Base 7;IPTST.Base 7])))
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Base 14]))))

let test_get_minimal_alignment_and_cost_easy_times_project_left_from_requires_map _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Times
             (1
             ,1
             ,[(1
               ,0
               ,IPTSTA.Nonempty.Times
                   (246
                   ,239
                   ,[(0,0,IPTSTA.Nonempty.Base 1)]
                   ,[]
                   ,[]))]
             ,[0]
             ,[]))))
    ,2.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (239,[IPTST.Base 2]);IPTST.Times (246,[IPTST.Base 2])])))
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (239,[IPTST.Base 2])]))))

let test_get_minimal_alignment_and_cost_hard_times_bijection _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Times
             (1
             ,1
             ,[(0
               ,1
               ,IPTSTA.Nonempty.Times
                   (1
                   ,1
                   ,[(0,0,IPTSTA.Nonempty.Base 1)]
                   ,[]
                   ,[]))
              ;(1
               ,0
               ,IPTSTA.Nonempty.Times
                   (2
                   ,2
                   ,[(0,0,IPTSTA.Nonempty.Base 1)]
                   ,[]
                   ,[]))]
             ,[]
             ,[]))))
    ,0.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (1,[IPTST.Base 4]);IPTST.Times (2,[IPTST.Base 4])])))
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (2,[IPTST.Base 4]);IPTST.Times (1,[IPTST.Base 4])]))))

let test_get_minimal_alignment_and_cost_emptyplus_emptyplus _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Plus
             (1
             ,1
             ,[]
             ,[]
             ,[]))))
    ,0.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Plus (1,[])))
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Plus (1,[]))))

let test_get_minimal_alignment_and_cost_plus_emptyplus _ =
  assert_alignment_option_cost_equal
    (None,1.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[(IPTST.Plus (1,[]),1.)])))
       (IPTST.Nonempty
          (IPTST.Plus (1,[]))))

let test_get_minimal_alignment_and_cost_easy_plus_bijection _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Plus
             (1
             ,1
             ,[(0
               ,0
               ,IPTSTA.Nonempty.Plus
                   (1
                   ,1
                 ,[]
                 ,[]
                 ,[]))]
             ,[0]
             ,[0]))))
    ,0.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[(IPTST.Plus (1,[]),1.)])))
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[(IPTST.Plus (1,[]),1.)]))))

let test_get_minimal_alignment_and_cost_hard_plus_bijection _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Plus
             (1
             ,1
             ,[(2
               ,1
               ,IPTSTA.Nonempty.Plus
                   (0
                   ,0
                 ,[]
                 ,[]
                   ,[]))
              ;(1
               ,2
               ,IPTSTA.Nonempty.Plus
                   (2
                   ,2
                 ,[]
                 ,[]
                   ,[]))
              ;(0
               ,0
               ,IPTSTA.Nonempty.Plus
                   (1
                   ,1
                 ,[]
                 ,[]
                   ,[]))]
             ,[0;2;1]
             ,[0;2;1]))))
    ,0.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[(IPTST.Plus (1,[]),1. /. 3.)
              ;(IPTST.Plus (2,[]),1. /. 3.)
              ;(IPTST.Plus (0,[]),1. /. 3.)])))
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[(IPTST.Plus (1,[]),1. /. 3.)
              ;(IPTST.Plus (0,[]),1. /. 3.)
              ;(IPTST.Plus (2,[]),1. /. 3.)]))))

let test_get_minimal_alignment_and_cost_plus_merge_equiv _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Plus
             (1
             ,1
             ,[(1
               ,0
               ,IPTSTA.Nonempty.Base 1)
              ;(0
               ,0
               ,IPTSTA.Nonempty.Base 1)]
             ,[0;0]
             ,[1]))))
    ,1.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[(IPTST.Base 2,1. /. 2.)
              ;(IPTST.Base 2,1. /. 2.)])))
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[(IPTST.Base 2,1.)]))))

let test_get_minimal_alignment_and_cost_plus_merge_diff _ =
  let b2 = (0,0,IPTSTA.Nonempty.Base 1) in
  let b1 = (1,0,IPTSTA.Nonempty.Base 2) in
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Plus
             (1
             ,1
             ,[b1
              ;b2]
             ,[0;0]
             ,[0]))))
    ,1.5)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[(IPTST.Base 2,1. /. 2.)
              ;(IPTST.Base 4,1. /. 2.)])))
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[(IPTST.Base 2,1.)]))))

let alignment_distance_suite = "Test get_minimal_alignment_and_cost" >:::
  [
    "test_get_minimal_alignment_and_cost_empty" >:: test_get_minimal_alignment_and_cost_empty;
    "test_get_minimal_alignment_and_cost_empty_nonempty" >:: test_get_minimal_alignment_and_cost_empty_nonempty;
    "test_get_minimal_alignment_and_cost_times_plus" >:: test_get_minimal_alignment_and_cost_times_plus;
    "test_get_minimal_alignment_and_cost_emptytimes_emptytimes" >:: test_get_minimal_alignment_and_cost_emptytimes_emptytimes;
    "test_get_minimal_alignment_and_cost_times_emptytimes" >:: test_get_minimal_alignment_and_cost_times_emptytimes;
    "test_get_minimal_alignment_and_cost_easy_times_bijection" >:: test_get_minimal_alignment_and_cost_easy_times_bijection;
    "test_get_minimal_alignment_and_cost_easy_times_project_left" >:: test_get_minimal_alignment_and_cost_easy_times_project_left;
    "test_get_minimal_alignment_and_cost_easy_times_project_from_space" >:: test_get_minimal_alignment_and_cost_easy_times_project_from_space;
    "test_get_minimal_alignment_and_cost_easy_times_project_left_from_requires_map" >:: test_get_minimal_alignment_and_cost_easy_times_project_left_from_requires_map;
    "test_get_minimal_alignment_and_cost_hard_times_bijection" >:: test_get_minimal_alignment_and_cost_hard_times_bijection;
    "test_get_minimal_alignment_and_cost_emptyplus_emptyplus" >:: test_get_minimal_alignment_and_cost_emptyplus_emptyplus;
    "test_get_minimal_alignment_and_cost_plus_emptyplus" >:: test_get_minimal_alignment_and_cost_plus_emptyplus;
    "test_get_minimal_alignment_and_cost_easy_plus_bijection" >:: test_get_minimal_alignment_and_cost_easy_plus_bijection;
    "test_get_minimal_alignment_and_cost_hard_plus_bijection" >:: test_get_minimal_alignment_and_cost_hard_plus_bijection;
    "test_get_minimal_alignment_and_cost_plus_merge_equiv" >:: test_get_minimal_alignment_and_cost_plus_merge_equiv;
    "test_get_minimal_alignment_and_cost_plus_merge_diff" >:: test_get_minimal_alignment_and_cost_plus_merge_diff;
  ]

let _ = run_test_tt_main alignment_distance_suite

let test_exampled_dnf_regex_to_tree_empty _ =
  assert_rxtree_equal
    (StarSemiringTreeRep.Tree.Nonempty
       (StarSemiringTreeRep.Tree.Plus
          (StarSemiringTreeRep.PD.make
             (make_example_data
                ~arg1_data:[]
                ~arg2_data:[]
                ~output_data:[]),[])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       LensContext.empty
       ([],(make_example_data
              ~arg1_data:[]
              ~arg2_data:[]
              ~output_data:[])))

let test_exampled_dnf_regex_to_tree_epsilon _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_rxtree_equal
    (StarSemiringTreeRep.Tree.Nonempty
       (StarSemiringTreeRep.Tree.Plus
          (StarSemiringTreeRep.PD.make ex_data
          ,[(StarSemiringTreeRep.Tree.Times
               (StarSemiringTreeRep.TD.make ex_data [""] []
               ,[])
            ,1.)])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       LensContext.empty
       ([(([],[""],ex_data),1.)],ex_data))

let test_exampled_dnf_regex_to_tree_epsilon _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_rxtree_equal
    (StarSemiringTreeRep.Tree.Nonempty
       (StarSemiringTreeRep.Tree.Plus
          (StarSemiringTreeRep.PD.make ex_data
          ,[(StarSemiringTreeRep.Tree.Times
               (StarSemiringTreeRep.TD.make ex_data [""] []
               ,[])
            ,1.)])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       LensContext.empty
       ([(([],[""],ex_data),1.)],ex_data))

let test_exampled_dnf_regex_to_tree_base _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0];[1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  let str_data_post =
    make_example_data
      ~arg1_data:["aw";"wa"]
      ~arg2_data:[]
      ~output_data:[]
  in
  let str_data_pre =
    make_example_data
      ~arg1_data:["aw";"aw"]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_rxtree_equal
    (StarSemiringTreeRep.Tree.Nonempty
       (StarSemiringTreeRep.Tree.Plus
          (StarSemiringTreeRep.PD.make ex_data
          ,[(StarSemiringTreeRep.Tree.Times
               (StarSemiringTreeRep.TD.make ex_data ["a";"b"]
                  [StochasticRegex.make_closed (StochasticRegex.make_base "wa")]
               ,[StarSemiringTreeRep.Tree.Base
                   (StarSemiringTreeRep.BD.make
                      (Regex.make_base "aw")
                      (StochasticRegex.make_base "aw")
                      ex_data
                      str_data_post
                      LensContext.empty)])
            ,1.)])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       LensContext.empty
       ([(([EAClosed
              (Regex.make_base "aw"
              ,StochasticRegex.make_base "wa"
              ,Lens.Disconnect
                  (Regex.make_base "aw"
                  ,Regex.make_base "wa"
                  ,"aw"
                  ,"wa")
              ,str_data_pre
              ,ex_data
              ,str_data_post)]
          ,["a";"b"]
          ,ex_data)
         ,1.)]
       ,ex_data))

let test_exampled_dnf_regex_to_tree_star _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0]]
      ~arg2_data:[]
      ~output_data:[]
  in
  let ex_data_inside =
    make_example_data
      ~arg1_data:[[0;0];[0;1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_rxtree_equal
    (StarSemiringTreeRep.Tree.Nonempty
       (StarSemiringTreeRep.Tree.Plus
          (StarSemiringTreeRep.PD.make ex_data
          ,[(StarSemiringTreeRep.Tree.Times
               (StarSemiringTreeRep.TD.make
                  ex_data
                  ["a";"b"]
                  [StochasticRegex.make_star
                     (StochasticRegex.make_base "c")
                     0.8]
               ,[StarSemiringTreeRep.Tree.Star
                   ((StarSemiringTreeRep.SD.make
                       ex_data)
                   ,(StarSemiringTreeRep.Tree.Plus
                       (StarSemiringTreeRep.PD.make ex_data_inside
                       ,[(StarSemiringTreeRep.Tree.Times
                            (StarSemiringTreeRep.TD.make ex_data_inside ["c"] []
                            ,[])
                         ,1.)])))])
            ,1.)])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       LensContext.empty
       ([(([EAStar
              (([(([]
                  ,["c"]
                  ,ex_data_inside)
                 ,1.)]
               ,ex_data_inside)
              ,ex_data
              ,StochasticRegex.make_star (StochasticRegex.make_base "c") 0.8)
           ]
          ,["a";"b"]
          ,ex_data)
         ,1.)]
       ,ex_data))

let exampled_dnf_regex_to_tree_suite = "Test exampled_dnf_regex_to_tree" >:::
  [
    "test_exampled_dnf_regex_to_tree_empty" >:: test_exampled_dnf_regex_to_tree_empty;
    "test_exampled_dnf_regex_to_tree_epsilon" >:: test_exampled_dnf_regex_to_tree_epsilon;
    "test_exampled_dnf_regex_to_tree_base" >:: test_exampled_dnf_regex_to_tree_base;
    "test_exampled_dnf_regex_to_tree_star" >:: test_exampled_dnf_regex_to_tree_star;
  ]

let _ = run_test_tt_main exampled_dnf_regex_to_tree_suite


let test_regex_to_exampled_dnf_regex_or _ =
  assert_exampled_dnf_regex_option_equal
    (Some
       ([(([], ["a"], empty_parsing_example_data),
          0.5);
         (([], ["b"], empty_parsing_example_data),
          0.5)],
        empty_parsing_example_data))
    (regex_to_exampled_dnf_regex
       LensContext.empty
       (StochasticRegex.make_or
          (StochasticRegex.make_base "a")
          (StochasticRegex.make_base "b")
          0.5)
       empty_parsing_example_data)

let test_regex_to_exampled_dnf_regex_multi_or _ =
  assert_exampled_dnf_regex_option_equal
    (Some
       ([(([], ["a"], empty_parsing_example_data),
          0.16666666666666667);
         (([], ["b"], empty_parsing_example_data),
          0.16666666666666667);
         (([], ["c"], empty_parsing_example_data),
          0.16666666666666667);
         (([], ["d"], empty_parsing_example_data),
          0.16666666666666667);
         (([], ["e"], empty_parsing_example_data),
          0.16666666666666667);
         (([], ["f"], empty_parsing_example_data),
          0.16666666666666667)],
        empty_parsing_example_data))
    (regex_to_exampled_dnf_regex
       LensContext.empty
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
       empty_parsing_example_data)

let test_to_exampled_dnf_regex_suite = "Test regex_to_exampled_dnf_regex" >:::
  [
    "test_regex_to_exampled_dnf_regex_multi_or" >:: test_regex_to_exampled_dnf_regex_multi_or;
    "test_regex_to_exampled_dnf_regex_or" >:: test_regex_to_exampled_dnf_regex_or;
  ]

let _ = run_test_tt_main test_to_exampled_dnf_regex_suite

let test_kinda_rigid_synth_empty _ =
  assert_lens_float_option_equal
    (Some (Lens.zero,0.))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       StochasticRegex.empty
       StochasticRegex.empty
       []
       []
       []
       [])

let test_kinda_rigid_synth_project_lastnames _ =
  let uppercase = (Regex.from_char_set [(65,90)]) in
  let lowercases = Regex.make_star (Regex.from_char_set [(97,122)]) in
  let name =
    StochasticRegex.from_regex @$
    iteratively_deepen
      (Regex.make_concat
         uppercase
         lowercases)
  in
  let name_opened = Option.value_exn (StochasticRegex.separate_closed name) in
  let names =
    StochasticRegex.make_star
      (StochasticRegex.make_concat
         (StochasticRegex.make_base " ")
         name)
      0.8
  in
  let firstlast =
    StochasticRegex.make_concat
      name
      names
  in
  assert_lens_float_option_equal
    (Some
       (Lens.make_concat
          (Lens.make_const "" "")
          (Lens.make_concat
             (Lens.make_concat
                (Lens.make_ident (StochasticRegex.to_regex name_opened))
                (Lens.make_const "" ""))
             (Lens.make_concat
                (Lens.make_disconnect (Regex.make_base "") (StochasticRegex.to_regex names) "" "")
                (Lens.make_const "" "")))
       ,224.11399347))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       name
       firstlast
       [("Anders","Anders Miltner")]
       []
       []
       [])

let test_kinda_rigid_synth_project_firstname _ =
  let uppercase = (Regex.from_char_set [(65,90)]) in
  let lowercases = Regex.make_star (Regex.from_char_set [(97,122)]) in
  let name =
    iteratively_deepen
      (Regex.make_concat
         uppercase
         lowercases)
  in
  let name_opened = Option.value_exn (Regex.separate_closed name) in
  let firstlast =
    Regex.make_concat
      name
      (Regex.make_concat
         (Regex.make_base " ")
         name)
  in
  assert_lens_float_option_equal
    (Some
       (Lens.make_concat
          (Lens.make_const "" "")
          (Lens.make_swap
             (Lens.make_concat
                (Lens.make_ident name_opened)
                (Lens.make_const "" ""))
             (Lens.make_concat
                (Lens.make_disconnect (Regex.make_base "") name "" "A")
                (Lens.make_const "" " ")))
       ,54.2236781303))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       (StochasticRegex.from_regex name)
       (StochasticRegex.from_regex firstlast)
       [("Miltner","Anders Miltner")]
       []
       []
       [])

let test_kinda_rigid_synth_disconnect _ =
  let uppercase = (Regex.from_char_set [(65,90)]) in
  let lowercases = Regex.make_star (Regex.from_char_set [(97,122)]) in
  let name =
    iteratively_deepen
      (Regex.make_concat
         uppercase
         lowercases)
  in
  assert_lens_float_option_equal
    (Some
       (Lens.make_concat
          (Lens.make_const "" "")
          (Lens.make_swap
             (Lens.make_concat
                (Lens.make_disconnect name (Regex.one) "A" "")
                (Lens.make_const "" ""))
             (Lens.make_concat
                (Lens.make_disconnect (Regex.one) name "" "A")
                (Lens.make_const "" "")))
       ,108.447356261))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       (StochasticRegex.from_regex name)
       (StochasticRegex.from_regex name)
       [("Anders","Anders")]
       []
       [("Anders","Miltner","Miltner")]
       [])

let test_kinda_rigid_synth_project_last_from_put _ =
  let uppercase = (Regex.from_char_set [(65,90)]) in
  let lowercases = Regex.make_star (Regex.from_char_set [(97,122)]) in
  let name =
    iteratively_deepen
      (Regex.make_concat
         uppercase
         lowercases)
  in
  let name_opened = Option.value_exn (Regex.separate_closed name) in
  let firstlast =
    Regex.make_concat
      name
      (Regex.make_concat
         (Regex.make_base " ")
         name)
  in
  assert_lens_float_option_equal
    (Some
       (Lens.make_concat
          (Lens.make_const "" "")
          (Lens.make_concat
             (Lens.make_concat
                (Lens.make_ident name_opened)
                (Lens.make_const " " ""))
             (Lens.make_concat
                (Lens.make_disconnect name (Regex.make_base "") "A" "")
                (Lens.make_const "" "")))
       ,54.2236781303))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       (StochasticRegex.from_regex firstlast)
       (StochasticRegex.from_regex name)
       []
       []
       []
       [("Anders","Nnders Anders","Anders Anders")])

let test_kinda_rigid_synth_project_first_from_put _ =
  let uppercase = (Regex.from_char_set [(65,90)]) in
  let lowercases = Regex.make_star (Regex.from_char_set [(97,122)]) in
  let name =
    iteratively_deepen
      (Regex.make_concat
         uppercase
         lowercases)
  in
  let name_opened = Option.value_exn (Regex.separate_closed name) in
  let firstlast =
    Regex.make_concat
      name
      (Regex.make_concat
         (Regex.make_base " ")
         name)
  in
  print_endline "SUP";
  print_endline @$ Float.to_string (StochasticRegex.information_content (StochasticRegex.from_regex name));
  assert_lens_float_option_equal
    (Some
       (Lens.make_concat
          (Lens.make_const "" "")
          (Lens.make_swap
             (Lens.make_concat
                (Lens.make_disconnect name (Regex.make_base "") "A" "")
                (Lens.make_const " " ""))
             (Lens.make_concat
                (Lens.make_ident name_opened)
                (Lens.make_const "" "")))
       ,54.2236781303))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       (StochasticRegex.from_regex firstlast)
       (StochasticRegex.from_regex name)
       []
       []
       []
       [("Anders","Anders Nnders","Anders Anders")])

let kinda_rigid_synth_suite = "Test kinda_rigid_synth" >:::
  [
    "test_kinda_rigid_synth_empty" >:: test_kinda_rigid_synth_empty;
    "test_kinda_rigid_synth_project_lastnames" >:: test_kinda_rigid_synth_project_lastnames;
    "test_kinda_rigid_synth_project_firstname" >:: test_kinda_rigid_synth_project_firstname;
    "test_kinda_rigid_synth_disconnect" >:: test_kinda_rigid_synth_disconnect;
    "test_kinda_rigid_synth_project_last_from_put" >:: test_kinda_rigid_synth_project_last_from_put;
    "test_kinda_rigid_synth_project_first_from_put" >:: test_kinda_rigid_synth_project_first_from_put;
  ]

let _ = run_test_tt_main kinda_rigid_synth_suite


let test_alignment_to_lens_base _ =
  assert_lens_option_equal
    (Some (Lens.Identity (Regex.make_base "a")))
    (StarSemiringTreeRep.alignment_to_lens
       (StarSemiringTreeRep.OptimalAlignment.NonemptyTree
          (StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
             (Lens.Identity (Regex.make_base "a")))))

let test_alignment_to_lens_star _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0];[1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_lens_option_equal
    (Some (Lens.Iterate (Lens.Identity (Regex.make_base "b"))))
    (StarSemiringTreeRep.alignment_to_lens
       (StarSemiringTreeRep.OptimalAlignment.NonemptyTree
          (StarSemiringTreeRep.OptimalAlignment.Nonempty.Star
             (StarSemiringTreeRep.SD.make ex_data
             ,StarSemiringTreeRep.SD.make ex_data
             ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                 (Lens.Identity (Regex.make_base "b"))))))

let test_alignment_to_lens_concat_basic _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0];[1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_lens_option_equal
    (Some (Lens.Disconnect (Regex.make_base "a",Regex.make_base "b","a","b")))
    (StarSemiringTreeRep.alignment_to_lens
       (StarSemiringTreeRep.OptimalAlignment.NonemptyTree
          (StarSemiringTreeRep.OptimalAlignment.Nonempty.Times
             (StarSemiringTreeRep.TD.make ex_data ["a"] []
             ,StarSemiringTreeRep.TD.make ex_data ["b"] []
             ,[]
             ,[]
             ,[]))))

let test_alignment_to_lens_concat_onesub _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0];[1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_lens_option_equal
    (Some
       (Lens.make_times
          (Lens.make_disconnect
             (Regex.make_base "a1")
             (Regex.make_base "b1")
             "a1"
             "b1")
          (Lens.make_times
             (Lens.make_ident
                (Regex.make_closed (Regex.make_base "t")))
             (Lens.make_disconnect
                (Regex.make_base "a2")
                (Regex.make_base "b2")
                "a2"
                "b2"))))
    (StarSemiringTreeRep.alignment_to_lens
       (StarSemiringTreeRep.OptimalAlignment.NonemptyTree
          (StarSemiringTreeRep.OptimalAlignment.Nonempty.Times
             (StarSemiringTreeRep.TD.make
                ex_data
                ["a1";"a2"]
                [StochasticRegex.make_closed (StochasticRegex.make_base "t")]
             ,StarSemiringTreeRep.TD.make
                 ex_data
                 ["b1";"b2"]
                 [StochasticRegex.make_closed (StochasticRegex.make_base "t")]
             ,[(0
               ,0
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.Identity (Regex.make_closed (Regex.make_base "t"))))]
             ,[]
             ,[]))))

let test_alignment_to_lens_concat_oneproj_left _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0];[1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_lens_option_equal
    (Some
       (Lens.make_times
          (Lens.make_const
             "a1"
             "b1")
             (Lens.make_times
                (Lens.make_disconnect
                   (Regex.make_closed (Regex.make_base "t"))
                   Regex.one
                   "t"
                   "")
                (Lens.make_disconnect
                   ((Regex.make_base "a2"))
                   Regex.one
                   "a2"
                   ""))))
    (StarSemiringTreeRep.alignment_to_lens
       (StarSemiringTreeRep.OptimalAlignment.NonemptyTree
          (StarSemiringTreeRep.OptimalAlignment.Nonempty.Times
             (StarSemiringTreeRep.TD.make
                ex_data
                ["a1";"a2"]
                [StochasticRegex.make_closed (StochasticRegex.make_base "t")]
             ,StarSemiringTreeRep.TD.make
                 ex_data
                 ["b1"]
                 []
             ,[]
             ,[0]
             ,[]))))

let test_alignment_to_lens_concat_oneproj_right _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0];[1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_lens_option_equal
    (Some
       (Lens.make_times
          (Lens.make_disconnect
             (Regex.make_base "a1")
             (Regex.make_base "b1")
             "a1"
             "b1")
             (Lens.make_times
                (Lens.make_disconnect
                   Regex.one
                   (Regex.make_closed (Regex.make_base "u"))
                   ""
                   "u")
                (Lens.make_disconnect
                   Regex.one
                   ((Regex.make_base "b2"))
                   ""
                   "b2"))))
    (StarSemiringTreeRep.alignment_to_lens
       (StarSemiringTreeRep.OptimalAlignment.NonemptyTree
          (StarSemiringTreeRep.OptimalAlignment.Nonempty.Times
             (StarSemiringTreeRep.TD.make
                ex_data
                ["a1"]
                []
             ,StarSemiringTreeRep.TD.make
                 ex_data
                 ["b1";"b2"]
                 [StochasticRegex.make_closed (StochasticRegex.make_base "u")]
             ,[]
             ,[]
             ,[0]))))

let test_alignment_to_lens_concat_projs_swap _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0];[1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_lens_option_equal
    (Some
       (Lens.make_concat
          (Lens.make_const "a1" "b1")
          (Lens.make_permute
             (Permutation.create [1;3;0;2])
             [Lens.make_concat
                (Lens.make_ident
                   (Regex.make_closed
                      (Regex.make_base "t")))
                (Lens.make_const "a2" "b3")
             ;Lens.make_concat
                 (Lens.make_disconnect
                    (Regex.make_closed (Regex.make_base "u"))
                    (Regex.make_base "")
                    "u"
                    "")
                 (Lens.make_const "a3" "")
             ;Lens.make_concat
                 (Lens.make_ident
                    (Regex.make_closed
                       (Regex.make_base "v")))
                 (Lens.make_const "a4" "b2")
             ;Lens.make_concat
                 (Lens.make_disconnect
                    (Regex.make_base "")
                    (Regex.make_closed (Regex.make_base "w"))
                    ""
                    "w")
                 (Lens.make_const "" "b4")])))
    (StarSemiringTreeRep.alignment_to_lens
       (StarSemiringTreeRep.OptimalAlignment.NonemptyTree
          (StarSemiringTreeRep.OptimalAlignment.Nonempty.Times
             (StarSemiringTreeRep.TD.make
                ex_data
                ["a1";"a2";"a3";"a4"]
                [StochasticRegex.make_closed (StochasticRegex.make_base "t")
                ;StochasticRegex.make_closed (StochasticRegex.make_base "u")
                ;StochasticRegex.make_closed (StochasticRegex.make_base "v")]
             ,StarSemiringTreeRep.TD.make
                 ex_data
                 ["b1";"b2";"b3";"b4"]
                 [StochasticRegex.make_closed (StochasticRegex.make_base "v")
                 ;StochasticRegex.make_closed (StochasticRegex.make_base "t")
                 ;StochasticRegex.make_closed (StochasticRegex.make_base "w")]
             ,[(0
               ,1
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_closed (Regex.make_base "t"))))
              ;(2
               ,0
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_closed (Regex.make_base "v"))))]
             ,[1]
             ,[2]))))

let test_alignment_to_lens_or_basic _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0];[1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_lens_option_equal
    (Some (Lens.make_ident (Regex.make_base "t")))
    (StarSemiringTreeRep.alignment_to_lens
       (StarSemiringTreeRep.OptimalAlignment.NonemptyTree
          (StarSemiringTreeRep.OptimalAlignment.Nonempty.Plus
             (StarSemiringTreeRep.PD.make ex_data
             ,StarSemiringTreeRep.PD.make ex_data
             ,[(0
               ,0
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_base "t")))]
             ,[]
             ,[]))))

let test_alignment_to_lens_or_merge_left _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0];[1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_lens_option_equal
    (Some
       (Lens.make_or
          (Lens.make_ident (Regex.make_base "t"))
          (Lens.make_const "u" "t")))
    (StarSemiringTreeRep.alignment_to_lens
       (StarSemiringTreeRep.OptimalAlignment.NonemptyTree
          (StarSemiringTreeRep.OptimalAlignment.Nonempty.Plus
             (StarSemiringTreeRep.PD.make ex_data
             ,StarSemiringTreeRep.PD.make ex_data
             ,[(0
               ,0
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_base "t")))
              ;(1
               ,0
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_const "u" "t"))]
             ,[0;0]
             ,[0]))))

let test_alignment_to_lens_or_merge_right _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0];[1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_lens_option_equal
    (Some
       (Lens.make_or
          (Lens.make_ident (Regex.make_base "t"))
          (Lens.make_const "t" "u")))
    (StarSemiringTreeRep.alignment_to_lens
       (StarSemiringTreeRep.OptimalAlignment.NonemptyTree
          (StarSemiringTreeRep.OptimalAlignment.Nonempty.Plus
             (StarSemiringTreeRep.PD.make ex_data
             ,StarSemiringTreeRep.PD.make ex_data
             ,[(0
               ,0
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_base "t")))
              ;(0
               ,1
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_const "t" "u"))]
             ,[0]
             ,[0;0]))))

let test_alignment_to_lens_or_bijection _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0];[1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_lens_option_equal
    (Some
       (Lens.make_or
          (Lens.make_ident (Regex.make_base "t"))
          (Lens.make_ident (Regex.make_base "u"))))
    (StarSemiringTreeRep.alignment_to_lens
       (StarSemiringTreeRep.OptimalAlignment.NonemptyTree
          (StarSemiringTreeRep.OptimalAlignment.Nonempty.Plus
             (StarSemiringTreeRep.PD.make ex_data
             ,StarSemiringTreeRep.PD.make ex_data
             ,[(0
               ,1
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_base "t")))
              ;(1
               ,0
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_base "u")))]
             ,[1;0]
             ,[1;0]))))

let test_alignment_to_lens_or_crossing_creates _ =
  let ex_data =
    make_example_data
      ~arg1_data:[[0];[1]]
      ~arg2_data:[]
      ~output_data:[]
  in
  assert_lens_option_equal
    (Some
       (Lens.make_or
          Lens.zero
          (Lens.make_compose
             (Lens.make_compose
                (Lens.make_or
                   (Lens.make_or
                      (Lens.make_or
                         (Lens.make_concat
                            (Lens.make_const "" "0")
                            (Lens.make_ident (Regex.make_base "t")))
                         (Lens.make_concat
                            (Lens.make_const "" "1")
                            (Lens.make_ident (Regex.make_base "t"))))
                      (Lens.make_concat
                         (Lens.make_const "" "1")
                         (Lens.make_ident (Regex.make_base "u")))
                   )
                   (Lens.make_concat
                      (Lens.make_const "" "0")
                      (Lens.make_ident (Regex.make_base "u"))))
                (Lens.make_or
                   (Lens.make_or
                      (Lens.make_or
                         (Lens.make_concat
                            (Lens.make_const "1" "1")
                            (Lens.make_ident (Regex.make_base "u")))
                         (Lens.make_concat
                            (Lens.make_const  "0" "1")
                            (Lens.make_const "u" "t")))
                      (Lens.make_concat
                         (Lens.make_const "1" "0")
                         (Lens.make_const "t" "u")))
                   (Lens.make_concat
                      (Lens.make_const "0" "0")
                      (Lens.make_ident (Regex.make_base "t")))))
             (Lens.make_or
                (Lens.make_or
                   (Lens.make_or
                      (Lens.make_concat
                         (Lens.make_const "1" "")
                         (Lens.make_ident (Regex.make_base "t")))
                      (Lens.make_concat
                         (Lens.make_const "0" "")
                         (Lens.make_ident (Regex.make_base "t"))))
                   (Lens.make_concat
                      (Lens.make_const "0" "")
                      (Lens.make_ident (Regex.make_base "u"))))
                (Lens.make_concat
                   (Lens.make_const "1" "")
                   (Lens.make_ident (Regex.make_base "u")))))))
    (StarSemiringTreeRep.alignment_to_lens
       (StarSemiringTreeRep.OptimalAlignment.NonemptyTree
          (StarSemiringTreeRep.OptimalAlignment.Nonempty.Plus
             (StarSemiringTreeRep.PD.make ex_data
             ,StarSemiringTreeRep.PD.make ex_data
             ,[(0
               ,0
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_base "t")))
              ;(0
               ,1
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_const "t" "u"))
              ;(1
               ,0
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_const "u" "t"))
              ;(1
               ,1
               ,StarSemiringTreeRep.OptimalAlignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_base "u")))]
             ,[0;1]
             ,[1;0]))))

let alignment_to_lens_suite = "Test alignment_to_lens" >:::
  [
    "test_alignment_to_lens_base" >:: test_alignment_to_lens_base;
    "test_alignment_to_lens_star" >:: test_alignment_to_lens_star;
    "test_alignment_to_lens_concat_basic" >:: test_alignment_to_lens_concat_basic;
    "test_alignment_to_lens_concat_onesub" >:: test_alignment_to_lens_concat_onesub;
    "test_alignment_to_lens_concat_oneproj_left" >:: test_alignment_to_lens_concat_oneproj_left;
    "test_alignment_to_lens_concat_oneproj_right" >:: test_alignment_to_lens_concat_oneproj_right;
    "test_alignment_to_lens_concat_projs_swap" >:: test_alignment_to_lens_concat_projs_swap;
    "test_alignment_to_lens_or_basic" >:: test_alignment_to_lens_or_basic;
    "test_alignment_to_lens_or_merge_left" >:: test_alignment_to_lens_or_merge_left;
    "test_alignment_to_lens_or_merge_right" >:: test_alignment_to_lens_or_merge_right;
    "test_alignment_to_lens_or_bijection" >:: test_alignment_to_lens_or_bijection;
    "test_alignment_to_lens_or_crossing_creates" >:: test_alignment_to_lens_or_crossing_creates;
  ]

let _ = run_test_tt_main alignment_to_lens_suite
