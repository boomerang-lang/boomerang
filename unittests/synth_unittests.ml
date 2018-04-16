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
          [(IntNormalizedPTST.Nonempty.mk_base 12345,1)])
    ,IntNormalizedPTST.NormalizationScript.Nonempty
        (IntNormalizedPTST.NormalizationScript.Plus
           (IntNormalizedPTST.NormalizationScript.PD_NormalizationLabel.make
              ~label:54321
              ~perm:([
                  CountedPermutation.make_element
                    ~old_index:0
                    ~new_index:(0,0)])
           ,[IntNormalizedPTST.NormalizationScript.Base 12345])))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Plus
             (54321
             ,[IntNormalizedPTST.NonNormalizedTree.Base 12345]))))

let test_normalize_tree_plus_dupes _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_plus
          54321
          [(IntNormalizedPTST.Nonempty.mk_base 12345,2)])
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
           ,[IntNormalizedPTST.NormalizationScript.Base 12345
            ;IntNormalizedPTST.NormalizationScript.Base 12345])))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Plus
             (54321
             ,[IntNormalizedPTST.NonNormalizedTree.Base 12345
              ;IntNormalizedPTST.NonNormalizedTree.Base 12345]))))

let test_normalize_tree_plus_many_dupes_with_times _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_plus
          54321
          [(IntNormalizedPTST.Nonempty.mk_times 123 [],7)])
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
           ,[IntNormalizedPTST.NormalizationScript.mk_times
               (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:123 ~perm:[])
               []
            ;IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:246 ~perm:[])
                []
            ;IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:369 ~perm:[])
                []
            ;IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:492 ~perm:[])
                []
            ;IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:615 ~perm:[])
                []
            ;IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:738 ~perm:[])
                []
            ;IntNormalizedPTST.NormalizationScript.mk_times
                (IntNormalizedPTST.NormalizationScript.TD_NormalizationLabel.make ~label:861 ~perm:[])
                []])))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Plus
             (54321
             ,[IntNormalizedPTST.NonNormalizedTree.mk_times 123 []
              ;IntNormalizedPTST.NonNormalizedTree.mk_times 246 []
              ;IntNormalizedPTST.NonNormalizedTree.mk_times 369 []
              ;IntNormalizedPTST.NonNormalizedTree.mk_times 492 []
              ;IntNormalizedPTST.NonNormalizedTree.mk_times 615 []
              ;IntNormalizedPTST.NonNormalizedTree.mk_times 738 []
              ;IntNormalizedPTST.NonNormalizedTree.mk_times 861 []]))))

let test_normalize_tree_plus_complex_perm _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_plus
          54321
          [(IntNormalizedPTST.Nonempty.mk_base 12345,1)
          ;(IntNormalizedPTST.Nonempty.mk_base 12346,2)])
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
           ,[IntNormalizedPTST.NormalizationScript.Base 12346
            ;IntNormalizedPTST.NormalizationScript.Base 12345
            ;IntNormalizedPTST.NormalizationScript.Base 12346])))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Plus
             (54321
             ,[IntNormalizedPTST.NonNormalizedTree.Base 12346
              ;IntNormalizedPTST.NonNormalizedTree.Base 12345
              ;IntNormalizedPTST.NonNormalizedTree.Base 12346]))))

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
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.mk_times
          54321
          [(IntNormalizedPTST.Nonempty.mk_base 12345,1)
          ;(IntNormalizedPTST.Nonempty.mk_base 12346,2)])
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
               ,IPTSTA.Nonempty.Times
                   (1
                   ,1
                   ,[]
                   ,[]
                   ,[]))]
             ,[]
             ,[]))))
    ,0.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (1,[])])))
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (1,[])]))))

let test_get_minimal_alignment_and_cost_easy_times_project_left _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Times
             (1
             ,1
             ,[(1
               ,0
               ,IPTSTA.Nonempty.Times
                   (1
                   ,1
                   ,[]
                   ,[]
                   ,[]))]
             ,[0]
             ,[]))))
    ,2.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Base 2;IPTST.Times (1,[])])))
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (1,[])]))))

let test_get_minimal_alignment_and_cost_hard_times_bijection _ =
  assert_alignment_option_cost_equal
    ((Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Times
             (1
             ,1
             ,[(1
               ,0
               ,IPTSTA.Nonempty.Times
                   (2
                   ,2
                   ,[]
                   ,[]
                   ,[]))
              ;(0
               ,1
               ,IPTSTA.Nonempty.Times
                   (1
                   ,1
                   ,[]
                   ,[]
                   ,[]))]
             ,[]
             ,[]))))
    ,0.)
    (IPTSTA.get_minimal_alignment_and_cost
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (1,[]);IPTST.Times (2,[])])))
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (2,[]);IPTST.Times (1,[])]))))

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
             ,[IPTST.Plus (1,[])])))
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
             ,[IPTST.Plus (1,[])])))
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[IPTST.Plus (1,[])]))))

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
             ,[IPTST.Plus (1,[]);IPTST.Plus (2,[]);IPTST.Plus (0,[])])))
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[IPTST.Plus (1,[]);IPTST.Plus (0,[]);IPTST.Plus (2,[])]))))

let alignment_distance_suite = "Test get_minimal_alignment_and_cost" >:::
  [
    "test_get_minimal_alignment_and_cost_empty" >:: test_get_minimal_alignment_and_cost_empty;
    "test_get_minimal_alignment_and_cost_empty_nonempty" >:: test_get_minimal_alignment_and_cost_empty_nonempty;
    "test_get_minimal_alignment_and_cost_times_plus" >:: test_get_minimal_alignment_and_cost_times_plus;
    "test_get_minimal_alignment_and_cost_emptytimes_emptytimes" >:: test_get_minimal_alignment_and_cost_emptytimes_emptytimes;
    "test_get_minimal_alignment_and_cost_times_emptytimes" >:: test_get_minimal_alignment_and_cost_times_emptytimes;
    "test_get_minimal_alignment_and_cost_easy_times_bijection" >:: test_get_minimal_alignment_and_cost_easy_times_bijection;
    "test_get_minimal_alignment_and_cost_easy_times_project_left" >:: test_get_minimal_alignment_and_cost_easy_times_project_left;
    "test_get_minimal_alignment_and_cost_hard_times_bijection" >:: test_get_minimal_alignment_and_cost_hard_times_bijection;
    "test_get_minimal_alignment_and_cost_emptyplus_emptyplus" >:: test_get_minimal_alignment_and_cost_emptyplus_emptyplus;
    "test_get_minimal_alignment_and_cost_plus_emptyplus" >:: test_get_minimal_alignment_and_cost_plus_emptyplus;
    "test_get_minimal_alignment_and_cost_easy_plus_bijection" >:: test_get_minimal_alignment_and_cost_easy_plus_bijection;
    "test_get_minimal_alignment_and_cost_hard_plus_bijection" >:: test_get_minimal_alignment_and_cost_hard_plus_bijection;
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
          ,[StarSemiringTreeRep.Tree.Times
              (StarSemiringTreeRep.TD.make ex_data [""] []
              ,[])])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       ([([],[""],ex_data)],ex_data))

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
          ,[StarSemiringTreeRep.Tree.Times
              (StarSemiringTreeRep.TD.make ex_data [""] []
              ,[])])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       ([([],[""],ex_data)],ex_data))

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
          ,[StarSemiringTreeRep.Tree.Times
              (StarSemiringTreeRep.TD.make ex_data ["a";"b"] [Regex.make_closed (Regex.make_base "wa")]
              ,[StarSemiringTreeRep.Tree.Base
                  (StarSemiringTreeRep.BD.make
                     (Regex.make_base "aw")
                     ex_data
                     str_data_post)])])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       ([([EAClosed
             (Regex.make_base "aw"
             ,Regex.make_base "wa"
             ,Lens.Disconnect
                 (Regex.make_base "aw"
                 ,Regex.make_base "wa"
                 ,"aw"
                 ,"wa")
             ,str_data_pre
             ,ex_data
             ,str_data_post)]
         ,["a";"b"]
         ,ex_data)]
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
          ,[StarSemiringTreeRep.Tree.Times
              (StarSemiringTreeRep.TD.make ex_data ["a";"b"] [Regex.make_star (Regex.make_base "c")]
              ,[StarSemiringTreeRep.Tree.Star
                  ((StarSemiringTreeRep.SD.make
                      ex_data)
                  ,(StarSemiringTreeRep.Tree.Plus
                      (StarSemiringTreeRep.PD.make ex_data_inside
                      ,[StarSemiringTreeRep.Tree.Times
                          (StarSemiringTreeRep.TD.make ex_data_inside ["c"] []
                          ,[])])))])])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       ([([EAStar
             (([([]
                ,["c"]
                ,ex_data_inside)]
              ,ex_data_inside)
             ,ex_data
             ,Regex.make_star (Regex.make_base "c"))
          ]
         ,["a";"b"]
         ,ex_data)]
       ,ex_data))

let exampled_dnf_regex_to_tree_suite = "Test exampled_dnf_regex_to_tree" >:::
                                       [
                                         "test_exampled_dnf_regex_to_tree_empty" >:: test_exampled_dnf_regex_to_tree_empty;
                                         "test_exampled_dnf_regex_to_tree_epsilon" >:: test_exampled_dnf_regex_to_tree_epsilon;
                                         "test_exampled_dnf_regex_to_tree_base" >:: test_exampled_dnf_regex_to_tree_base;
                                         "test_exampled_dnf_regex_to_tree_star" >:: test_exampled_dnf_regex_to_tree_star;
  ]

let _ = run_test_tt_main exampled_dnf_regex_to_tree_suite

let exampled_dnf_regex_to_tree_suite = "Test exampled_dnf_regex_to_tree" >:::
  [
    "test_exampled_dnf_regex_to_tree_empty" >:: test_exampled_dnf_regex_to_tree_empty;
    "test_exampled_dnf_regex_to_tree_epsilon" >:: test_exampled_dnf_regex_to_tree_epsilon;
    "test_exampled_dnf_regex_to_tree_base" >:: test_exampled_dnf_regex_to_tree_base;
    "test_exampled_dnf_regex_to_tree_star" >:: test_exampled_dnf_regex_to_tree_star;
  ]

let exampled_dnf_regex_to_tree_suite = "Test exampled_dnf_regex_to_tree" >:::
  [
    "test_exampled_dnf_regex_to_tree_empty" >:: test_exampled_dnf_regex_to_tree_empty;
    "test_exampled_dnf_regex_to_tree_epsilon" >:: test_exampled_dnf_regex_to_tree_epsilon;
    "test_exampled_dnf_regex_to_tree_base" >:: test_exampled_dnf_regex_to_tree_base;
    "test_exampled_dnf_regex_to_tree_star" >:: test_exampled_dnf_regex_to_tree_star;
  ]

let test_kinda_rigid_synth_empty _ =
  assert_lens_float_option_equal
    (Some (Lens.zero,0.))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       Regex.empty
       Regex.empty
       []
       []
       []
       [])

let test_kinda_rigid_synth_project_lastname _ =
  let uppercase = (Regex.from_char_set [(65,90)]) in
  let lowercases = Regex.make_star (Regex.from_char_set [(97,122)]) in
  let name =
    iteratively_deepen
      (Regex.make_concat
         uppercase
         lowercases)
  in
  let name_opened = Option.value_exn (Regex.separate_closed name) in
  let names =
    Regex.make_star
      (Regex.make_concat
         (Regex.make_base " ")
         name)
  in
  let firstlast =
    Regex.make_concat
      name
      names
  in
  assert_lens_float_option_equal
    (Some
       (Lens.make_concat
          (Lens.make_const "" "")
          (Lens.make_concat
             (Lens.make_concat
                (Lens.make_ident name_opened)
                (Lens.make_const "" ""))
             (Lens.make_concat
                (Lens.make_disconnect (Regex.make_base "") names "" "")
                (Lens.make_const "" "")))
       ,20.8017588726))
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
       ,20.8017588726))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       name
       firstlast
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
       ,37.6035177451))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       name
       name
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
       ,20.8017588726))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       firstlast
       name
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
       ,20.8017588726))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       firstlast
       name
       []
       []
       []
       [("Anders","Anders Nnders","Anders Anders")])

let kinda_rigid_synth_suite = "Test kinda_rigid_synth" >:::
  [
    "test_kinda_rigid_synth_empty" >:: test_kinda_rigid_synth_empty;
    "test_kinda_rigid_synth_project_lastname" >:: test_kinda_rigid_synth_project_lastname;
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
       (StarSemiringTreeRep.Alignment.NonemptyTree
          (StarSemiringTreeRep.Alignment.Nonempty.Base
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
       (StarSemiringTreeRep.Alignment.NonemptyTree
          (StarSemiringTreeRep.Alignment.Nonempty.Star
             (StarSemiringTreeRep.SD.make ex_data
             ,StarSemiringTreeRep.SD.make ex_data
             ,StarSemiringTreeRep.Alignment.Nonempty.Base
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
       (StarSemiringTreeRep.Alignment.NonemptyTree
          (StarSemiringTreeRep.Alignment.Nonempty.Times
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
       (StarSemiringTreeRep.Alignment.NonemptyTree
          (StarSemiringTreeRep.Alignment.Nonempty.Times
             (StarSemiringTreeRep.TD.make
                ex_data
                ["a1";"a2"]
                [Regex.make_closed (Regex.make_base "t")]
             ,StarSemiringTreeRep.TD.make
                 ex_data
                 ["b1";"b2"]
                 [Regex.make_closed (Regex.make_base "t")]
             ,[(0
               ,0
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
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
       (StarSemiringTreeRep.Alignment.NonemptyTree
          (StarSemiringTreeRep.Alignment.Nonempty.Times
             (StarSemiringTreeRep.TD.make
                ex_data
                ["a1";"a2"]
                [Regex.make_closed (Regex.make_base "t")]
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
       (StarSemiringTreeRep.Alignment.NonemptyTree
          (StarSemiringTreeRep.Alignment.Nonempty.Times
             (StarSemiringTreeRep.TD.make
                ex_data
                ["a1"]
                []
             ,StarSemiringTreeRep.TD.make
                 ex_data
                 ["b1";"b2"]
                 [Regex.make_closed (Regex.make_base "u")]
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
       (StarSemiringTreeRep.Alignment.NonemptyTree
          (StarSemiringTreeRep.Alignment.Nonempty.Times
             (StarSemiringTreeRep.TD.make
                ex_data
                ["a1";"a2";"a3";"a4"]
                [Regex.make_closed (Regex.make_base "t")
                ;Regex.make_closed (Regex.make_base "u")
                ;Regex.make_closed (Regex.make_base "v")]
             ,StarSemiringTreeRep.TD.make
                 ex_data
                 ["b1";"b2";"b3";"b4"]
                 [Regex.make_closed (Regex.make_base "v")
                 ;Regex.make_closed (Regex.make_base "t")
                 ;Regex.make_closed (Regex.make_base "w")]
             ,[(0
               ,1
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_closed (Regex.make_base "t"))))
              ;(2
               ,0
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
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
       (StarSemiringTreeRep.Alignment.NonemptyTree
          (StarSemiringTreeRep.Alignment.Nonempty.Plus
             (StarSemiringTreeRep.PD.make ex_data
             ,StarSemiringTreeRep.PD.make ex_data
             ,[(0
               ,0
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
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
       (StarSemiringTreeRep.Alignment.NonemptyTree
          (StarSemiringTreeRep.Alignment.Nonempty.Plus
             (StarSemiringTreeRep.PD.make ex_data
             ,StarSemiringTreeRep.PD.make ex_data
             ,[(0
               ,0
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_base "t")))
              ;(1
               ,0
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
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
       (StarSemiringTreeRep.Alignment.NonemptyTree
          (StarSemiringTreeRep.Alignment.Nonempty.Plus
             (StarSemiringTreeRep.PD.make ex_data
             ,StarSemiringTreeRep.PD.make ex_data
             ,[(0
               ,0
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_base "t")))
              ;(0
               ,1
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
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
       (StarSemiringTreeRep.Alignment.NonemptyTree
          (StarSemiringTreeRep.Alignment.Nonempty.Plus
             (StarSemiringTreeRep.PD.make ex_data
             ,StarSemiringTreeRep.PD.make ex_data
             ,[(0
               ,1
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_base "t")))
              ;(1
               ,0
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
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
       (StarSemiringTreeRep.Alignment.NonemptyTree
          (StarSemiringTreeRep.Alignment.Nonempty.Plus
             (StarSemiringTreeRep.PD.make ex_data
             ,StarSemiringTreeRep.PD.make ex_data
             ,[(0
               ,0
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
                   (Lens.make_ident (Regex.make_base "t")))
              ;(0
               ,1
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
                   (Lens.make_const "t" "u"))
              ;(1
               ,0
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
                   (Lens.make_const "u" "t"))
              ;(1
               ,1
               ,StarSemiringTreeRep.Alignment.Nonempty.Base
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
