open Stdlib
open OUnit2
open Ounit_general_extensions
open Ounit_extensions
open Optician
open Star_semiring_tree_alignment
open Synth_structs
open Lang
open Lenscontext
open Regex_utilities

let test_normalize_tree_empty _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Empty,IntNormalizedPTST.NormalizationScript.Empty)
    (IntNormalizedPTST.from_tree IntNormalizedPTST.NonNormalizedTree.Empty)

let test_normalize_tree_base _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.Base 12345)
    ,IntNormalizedPTST.NormalizationScript.Nonempty
        (IntNormalizedPTST.NormalizationScript.Base 12345))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Base 12345)))

let test_normalize_tree_star _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.Star
          (54321
       ,(IntNormalizedPTST.Nonempty.Base 12345,1)))
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
       (IntNormalizedPTST.Nonempty.Plus
          (54321
          ,[(IntNormalizedPTST.Nonempty.Base 12345,1)]))
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
       (IntNormalizedPTST.Nonempty.Plus
          (54321
          ,[(IntNormalizedPTST.Nonempty.Base 12345,2)]))
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

let test_normalize_tree_plus_complex_perm _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Nonempty.Plus
          (54321
          ,[(IntNormalizedPTST.Nonempty.Base 12345,1)
           ;(IntNormalizedPTST.Nonempty.Base 12346,2)]))
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
       (IntNormalizedPTST.Nonempty.Times
          (54321
          ,[(IntNormalizedPTST.Nonempty.Base 12345,1)]))
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
       (IntNormalizedPTST.Nonempty.Times
          (54321
          ,[(IntNormalizedPTST.Nonempty.Base 12345,2)]))
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
       (IntNormalizedPTST.Nonempty.Times
          (54321
          ,[(IntNormalizedPTST.Nonempty.Base 12345,1)
           ;(IntNormalizedPTST.Nonempty.Base 12346,2)]))
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
    "test_normalize_tree_plus_complex_perm" >:: test_normalize_tree_plus_complex_perm;
    "test_normalize_tree_times_nodupes" >:: test_normalize_tree_times_nodupes;
    "test_normalize_tree_times_dupes" >:: test_normalize_tree_times_dupes;
    "test_normalize_tree_times_complex_perm" >:: test_normalize_tree_times_complex_perm;
  ]

let _ = run_test_tt_main normalize_from_tree_suite

module IPTSTA = IntPTSTAlignment
module IPTSTANE = IntPTSTAlignment.NonemptyNormalizedPlusStarTreeAlignment

let test_cost_none _ =
  assert_float_equal
    0.0
    (IPTSTA.cost None)

let test_cost_empty _ =
  assert_float_equal
    1.0
    (IPTSTA.cost (Some Empty))

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
    0.5
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_times
          5
          5
          []
          [(0,0)]
          []))

let test_cost_times_unmapped_right _ =
  assert_float_equal
    0.5
    (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.cost
       (IPTSTA.NonemptyNormalizedPlusStarTreeAlignment.mk_times
          5
          5
          []
          []
          [(0,0)]))

let test_cost_times_recursive _ =
  assert_float_equal
    (1. /. 3.)
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
          [(1,0)]))

let test_cost_times_imperfect_rec _ =
  assert_float_equal
    (1. /. 4.)
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
              [(0,0)]
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
    (1. /. 3.)
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
    (8. /. 15.)
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
    "test_cost_none" >:: test_cost_none;
    "test_cost_empty" >:: test_cost_empty;
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


let test_get_minimal_alignment_empty _ =
  assert_alignment_option_equal
    (Some IPTSTA.Empty)
    (IPTSTA.get_minimal_alignment
       IPTST.Empty
       IPTST.Empty)

let test_get_minimal_alignment_empty_nonempty _ =
  assert_alignment_option_equal
    None
    (IPTSTA.get_minimal_alignment
       (IPTST.Nonempty
          (IPTST.Plus (1,[])))
       IPTST.Empty)

let test_get_minimal_alignment_times_plus _ =
  assert_alignment_option_equal
    None
    (IPTSTA.get_minimal_alignment
       (IPTST.Nonempty
          (IPTST.Times (1,[])))
       (IPTST.Nonempty
          (IPTST.Plus (1,[]))))

let test_get_minimal_alignment_emptytimes_emptytimes _ =
  assert_alignment_option_equal
    (Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Times
             (1
             ,1
             ,[]
             ,[]
             ,[]))))
    (IPTSTA.get_minimal_alignment
       (IPTST.Nonempty
          (IPTST.Times (1,[])))
       (IPTST.Nonempty
          (IPTST.Times (1,[]))))

let test_get_minimal_alignment_times_emptytimes _ =
  assert_alignment_option_equal
    (Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Times
             (1
             ,1
             ,[]
             ,[0]
             ,[]))))
    (IPTSTA.get_minimal_alignment
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (1,[])])))
       (IPTST.Nonempty
          (IPTST.Times (1,[]))))

let test_get_minimal_alignment_easy_times_bijection _ =
  assert_alignment_option_equal
    (Some
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
    (IPTSTA.get_minimal_alignment
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (1,[])])))
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (1,[])]))))

let test_get_minimal_alignment_easy_times_project_left _ =
  assert_alignment_option_equal
    (Some
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
    (IPTSTA.get_minimal_alignment
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (2,[]);IPTST.Times (1,[])])))
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (1,[])]))))

let test_get_minimal_alignment_hard_times_bijection _ =
  assert_alignment_option_equal
    (Some
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
    (IPTSTA.get_minimal_alignment
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (1,[]);IPTST.Times (2,[])])))
       (IPTST.Nonempty
          (IPTST.Times
             (1
             ,[IPTST.Times (2,[]);IPTST.Times (1,[])]))))

let test_get_minimal_alignment_emptyplus_emptyplus _ =
  assert_alignment_option_equal
    (Some
       (IPTSTA.NonemptyTree
          (IPTSTA.Nonempty.Plus
             (1
             ,1
             ,[]
             ,[]
             ,[]))))
    (IPTSTA.get_minimal_alignment
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Plus (1,[])))
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Plus (1,[]))))

let test_get_minimal_alignment_plus_emptyplus _ =
  assert_alignment_option_equal
    None
    (IPTSTA.get_minimal_alignment
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[IPTST.Plus (1,[])])))
       (IPTST.Nonempty
          (IPTST.Plus (1,[]))))

let test_get_minimal_alignment_easy_plus_bijection _ =
  assert_alignment_option_equal
    (Some
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
    (IPTSTA.get_minimal_alignment
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[IPTST.Plus (1,[])])))
       (IPTST.Nonempty
          (IPTST.Plus
             (1
             ,[IPTST.Plus (1,[])]))))

let alignment_distance_suite = "Test get_minimal_alignment" >:::
  [
    "test_get_minimal_alignment_empty" >:: test_get_minimal_alignment_empty;
    "test_get_minimal_alignment_empty_nonempty" >:: test_get_minimal_alignment_empty_nonempty;
    "test_get_minimal_alignment_times_plus" >:: test_get_minimal_alignment_times_plus;
    "test_get_minimal_alignment_emptytimes_emptytimes" >:: test_get_minimal_alignment_emptytimes_emptytimes;
    "test_get_minimal_alignment_times_emptytimes" >:: test_get_minimal_alignment_times_emptytimes;
    "test_get_minimal_alignment_easy_times_bijection" >:: test_get_minimal_alignment_easy_times_bijection;
    "test_get_minimal_alignment_easy_times_project_left" >:: test_get_minimal_alignment_easy_times_project_left;
    "test_get_minimal_alignment_hard_times_bijection" >:: test_get_minimal_alignment_hard_times_bijection;
    "test_get_minimal_alignment_emptyplus_emptyplus" >:: test_get_minimal_alignment_emptyplus_emptyplus;
    "test_get_minimal_alignment_plus_emptyplus" >:: test_get_minimal_alignment_plus_emptyplus;
    "test_get_minimal_alignment_easy_plus_bijection" >:: test_get_minimal_alignment_easy_plus_bijection;
  ]

let _ = run_test_tt_main alignment_distance_suite

let test_exampled_dnf_regex_to_tree_empty _ =
  assert_rxtree_equal
    (StarSemiringTreeRep.Tree.Nonempty
       (StarSemiringTreeRep.Tree.Plus
          (Parsings [],[])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       ([],[]))

let test_exampled_dnf_regex_to_tree_epsilon _ =
  assert_rxtree_equal
    (StarSemiringTreeRep.Tree.Nonempty
       (StarSemiringTreeRep.Tree.Plus
          (Parsings [[0]]
          ,[StarSemiringTreeRep.Tree.Times
              (StarSemiringTreeRep.TD.make [[0]] [""] []
              ,[])])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       ([([],[""],[[0]])],[[0]]))

let test_exampled_dnf_regex_to_tree_epsilon _ =
  assert_rxtree_equal
    (StarSemiringTreeRep.Tree.Nonempty
       (StarSemiringTreeRep.Tree.Plus
          (Parsings [[0]]
          ,[StarSemiringTreeRep.Tree.Times
              (StarSemiringTreeRep.TD.make [[0]] [""] []
              ,[])])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       ([([],[""],[[0]])],[[0]]))

let test_exampled_dnf_regex_to_tree_base _ =
  assert_rxtree_equal
    (StarSemiringTreeRep.Tree.Nonempty
       (StarSemiringTreeRep.Tree.Plus
          (Parsings [[0];[1]]
          ,[StarSemiringTreeRep.Tree.Times
              (StarSemiringTreeRep.TD.make [[0];[1]] ["a";"b"] [Regex.make_closed (Regex.make_base "wa")]
              ,[StarSemiringTreeRep.Tree.Base
                  (StarSemiringTreeRep.BD.make
                     (Regex.make_base "aw")
                     [[0];[1]]
                     ["aw";"aw"])])])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       ([([EAClosed
             (Regex.make_base "aw"
             ,Regex.make_base "wa"
             ,Lens.Disconnect
                 (Regex.make_base "aw"
                 ,Regex.make_base "wa"
                 ,"aw"
                 ,"wa")
             ,["aw";"aw"]
             ,[[0];[1]]
             ,["aw";"aw"])]
         ,["a";"b"]
         ,[[0];[1]])]
       ,[[0];[1]]))

let test_exampled_dnf_regex_to_tree_star _ =
  assert_rxtree_equal
    (StarSemiringTreeRep.Tree.Nonempty
       (StarSemiringTreeRep.Tree.Plus
          (Parsings [[0]]
          ,[StarSemiringTreeRep.Tree.Times
              (StarSemiringTreeRep.TD.make [[0]] ["a";"b"] [Regex.make_star (Regex.make_base "c")]
              ,[StarSemiringTreeRep.Tree.Star
                  ((StarSemiringTreeRep.SD.make
                      [[0]])
                  ,(StarSemiringTreeRep.Tree.Plus
                      (Parsings [[0;0];[0;1]]
                      ,[StarSemiringTreeRep.Tree.Times
                          (StarSemiringTreeRep.TD.make [[0;0];[0;1]] ["c"] []
                          ,[])])))])])))
    (StarSemiringTreeRep.exampled_dnf_regex_to_tree
       ([([EAStar
             (([([]
                ,["c"]
                ,[[0;0];[0;1]])]
              ,[[0;0];[0;1]])
             ,[[0]]
             ,Regex.make_star (Regex.make_base "c"))
          ]
         ,["a";"b"]
         ,[[0]])]
       ,[[0]]))

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
  assert_alignment_equal
    (StarSemiringTreeRep.Alignment.NonemptyTree
       (StarSemiringTreeRep.Alignment.Nonempty.Plus
          (StarSemiringTreeRep.PD.make []
          ,StarSemiringTreeRep.PD.make []
          ,[]
          ,[]
          ,[])))
    (Gen.DNFSynth.kinda_rigid_synth LensContext.empty Regex.make_empty Regex.make_empty [])

let test_kinda_rigid_synth_empty _ =
  assert_alignment_equal
    (StarSemiringTreeRep.Alignment.NonemptyTree
       (StarSemiringTreeRep.Alignment.Nonempty.Plus
          (StarSemiringTreeRep.PD.make []
          ,StarSemiringTreeRep.PD.make []
          ,[]
          ,[]
          ,[])))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       Regex.make_empty
       Regex.make_empty [])

let test_kinda_rigid_synth_project_lastname _ =
  let uppercase = (Regex.from_char_set [(65,91)]) in
  let lowercases = Regex.make_star (Regex.from_char_set [(97,123)]) in
  let name =
    iteratively_deepen
      (Regex.make_concat
         uppercase
         lowercases)
  in
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
  assert_alignment_equal
    (StarSemiringTreeRep.Alignment.NonemptyTree
       (StarSemiringTreeRep.Alignment.Nonempty.Plus
          (StarSemiringTreeRep.PD.make []
          ,StarSemiringTreeRep.PD.make []
          ,[]
          ,[]
          ,[])))
    (Gen.DNFSynth.kinda_rigid_synth
       LensContext.empty
       name
       firstlast
       [("Anders","Anders Miltner")])

let kinda_rigid_synth_suite = "Test kinda_rigid_synth" >:::
  [
    "test_kinda_rigid_synth_project_lastname" >:: test_kinda_rigid_synth_empty;
    (*"test_kinda_rigid_synth_project_lastname" >:: test_kinda_rigid_synth_project_lastname;*)
  ]

let _ = run_test_tt_main kinda_rigid_synth_suite
