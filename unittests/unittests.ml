open Stdlib
open OUnit2
open Ounit_general_extensions
open Ounit_extensions

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
       format_exp e1;
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

module INPTSTA = IntNormalizedPTSTAlignment

let test_cost_none _ =
  assert_float_equal
    0.0
    (INPTSTA.cost None)

let test_cost_empty _ =
  assert_float_equal
    1.0
    (INPTSTA.cost (Some Empty))

let test_cost_base _ =
  assert_float_equal
    0.5
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_base 2))))

let test_cost_star _ =
  assert_float_equal
    0.25
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_star
                2
                (INPTSTA.NonemptyPlusStarTreeAlignment.mk_base 4)))))

let test_cost_times_singleton _ =
  assert_float_equal
    0.0
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_times
                5
                (Permutation.create [])
                []
                []
                []))))

let test_cost_times_unmapped_left _ =
  assert_float_equal
    0.5
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_times
                5
                (Permutation.create [])
                [(0,0)]
                []
                []))))

let test_cost_times_unmapped_right _ =
  assert_float_equal
    0.5
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_times
                5
                (Permutation.create [])
                []
                [(0,0)]
                []))))

let test_cost_times_recursive _ =
  assert_float_equal
    (1. /. 3.)
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_times
                5
                (Permutation.create [0])
                []
                [(1,0)]
                [INPTSTA.NonemptyPlusStarTreeAlignment.mk_times
                    5
                    (Permutation.create [])
                    []
                    []
                    []]))))

let test_cost_times_imperfect_rec _ =
  assert_float_equal
    (1. /. 4.)
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_times
                5
                (Permutation.create [0])
                []
                []
                [INPTSTA.NonemptyPlusStarTreeAlignment.mk_times
                    8
                    (Permutation.create [])
                    []
                    [(0,0)]
                    []]))))

let test_cost_plus_singleton _ =
  assert_float_equal
    0.
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                5
                []
                []
                []))))

let test_cost_plus_single_rec _ =
  assert_float_equal
    0.
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                5
                [(0,0),(0,0),(INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                                5
                                []
                                []
                                [])]
                [((0,0),(0,0))]
                [((0,0),(0,0))]))))

let test_cost_plus_double_rec _ =
  assert_float_equal
    0.
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                5
                [(0,0),(0,0),(INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                                5
                                []
                                []
                                [])]
                [((0,0),(0,0))]
                [((0,0),(0,0))]))))

let test_cost_plus_double_rec _ =
  assert_float_equal
    0.
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                5
                [(0,0),(1,0),(INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                                5
                                []
                                []
                                [])
                ;(1,0),(0,0),(INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                                8
                                []
                                []
                                [])]
                [((0,0),(1,0));((1,0),(0,0))]
                [((0,0),(1,0));((1,0),(0,0))]))))

let test_cost_plus_double_rec _ =
  assert_float_equal
    0.
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                5
                [(0,0),(1,0),(INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                                5
                                []
                                []
                                [])
                ;(1,0),(0,0),(INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                                8
                                []
                                []
                                [])]
                [((0,0),(1,0));((1,0),(0,0))]
                [((0,0),(1,0));((1,0),(0,0))]))))

let test_cost_plus_single_merge _ =
  assert_float_equal
    0.5
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                5
                [(0,0),(0,0),(INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                                5
                                []
                                []
                                [])
                ;(0,0),(0,1),(INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                                8
                                []
                                []
                                [])]
                [((0,0),(0,1))]
                [((0,0),(0,0));((0,1),(0,0))]))))

let test_cost_plus_crossing_merge _ =
  assert_float_equal
    0.5
    (INPTSTA.cost
       (Some
          (INPTSTA.NonemptyTree
             (INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                5
                [(0,0),(0,0),(INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                                5
                                []
                                []
                                [])
                ;(0,0),(0,1),(INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                                8
                                []
                                []
                                [])
                ;(0,1),(0,0),(INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                                8
                                []
                                []
                                [])
                ;(0,1),(0,1),(INPTSTA.NonemptyPlusStarTreeAlignment.mk_plus
                                8
                                []
                                []
                                [])]
                [((0,0),(0,0));((0,1),(0,1))]
                [((0,0),(0,0));((0,1),(0,1))]))))

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
