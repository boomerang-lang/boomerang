open Stdlib
open OUnit2
open Ounit_extensions

let test_normalize_tree_empty _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Empty,IntNormalizedPTST.NormalizationScript.Empty)
    (IntNormalizedPTST.from_tree IntNormalizedPTST.NonNormalizedTree.Empty)

let test_normalize_tree_base _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Base 12345)
    ,IntNormalizedPTST.NormalizationScript.Nonempty
        (IntNormalizedPTST.NormalizationScript.Base 12345))
    (IntNormalizedPTST.from_tree
       (IntNormalizedPTST.NonNormalizedTree.Nonempty
          (IntNormalizedPTST.NonNormalizedTree.Base 12345)))

let test_normalize_tree_star _ =
  assert_normalized_tree_script_equal
    (IntNormalizedPTST.Nonempty
       (IntNormalizedPTST.Star
          (54321
          ,(IntNormalizedPTST.Base 12345,1)))
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
       (IntNormalizedPTST.Plus
          (54321
          ,[(IntNormalizedPTST.Base 12345,1)]))
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
       (IntNormalizedPTST.Plus
          (54321
          ,[(IntNormalizedPTST.Base 12345,2)]))
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
       (IntNormalizedPTST.Plus
          (54321
          ,[(IntNormalizedPTST.Base 12345,1)
           ;(IntNormalizedPTST.Base 12346,2)]))
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
       (IntNormalizedPTST.Times
          (54321
          ,[(IntNormalizedPTST.Base 12345,1)]))
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
       (IntNormalizedPTST.Times
          (54321
          ,[(IntNormalizedPTST.Base 12345,2)]))
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
       (IntNormalizedPTST.Times
          (54321
          ,[(IntNormalizedPTST.Base 12345,1)
           ;(IntNormalizedPTST.Base 12346,2)]))
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
