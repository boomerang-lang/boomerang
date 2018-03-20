open MyStdlib
open Lang
open Regex_utilities
open Star_semiring_tree_alignment
open Normalized_lang

module QueueElement = struct
  type t =
    {
      r1 : Regex.t;
      r2 : Regex.t;
      expansions_performed : int;
      expansions_inferred : int;
      expansions_forced : int;
    }
  [@@deriving ord, show, hash, make]

  let get_r1
      (q:t)
    : Regex.t =
    q.r1

  let get_r2
      (q:t)
    : Regex.t =
    q.r2

  let get_expansions_performed
      (q:t)
    : int =
    q.expansions_performed

  let get_expansions_inferred
      (q:t)
    : int =
    q.expansions_inferred

  let get_expansions_forced
      (q:t)
    : int =
    q.expansions_forced

  let nqe_to_tuple
      (q:t)
    : Regex.t * Regex.t * int * int * int =
    (q.r1,
     q.r2,
     q.expansions_performed,
     q.expansions_inferred,
     q.expansions_forced)

  let compare
      (q1:t)
      (q2:t)
    : comparison =
    quint_compare
      Regex.compare
      Regex.compare
      (fun _ _ -> 0)
      (fun _ _ -> 0)
      (fun _ _ -> 0)
      (nqe_to_tuple q1)
      (nqe_to_tuple q2)

  module Priority = struct
    type t = float
    [@@deriving show, hash]

    let compare
        (e1:t)
        (e2:t) 
      : int =
      Float.compare e1 e2
  end

  let priority
      (qe : t)
    : Priority.t =
    Float.of_int qe.expansions_performed
end

module SymmetricQueueElement = struct
  type t = 
    {
      r1 : Regex.t;
      r2 : Regex.t;
      expansion_choices : int list;
      expansions_performed : int;
      expansions_inferred : int;
      expansions_forced : int;
    }
  [@@deriving ord, show, hash, make]

  let get_r1
      (q:t)
    : Regex.t =
    q.r1

  let get_r2
      (q:t)
    : Regex.t =
    q.r2

  let get_expansions_choices
      (q:t)
    : int list =
    q.expansion_choices

  let get_expansions_performed
      (q:t)
    : int =
    q.expansions_performed

  let get_expansions_inferred
      (q:t)
    : int =
    q.expansions_inferred

  let get_expansions_forced
      (q:t)
    : int =
    q.expansions_forced

  let init
      (r1:Regex.t)
      (r2:Regex.t)
    : t =
    make
      ~r1:r1
      ~r2:r2
      ~expansion_choices:[]
      ~expansions_performed:0
      ~expansions_inferred:0
      ~expansions_forced:0
      ()

  let nqe_to_tuple
      (q:t)
    : Regex.t * Regex.t * int list * int * int * int =
    (q.r1,
     q.r2,
     q.expansion_choices,
     q.expansions_performed,
     q.expansions_inferred,
     q.expansions_forced)

  let compare
      (q1:t)
      (q2:t)
    : comparison =
    sext_compare
      Regex.compare
      Regex.compare
      (fun _ _ -> 0)
      (fun _ _ -> 0)
      (fun _ _ -> 0)
      (fun _ _ -> 0)
      (nqe_to_tuple q1)
      (nqe_to_tuple q2)

  module Priority = struct
    type t = float
    [@@deriving show, hash]

    let compare
        (e1:t)
        (e2:t) 
      : int =
      Float.compare e1 e2
  end

  let priority
      (qe : t)
    : Priority.t =
    let probability_of_choice
        (n:int)
      : float =
      1. /. (Float.of_int n)
    in
    let ps_of_choices =
      List.map
        ~f:probability_of_choice
        qe.expansion_choices
    in
    1. -.
    List.fold_left
      ~f:( *. )
      ~init:1.
      ps_of_choices
end



module StarSemiringTreeRep =
struct
  module PD =
  struct
    type t =
      {
        parsings : int list list example_data ;
      }
    [@@deriving ord, show, hash, make]

    let are_compatible
        (v1:t)
        (v2:t)
      : bool =
      let r_arg1_parsings = v1.parsings.arg1_data in
      let r_result_parsings = v2.parsings.output_data in
      let l_arg1_parsings = v2.parsings.arg1_data in
      let l_result_parsings = v1.parsings.output_data in
      let is_compat_r =
        is_equal
          (compare_list
             ~cmp:(compare_list ~cmp:Int.compare)
             r_arg1_parsings
             r_result_parsings)
      in
      let is_compat_l =
        is_equal
          (compare_list
             ~cmp:(compare_list ~cmp:Int.compare)
             l_arg1_parsings
             l_result_parsings)
      in
      is_compat_r && is_compat_l

    let requires_mapping
        (v:t)
      : bool =
      not
        (List.for_all
           ~f:(fun p ->
               List.mem
                 ~equal:(is_equal %% (compare_list ~cmp:Int.compare))
                 v.parsings.output_data
                 p)
           v.parsings.arg2_data)

    module Default = IntModule
    let extract_default _ = failwith "ah"
  end

  module TD =
  struct
    type t =
      {
        parsings : int list list example_data ;
        strings  : string list   ;
        atoms    : Regex.t list  ;
      }
    [@@deriving ord, show, hash]

    let make
        (parsings:int list list example_data)
        (strings:string list)
        (atoms:Regex.t list)
      : t =
      {
        parsings = parsings ;
        strings  = strings  ;
        atoms    = atoms    ;
      }

    let get_strings
        (v:t)
      : string list =
      v.strings

    let get_atoms
        (v:t)
      : Regex.t list =
      v.atoms

    let are_compatible
        (v1:t)
        (v2:t)
      : bool =
      let r_arg1_parsings = v1.parsings.arg1_data in
      let r_result_parsings = v2.parsings.output_data in
      let l_arg1_parsings = v2.parsings.arg1_data in
      let l_result_parsings = v1.parsings.output_data in
      let is_compat_r =
        is_equal
          (compare_list
             ~cmp:(compare_list ~cmp:Int.compare)
             r_arg1_parsings
             r_result_parsings)
      in
      let is_compat_l =
        is_equal
          (compare_list
             ~cmp:(compare_list ~cmp:Int.compare)
             l_arg1_parsings
             l_result_parsings)
      in
      is_compat_r && is_compat_l

    let requires_mapping
        (v:t)
      : bool =
      not
        (List.for_all
           ~f:(fun p ->
               List.mem
                 ~equal:(is_equal %% (compare_list ~cmp:Int.compare))
                 v.parsings.output_data
                 p)
           v.parsings.arg2_data)

    module Default = IntModule
    let extract_default _ = failwith "ah"
  end

  module SD =
  struct
    type t =
      {
        parsings : int list list example_data ;
      }
    [@@deriving ord, show, hash, make]

    let are_compatible
        (v1:t)
        (v2:t)
      : bool =
      let r_arg1_parsings = v1.parsings.arg1_data in
      let r_result_parsings = v2.parsings.output_data in
      let l_arg1_parsings = v2.parsings.arg1_data in
      let l_result_parsings = v1.parsings.output_data in
      let is_compat_r =
        is_equal
          (compare_list
             ~cmp:(compare_list ~cmp:Int.compare)
             r_arg1_parsings
             r_result_parsings)
      in
      let is_compat_l =
        is_equal
          (compare_list
             ~cmp:(compare_list ~cmp:Int.compare)
             l_arg1_parsings
             l_result_parsings)
      in
      is_compat_r && is_compat_l

    let requires_mapping
        (v:t)
      : bool =
      not
        (List.for_all
           ~f:(fun p ->
               List.mem
                 ~equal:(is_equal %% (compare_list ~cmp:Int.compare))
                 v.parsings.output_data
                 p)
           v.parsings.arg2_data)

    module Default = IntModule
    let extract_default _ = failwith "ah"
  end

  module BD =
  struct
    type t =
      {
        parsings : int list list example_data ;
        strings  : string list example_data   ;
        regex    : Regex.t                    ;
      }
    [@@deriving ord, show, hash]

    let make
        (r:Regex.t)
        (ill:int list list example_data)
        (ss:string list example_data)
      : t =
      {
        parsings = ill ;
        strings  = ss  ;
        regex    = r   ;
      }

    module Alignment =
    struct
      include Lens
      let cost _ = 1.
    end

    let get_alignment
        (v1:t)
        (v2:t)
      : Lens.t option =
      (*TODO contextual lenses*)
      let creater_args_parsings = v1.parsings.arg1_data in
      let creater_results_parsings = v2.parsings.output_data in
      let createl_args_parsings = v2.parsings.arg1_data in
      let createl_results_parsings = v1.parsings.output_data in
      let creater_args_strings = v1.strings.arg1_data in
      let creater_results_strings = v2.strings.output_data in
      let createl_args_strings = v2.strings.arg1_data in
      let createl_results_strings = v1.strings.output_data in
      let is_eq_creater =
        is_equal
          (compare_list
             ~cmp:(compare_list ~cmp:Int.compare)
             creater_args_parsings
             creater_results_parsings)
        &&
        is_equal
          (compare_list
             ~cmp:(String.compare)
             creater_args_strings
             creater_results_strings)
      in
      let is_eq_createl =
        is_equal
          (compare_list
             ~cmp:(compare_list ~cmp:Int.compare)
             createl_args_parsings
             createl_results_parsings)
        &&
        is_equal
          (compare_list
             ~cmp:(String.compare)
             createl_args_strings
             createl_results_strings)
      in
      if is_eq_creater && is_eq_createl then
        Some (Lens.make_ident v1.regex)
      else
        None

    let cost
        (_:unit)
      : float =
      0.

    let requires_mapping
        (v:t)
      : bool =
      let arg2_parsings_strings =
        List.zip_exn
          v.parsings.arg2_data
          v.strings.arg2_data
      in
      let output_parsings_strings =
        List.zip_exn
          v.parsings.output_data
          v.strings.output_data
      in
      not
        (List.for_all
           ~f:(fun p ->
               List.mem
                 ~equal:(is_equal
                         %% (pair_compare
                               (compare_list ~cmp:Int.compare)
                               String.compare))
                 output_parsings_strings
                 p)
           arg2_parsings_strings)

    module Default = IntModule
    let extract_default _ = failwith "ah"
  end

  module Alignment = PlusTimesStarTreeAlignmentOf(PD)(TD)(SD)(BD)
  module Tree = Alignment.Tree

  let exampled_dnf_regex_to_tree
      (ed:exampled_dnf_regex)
    : Tree.t =
    let rec exampled_dnf_regex_to_nonempty_tree
        ((cs,ill):exampled_dnf_regex)
      : Tree.nonempty_t =
      let children =
        List.map
          ~f:exampled_clause_to_nonempty_tree
          cs
      in
      Tree.mk_plus (PD.make ill) children
    and exampled_clause_to_nonempty_tree
        ((ats,ss,ill):exampled_clause)
      : Tree.nonempty_t =
      let (children,regexps) =
        List.unzip
          (List.map
             ~f:(fun ec ->
                 (exampled_atom_to_nonempty_tree ec
                 ,get_atom_regex ec))
             ats)
      in
      Tree.mk_times (TD.make ill ss regexps) children
    and exampled_atom_to_nonempty_tree
        (a:exampled_atom)
      : Tree.nonempty_t =
      begin match a with
        | EAClosed (r,_,_,_,ill,ss) ->
          Tree.mk_base (BD.make r ill ss)
        | EAStar (d,ill,_) ->
          let child = exampled_dnf_regex_to_nonempty_tree d in
          Tree.mk_star (SD.make ill) child
      end
    in
    Tree.mk_nonempty (exampled_dnf_regex_to_nonempty_tree ed)

  module CreateDict = DictOf(IntModule)(IntModule)
  module MatchDict = DictOf(PairOf(IntModule)(IntModule))(Lens)

  let alignment_to_lens
      (a:Alignment.t)
    : Lens.t option =
    let rec nonempty_alignment_to_lens
        (a:Alignment.Nonempty.t)
      : Lens.t =
      begin match a with
        | Alignment.Nonempty.Base l -> l
        | Alignment.Nonempty.Plus (_,_,matches,createls,creaters) ->
          let cdl =
            CreateDict.from_kvp_list
              (List.mapi ~f:(curry ident) createls)
          in
          let cdr =
            CreateDict.from_kvp_list
              (List.mapi ~f:(curry ident) creaters)
          in
          let md =
            MatchDict.from_kvp_list
              (List.map
                 ~f:(fun (i,j,a) -> ((i,j),nonempty_alignment_to_lens a))
                 matches)
          in
          let (ll,cdl,cdr,md) =
            fold_until_completion
              ~f:(fun (ll,cdl,cdr,md) ->
                  let addable
                      (cd:CreateDict.t)
                      (i:int)
                      (j:int)
                    : bool =
                    begin match CreateDict.lookup cd i with
                      | None -> true
                      | Some j' -> is_equal (Int.compare j j')
                    end
                  in
                  let first_possible_option =
                    MatchDict.first
                      ~f:(fun (i,j) _ ->
                          addable cdl i j
                          && addable cdr j i)
                      md
                  in
                  begin match first_possible_option with
                    | None -> Right (ll,cdl,cdr,md)
                    | Some ((i,j),l) ->
                      let cdl = CreateDict.remove cdl i in
                      let cdr = CreateDict.remove cdr i in
                      let md = MatchDict.remove md (i,j) in
                      Left (l::ll,cdl,cdr,md)
                  end)
              ([],cdl,cdr,md)
          in
          if MatchDict.is_empty md then
            fold_on_head_with_default
              ~f:Lens.make_or
              ~default:Lens.zero
              (List.rev ll)
          else
            let mapping_triples =
              List.map
                ~f:(fun ((i,j),l) ->
                    let (stype,vtype) = Typing.type_lens l in
                    let istring = Int.to_string i in
                    let jstring = Int.to_string j in
                    let l_left =
                      Lens.make_concat
                        (Lens.make_const "" jstring)
                        (Lens.make_ident stype)
                    in
                    let l_right =
                      Lens.make_concat
                        (Lens.make_const istring "")
                        (Lens.make_ident vtype)
                    in
                    let l_middle =
                      Lens.make_concat
                        (Lens.make_const (Int.to_string j) (Int.to_string i))
                        l
                    in
                    ((i,j,l_left)
                    ,l_middle
                    ,(j,i,l_right)))
                (MatchDict.as_kvp_list md)
            in
            let (lij_list_l,l_list_m,lij_list_r) =
              List.unzip3
                mapping_triples
            in
            let lens_sort_compare_by_createdict
                (cd:CreateDict.t)
                ((i1,j1,_):int * int * Lens.t)
                ((i2,j2,_):int * int * Lens.t)
              : int =
              let c = Int.compare i1 i2 in
              if is_equal c then
                let jo = CreateDict.lookup cd i1 in
                begin match jo with
                  | None -> 0
                  | Some j ->
                    if is_equal (Int.compare j1 j) then
                      -1
                    else if is_equal (Int.compare j2 j) then
                      1
                    else
                      0
                end
              else
                c
            in
            let lij_list_sorted_l =
              List.sort
                ~cmp:(lens_sort_compare_by_createdict cdl)
                lij_list_l
            in
            let lij_list_sorted_r =
              List.sort
                ~cmp:(lens_sort_compare_by_createdict cdr)
                lij_list_r
            in
            let l_list_l = List.map ~f:trd3 lij_list_sorted_l in
            let l_list_r = List.map ~f:trd3 lij_list_sorted_r in
            let l_l =
              fold_on_head_exn
                ~f:Lens.make_or
                l_list_l
            in
            let l_m =
              fold_on_head_exn
                ~f:Lens.make_or
                l_list_m
            in
            let l_r =
              fold_on_head_exn
                ~f:Lens.make_or
                l_list_r
            in
            Lens.make_compose
              (Lens.make_compose l_l l_m)
              l_r
        | Alignment.Nonempty.Star (_,_,a) ->
          Lens.make_star (nonempty_alignment_to_lens a)
        | Alignment.Nonempty.Times (tll,tlr,aligns,projl,projr) ->
          let projl = List.sort ~cmp:Int.compare projl in
          let projr = List.sort ~cmp:Int.compare projr in
          let rec combine_scct_and_sub_lenses
              (sub_lenses:Lens.t list)
              (scct:Permutation.swap_concat_compose_tree)
            : (Lens.t * Lens.t list) =
            begin match scct with
              | SCCTSwap (s1,s2) ->
                let (l1,remaining_left) =
                  combine_scct_and_sub_lenses
                    sub_lenses
                    s1 in
                let (l2,remaining_total) =
                  combine_scct_and_sub_lenses
                    remaining_left
                    s2 in
                (Lens.Swap(l1,l2),remaining_total)
              | SCCTConcat (s1,s2) ->
                let (l1,remaining_left) =
                  combine_scct_and_sub_lenses
                    sub_lenses
                    s1 in
                let (l2,remaining_total) =
                  combine_scct_and_sub_lenses
                    remaining_left
                    s2 in
                (Lens.Concat(l1,l2),remaining_total)
              | SCCTCompose _ ->
                failwith "compose is too ugly, should have failed faster"
              (*let s2size = size_scct s2 in
                let identity_copies = duplicate (LensIdentity (RegExBase "TODO")) s2size in
                let (l1,_) =
                combine_scct_and_sub_lenses
                  identity_copies
                  s1 in
                let (l2,remaining_total) =
                combine_scct_and_sub_lenses
                  sub_lenses
                  s2 in
                (LensCompose(l1,l2),remaining_total)*)
              | SCCTLeaf -> split_by_first_exn sub_lenses
            end
          in
          let left_strings = TD.get_strings tll in
          let right_strings = TD.get_strings tlr in
          let left_atoms = TD.get_atoms tll in
          let right_atoms = TD.get_atoms tlr in
          let last_index_left = List.length left_atoms in
          let last_index_right = List.length right_atoms in
          let (left_string_h,left_strings_t) =
            split_by_first_exn
              left_strings
          in
          let (right_string_h,right_strings_t) =
            split_by_first_exn
              right_strings
          in
          let left_projections =
            List.mapi
              ~f:(fun right_offset index ->
                  let rx_left = List.nth_exn left_atoms index in
                  ((index
                   ,last_index_right+right_offset)
                  ,Lens.make_disconnect
                      rx_left
                      Regex.one
                      (Regex.representative_exn rx_left)
                      ""))
              projl
          in
          let right_projections =
            List.mapi
              ~f:(fun left_offset index ->
                  let rx_right = List.nth_exn right_atoms index in
                  ((last_index_left+left_offset
                   ,index)
                  ,Lens.make_disconnect
                      (Regex.one)
                      rx_right
                      ""
                      (Regex.representative_exn rx_right)))
              projr
          in
          let left_strings_t_full =
            left_strings_t
            @ (List.init
                 (List.length right_projections)
                 (fun _ -> ""))
          in
          let right_strings_t_full =
            right_strings_t
            @ (List.init
                 (List.length left_projections)
                 (fun _ -> ""))
          in
          let aligns =
            List.map
              ~f:(fun (i,j,a) -> ((i,j),nonempty_alignment_to_lens a))
              aligns
          in
          let all_aligns =
            left_projections
            @ right_projections
            @ aligns
          in
          let all_aligns_ordered =
            List.sort
              ~cmp:(fun ((i1,_),_) ((i2,_),_) -> Int.compare i1 i2)
              all_aligns
          in
          let (index_pairs,alignments) =
            List.unzip
              all_aligns_ordered
          in
          let perm =
            Permutation.create_from_pairs
              index_pairs
          in
          let right_strings_t_invperm =
            Permutation.apply_inverse_to_list_exn
              perm
              right_strings_t_full
          in
          let string_lss_hd =
            Lens.make_disconnect
               (Regex.make_base left_string_h)
               (Regex.make_base right_string_h)
               left_string_h
               right_string_h
          in
          let string_tl_combos =
            List.zip_exn
              left_strings_t_full
              right_strings_t_invperm
          in
          let aligns_consts_zips =
            List.zip_exn
              alignments
              string_tl_combos
          in
          let atom_string_concats =
            List.map
              ~f:(fun (x,(s1,s2)) ->
                  Lens.make_times
                    x
                    (Lens.make_const s1 s2))
              aligns_consts_zips
          in
          begin match aligns_consts_zips with
            | [] -> string_lss_hd
            | _ ->
              let permutation_scct =
                Permutation.to_swap_concat_compose_tree
                  perm
              in
              if Permutation.has_compose permutation_scct then
                Lens.make_times
                  string_lss_hd
                  (Lens.make_permute perm atom_string_concats)
              else
                Lens.make_times
                  string_lss_hd
                  (fst (combine_scct_and_sub_lenses
                          atom_string_concats
                          permutation_scct))
          end
      end
    in
    begin match a with
      | Empty -> None
      | NonemptyTree nt -> Some (nonempty_alignment_to_lens nt)
    end
end
