open Stdlib
open Lang
open Normalized_lang
open Permutation


let rec to_dnf_regex (r:Regex.t) : dnf_regex =
  let atom_to_dnf_regex (a:atom) : dnf_regex =
    [([a],["";""])]
  in
  begin match r with
  | Regex.RegExEmpty -> []
  | Regex.RegExBase c -> [([],[c])]
  | Regex.RegExConcat (r1,r2) ->
      cartesian_map
        ~f:(fun (a1s,s1s) (a2s,s2s) -> (a1s@a2s,weld_lists (^) s1s s2s))
        (to_dnf_regex r1)
        (to_dnf_regex r2)
  | Regex.RegExOr (r1, r2) -> (to_dnf_regex r1) @ (to_dnf_regex r2)
  | Regex.RegExStar (r') -> atom_to_dnf_regex (AStar (to_dnf_regex r'))
  | Regex.RegExVariable s -> atom_to_dnf_regex (AVar s)
  end

let rec atom_lens_to_lens (a:atom_lens) : Lens.t =
  begin match a with
    | AtomLensIterate d -> Lens.LensIterate (dnf_lens_to_lens d)
    | AtomLensVariable l -> l
  end

and clause_lens_to_lens ((atoms,permutation,strings1,strings2):clause_lens)
  : Lens.t =
    let rec combine_scct_and_atom_lenses
            (atom_lenses:Lens.t list)
            (scct:swap_concat_compose_tree)
            : (Lens.t * Lens.t list) =
      begin match scct with
      | SCCTSwap (s1,s2) ->
          let (l1,remaining_left) =
            combine_scct_and_atom_lenses
              atom_lenses
              s1 in
          let (l2,remaining_total) =
            combine_scct_and_atom_lenses
              remaining_left
              s2 in
          (Lens.LensSwap(l1,l2),remaining_total)
      | SCCTConcat (s1,s2) ->
          let (l1,remaining_left) =
            combine_scct_and_atom_lenses
              atom_lenses
              s1 in
          let (l2,remaining_total) =
            combine_scct_and_atom_lenses
              remaining_left
              s2 in
          (Lens.LensConcat(l1,l2),remaining_total)
      | SCCTCompose _ ->
        failwith "compose is too ugly, should have failed faster"
          (*let s2size = size_scct s2 in
          let identity_copies = duplicate (LensIdentity (RegExBase "TODO")) s2size in
          let (l1,_) =
            combine_scct_and_atom_lenses
              identity_copies
              s1 in
          let (l2,remaining_total) =
            combine_scct_and_atom_lenses
              atom_lenses
              s2 in
            (LensCompose(l1,l2),remaining_total)*)
      | SCCTLeaf -> split_by_first_exn atom_lenses
      end
    in
    let (string1h,string1t) = split_by_first_exn strings1 in
    let (string2h,string2t) = split_by_first_exn strings2 in
    let (string2t_invperm) =
      Permutation.apply_inverse_to_list_exn
        permutation
        string2t
    in
    let string_lss_hd = Lens.LensConst (string1h, string2h) in
    let string_tl_combos = List.zip_exn string1t string2t_invperm in
    let string_lss_tl =
      List.map
        ~f:(fun (x,y) -> Lens.LensConst (x,y))
        string_tl_combos
    in
    let atom_lenses =
      List.map ~f:atom_lens_to_lens atoms in
    let atom_string_zips = List.zip_exn atom_lenses string_lss_tl in
    let atom_string_concats =
      List.map ~f:(fun (x,y) -> Lens.LensConcat (x,y)) atom_string_zips in
    begin match atom_string_concats with
    | [] -> string_lss_hd
    | _ ->
      let permutation_scct =
        Permutation.to_swap_concat_compose_tree permutation in
      if has_compose permutation_scct then
        Lens.LensConcat(string_lss_hd,
                   Lens.LensPermute (permutation,atom_string_concats))
      else
        Lens.LensConcat(string_lss_hd,
                   (fst (combine_scct_and_atom_lenses
                           atom_string_concats
                           permutation_scct)))
    end

and dnf_lens_to_lens ((clauses,_):dnf_lens) : Lens.t =
  let clause_lenses = List.map ~f:clause_lens_to_lens clauses in
  List.fold_left
    ~f:(fun acc l -> Lens.LensUnion (acc, l))
    ~init:(Lens.LensIdentity (Regex.RegExEmpty))
    clause_lenses
