open Stdlib
open Lang
open Regexcontext
open Regex_utilities
open Lenscontext

let retrieve_transitive_referenced_lenses
    (lc:LensContext.t)
    (l:Lens.t)
  : Lens.t list =
  let rec retrieve_transitive_referenced_lenses_internal
      (l:Lens.t)
    : Lens.t list =
    begin match l with
      | Lens.LensConst _ -> []
      | Lens.LensConcat (l1,l2) ->
        (retrieve_transitive_referenced_lenses_internal l1)
        @ (retrieve_transitive_referenced_lenses_internal l2)
      | Lens.LensSwap (l1,l2) ->
        (retrieve_transitive_referenced_lenses_internal l1)
        @ (retrieve_transitive_referenced_lenses_internal l2)
      | Lens.LensUnion (l1,l2) ->
        (retrieve_transitive_referenced_lenses_internal l1)
        @ (retrieve_transitive_referenced_lenses_internal l2)
      | Lens.LensCompose (l1,l2) ->
        (retrieve_transitive_referenced_lenses_internal l1)
        @ (retrieve_transitive_referenced_lenses_internal l2)
      | Lens.LensIterate l' ->
        retrieve_transitive_referenced_lenses_internal l'
      | Lens.LensIdentity _ -> []
      | Lens.LensInverse l' ->
        retrieve_transitive_referenced_lenses_internal l'
      | Lens.LensVariable v ->
        let l' = LensContext.lookup_impl_exn lc v in
        l'::
        (retrieve_transitive_referenced_lenses_internal
           l')
      | Lens.LensPermute (_,ll) ->
        List.concat_map
          ~f:(retrieve_transitive_referenced_lenses_internal)
          ll
    end
  in
  l::
  (retrieve_transitive_referenced_lenses_internal l)

let rec apply_at_every_level_lens (f:Lens.t -> Lens.t) (l:Lens.t) : Lens.t =
  let l =
    begin match l with
      | Lens.LensConcat (l1,l2) ->
        Lens.LensConcat (apply_at_every_level_lens f l1, apply_at_every_level_lens f l2)
      | Lens.LensSwap (l1,l2) ->
        Lens.LensSwap (apply_at_every_level_lens f l1, apply_at_every_level_lens f l2)
      | Lens.LensUnion (l1,l2) ->
        Lens.LensUnion (apply_at_every_level_lens f l1, apply_at_every_level_lens f l2)
      | Lens.LensCompose (l1,l2) ->
        Lens.LensCompose (apply_at_every_level_lens f l1, apply_at_every_level_lens f l2)
      | Lens.LensIterate (l') ->
        Lens.LensIterate (apply_at_every_level_lens f l')
      | Lens.LensInverse (l') ->
        Lens.LensInverse (apply_at_every_level_lens f l')
      | _ -> l
    end
  in
  f l

let rec make_lens_safe_in_smaller_context
    (rc_smaller:RegexContext.t)
    (rc_larger:RegexContext.t)
    (l:Lens.t)
  : Lens.t =
  begin match l with
    | Lens.LensConst _ -> l
    | Lens.LensPermute (p,ls) ->
      let ls =
        List.map
          ~f:(make_lens_safe_in_smaller_context rc_smaller rc_larger)
          ls
      in
      Lens.LensPermute (p,ls)
    | Lens.LensConcat (l1,l2) ->
      let l1 = make_lens_safe_in_smaller_context rc_smaller rc_larger l1 in
      let l2 = make_lens_safe_in_smaller_context rc_smaller rc_larger l2 in
      Lens.LensConcat (l1,l2)
    | Lens.LensSwap (l1,l2) ->
      let l1 = make_lens_safe_in_smaller_context rc_smaller rc_larger l1 in
      let l2 = make_lens_safe_in_smaller_context rc_smaller rc_larger l2 in
      Lens.LensSwap (l1,l2)
    | Lens.LensUnion (l1,l2) ->
      let l1 = make_lens_safe_in_smaller_context rc_smaller rc_larger l1 in
      let l2 = make_lens_safe_in_smaller_context rc_smaller rc_larger l2 in
      Lens.LensUnion (l1,l2)
    | Lens.LensCompose (l1,l2) ->
      let l1 = make_lens_safe_in_smaller_context rc_smaller rc_larger l1 in
      let l2 = make_lens_safe_in_smaller_context rc_smaller rc_larger l2 in
      Lens.LensCompose (l1,l2)
    | Lens.LensIterate l' ->
      let l' = make_lens_safe_in_smaller_context rc_smaller rc_larger l' in
      Lens.LensIterate l'
    | Lens.LensIdentity r ->
      Lens.LensIdentity (make_regex_safe_in_smaller_context rc_smaller rc_larger r)
    | Lens.LensInverse l' ->
      let l' = make_lens_safe_in_smaller_context rc_smaller rc_larger l' in
      Lens.LensInverse l'
    | Lens.LensVariable _ -> l
  end

let distribute_inverses : Lens.t -> Lens.t =
  let distribute_inverses_current_level (l:Lens.t) : Lens.t =
    begin match l with
      | Lens.LensInverse l' ->
        begin match l' with
          | Lens.LensConst (s1,s2) ->
            Lens.LensConst(s2,s1)
          | Lens.LensConcat (l1',l2') ->
            Lens.LensConcat (Lens.LensInverse l1', Lens.LensInverse l2')
          | Lens.LensSwap (l1',l2') ->
            Lens.LensSwap (Lens.LensInverse l2', Lens.LensInverse l1')
          | Lens.LensUnion (l1',l2') ->
            Lens.LensUnion (Lens.LensInverse l1', Lens.LensInverse l2')
          | Lens.LensCompose (l1',l2') ->
            Lens.LensCompose (Lens.LensInverse l2', Lens.LensInverse l1')
          | Lens.LensIterate l'' ->
            Lens.LensIterate (Lens.LensInverse l'')
          | Lens.LensIdentity r ->
            Lens.LensIdentity r
          | Lens.LensInverse l'' ->
            l''
          | Lens.LensVariable _ ->
            l
          | Lens.LensPermute (p,ls) ->
            Lens.LensPermute (Permutation.inverse p, ls)
        end
      | _ -> l
    end
  in
  apply_at_every_level_lens distribute_inverses_current_level


let simplify_lens : Lens.t -> Lens.t =
  let maximally_factor_lens : Lens.t -> Lens.t =
    Semiring.maximally_factor_element
      lens_semiring
  in
  let distribute_identities (l:Lens.t) : Lens.t =
    let merge_concated_identities : Lens.t -> Lens.t =
      let rec retrieve_rightmost_identity
          (l:Lens.t)
        : (Lens.t option * Regex.t option) =
        begin match l with
          | Lens.LensConcat (l1,l2) ->
            begin match retrieve_rightmost_identity l2 with
              | (None, ro) -> (Some l1, ro)
              | (Some l2, ro) -> (Some (Lens.LensConcat (l1,l2)),ro)
            end
          | Lens.LensIdentity r -> (None, Some r)
          | _ -> (Some l, None)
        end
      in
      let rec try_insert_into_leftmost_identity
          (l:Lens.t)
          (r1:Regex.t)
        : Lens.t option =
        begin match l with
          | Lens.LensConcat (l1,l2) ->
            Option.map
              ~f:(fun l1 -> Lens.LensConcat (l1,l2))
              (try_insert_into_leftmost_identity l1 r1)
          | Lens.LensIdentity r2 ->
            Some (Lens.LensIdentity (Regex.RegExConcat (r1,r2)))
          | _ -> None
        end
      in
      let merge_concated_identities_current_level
          (l:Lens.t)
        : Lens.t =
        begin match l with
          | Lens.LensConcat (l1,l2) ->
            begin match retrieve_rightmost_identity l1 with
              | (l1o,Some r1) ->
                begin match try_insert_into_leftmost_identity l2 r1 with
                  | None -> l
                  | Some l2 ->
                    begin match l1o with
                      | None -> l2
                      | Some l1 -> Lens.LensConcat (l1,l2)
                    end
                end
              | (_, None) -> l
            end
          | _ -> l
        end
      in
      apply_at_every_level_lens merge_concated_identities_current_level
    in

    let merge_ored_identities
        (l:Lens.t)
      : Lens.t =
      let or_lens_with_identity
          (lo:Lens.t option)
          (ro:Regex.t option)
        : Lens.t =
        begin match (lo,ro) with
          | (None,None) -> failwith "badly implemented merge"
          | (None, Some r) -> Lens.LensIdentity r
          | (Some l, None) -> l
          | (Some l, Some r) ->
            Lens.LensUnion (l, Lens.LensIdentity r)
        end
      in
      let or_regex_options
          (r1o:Regex.t option)
          (r2o:Regex.t option)
        : Regex.t option =
        begin match (r1o,r2o) with
          | (None, None) -> None
          | (Some r1, None) -> Some r1
          | (None, Some r2) -> Some r2
          | (Some r1, Some r2) -> Some (Regex.RegExOr (r1,r2))
        end
      in
      let or_lens_options
          (l1o:Lens.t option)
          (l2o:Lens.t option)
        : Lens.t option =
        begin match (l1o,l2o) with
          | (None, None) -> None
          | (Some l1, None) -> Some l1
          | (None, Some l2) -> Some l2
          | (Some l1, Some l2) -> Some (Lens.LensUnion (l1,l2))
        end
      in
      let rec merge_ored_identities_internal
          (l:Lens.t)
        : ((Lens.t option) * (Regex.t option)) =
        let merge_ored_beneath (l:Lens.t) : Lens.t =
          let (lo,ro) = merge_ored_identities_internal l in
          or_lens_with_identity lo ro
        in
        begin match l with
          | Lens.LensConst _ -> (Some l, None)
          | Lens.LensConcat (l1,l2) ->
            let l1 = merge_ored_beneath l1 in
            let l2 = merge_ored_beneath l2 in
            (Some (Lens.LensConcat (l1,l2)), None)
          | Lens.LensSwap (l1,l2) ->
            let l1 = merge_ored_beneath l1 in
            let l2 = merge_ored_beneath l2 in
            (Some (Lens.LensSwap (l1,l2)), None)
          | Lens.LensUnion (l1,l2) ->
            let (l1o,r1o) = merge_ored_identities_internal l1 in
            let (l2o,r2o) = merge_ored_identities_internal l2 in
            (or_lens_options l1o l2o, or_regex_options r1o r2o)
          | Lens.LensCompose (l1,l2) ->
            let l1 = merge_ored_beneath l1 in
            let l2 = merge_ored_beneath l2 in
            (Some (Lens.LensCompose (l1,l2)), None)
          | Lens.LensIterate l' ->
            let l' = merge_ored_beneath l' in
            (Some (Lens.LensIterate l'), None)
          | Lens.LensIdentity r ->
            (None, if r = Regex.RegExEmpty then None else Some r)
          | Lens.LensInverse l' ->
            let l' = merge_ored_beneath l' in
            (Some (Lens.LensInverse l'), None)
          | Lens.LensVariable _ -> (Some l, None)
          | Lens.LensPermute (p,ls) ->
            let ls = List.map ~f:merge_ored_beneath ls in
            (Some (Lens.LensPermute (p,ls)), None)
        end
      in
      let (lo,ro) = merge_ored_identities_internal l in
      or_lens_with_identity lo ro
    in

    let distribute_iteration : Lens.t -> Lens.t =
      let distribute_iteration_single_level (l:Lens.t) : Lens.t =
        begin match l with
          | Lens.LensIterate (Lens.LensIdentity r) -> Lens.LensIdentity (Regex.RegExStar r)
          | _ -> l
        end
      in
      apply_at_every_level_lens distribute_iteration_single_level
    in


    l
    |> merge_ored_identities
    |> merge_concated_identities
    |> distribute_iteration
  in

  let clean_identities : Lens.t -> Lens.t =
    let clean_identities_single_level (l:Lens.t) : Lens.t =
      begin match l with
        | Lens.LensIdentity r -> Lens.LensIdentity (simplify_regex r)
        | _ -> l
      end
    in
    apply_at_every_level_lens clean_identities_single_level
  in

  let identify_identity_consts : Lens.t -> Lens.t =
    let identify_identity_consts_current_level (l:Lens.t) : Lens.t =
      begin match l with
        | Lens.LensConst(s1,s2) ->
          if s1 = s2 then
            Lens.LensIdentity (Regex.RegExBase s1)
          else
            l
        | _ -> l
      end
    in
    apply_at_every_level_lens identify_identity_consts_current_level
  in

  let remove_identity_identities : Lens.t -> Lens.t =
    let remove_identity_identities_current_level (l:Lens.t) : Lens.t =
      begin match l with
        | Lens.LensConcat(Lens.LensIdentity (Regex.RegExBase ""), l) -> l
        | Lens.LensConcat(l, Lens.LensIdentity (Regex.RegExBase "")) -> l
        | Lens.LensSwap (Lens.LensIdentity (Regex.RegExBase ""), l) -> l
        | Lens.LensSwap (l, Lens.LensIdentity (Regex.RegExBase "")) -> l
        | Lens.LensUnion (l, Lens.LensIdentity (Regex.RegExEmpty))   -> l
        | Lens.LensUnion (Lens.LensIdentity (Regex.RegExEmpty), l)   -> l
        | _ -> l
      end
    in
    apply_at_every_level_lens remove_identity_identities_current_level
  in

  let split_consts_into_concats_leftfirst : Lens.t -> Lens.t =
    let split_consts_into_concats_leftfirst_current_level (l:Lens.t) : Lens.t =
      begin match l with
        | Lens.LensConst(s1,s2) -> Lens.LensConcat(Lens.LensConst(s1,""),Lens.LensConst("",s2))
        | _ -> l
      end
    in
    apply_at_every_level_lens split_consts_into_concats_leftfirst_current_level
  in

  let split_consts_into_concats_rightfirst : Lens.t -> Lens.t =
    let split_consts_into_concats_rightfirst_current_level (l:Lens.t) : Lens.t =
      begin match l with
        | Lens.LensConst(s1,s2) -> Lens.LensConcat(Lens.LensConst(s1,""),Lens.LensConst("",s2))
        | _ -> l
      end
    in
    apply_at_every_level_lens split_consts_into_concats_rightfirst_current_level
  in

  let separate_emptystring_consts : Lens.t -> Lens.t =
    let string_to_singlecharstring_list (s:string) : string list =
      let cl = string_to_char_list s in
      List.map ~f:Char.to_string cl
    in
    let separate_emptystring_consts_current_level (l:Lens.t) : Lens.t =
      begin match l with
        | Lens.LensConst("","") -> l
        | Lens.LensConst("",s2) ->
          let sl = string_to_singlecharstring_list s2 in
          let (fs,ls) = split_by_last_exn sl in
          List.fold_right
            ~f:(fun s acc ->
                Lens.LensConcat(Lens.LensConst("",s),acc))
            ~init:(Lens.LensConst("",ls))
            fs
        | Lens.LensConst(s1,"") ->
          let sl = string_to_singlecharstring_list s1 in
          let (fs,ls) = split_by_last_exn sl in
          List.fold_right
            ~f:(fun s acc ->
                Lens.LensConcat(Lens.LensConst(s,""),acc))
            ~init:(Lens.LensConst(ls,""))
            fs
        | _ -> l
      end
    in

    apply_at_every_level_lens separate_emptystring_consts_current_level
  in

  let merge_concated_consts : Lens.t -> Lens.t =
    let rec retrieve_rightmost_const
        (l:Lens.t)
      : (Lens.t option * ((string*string) option)) =
      begin match l with
        | Lens.LensConcat (l1,l2) ->
          begin match retrieve_rightmost_const l2 with
            | (None, sso) -> (Some l1, sso)
            | (Some l2, sso) -> (Some (Lens.LensConcat (l1,l2)),sso)
          end
        | Lens.LensConst(s1,s2) -> (None, Some (s1,s2))
        | _ -> (Some l, None)
      end
    in
    let rec try_insert_into_leftmost_const
        (l:Lens.t)
        (s1:string)
        (s2:string)
      : Lens.t option =
      begin match l with
        | Lens.LensConcat (l1,l2) ->
          Option.map
            ~f:(fun l1 -> Lens.LensConcat (l1,l2))
            (try_insert_into_leftmost_const l1 s1 s2)
        | Lens.LensConst (t1,t2) ->
          Some (Lens.LensConst (s1^t1,s2^t2))
        | _ -> None
      end
    in
    let merge_concated_consts_current_level
        (l:Lens.t)
      : Lens.t =
      begin match l with
        | Lens.LensConcat (l1,l2) ->
          begin match retrieve_rightmost_const l1 with
            | (l1o,Some (s1,s2)) ->
              begin match try_insert_into_leftmost_const l2 s1 s2 with
                | None -> l
                | Some l2 ->
                  begin match l1o with
                    | None -> l2
                    | Some l1 -> Lens.LensConcat (l1,l2)
                  end
              end
            | (_, None) -> l
          end
        | _ -> l
      end
    in
    apply_at_every_level_lens merge_concated_consts_current_level
  in

  let perform_cleanups =
    distribute_inverses
    % identify_identity_consts
    % merge_concated_consts
    % remove_identity_identities
    % clean_identities
    % distribute_identities
    % maximally_factor_lens
    % separate_emptystring_consts
  in
  
  fold_until_fixpoint
    (perform_cleanups
     % split_consts_into_concats_rightfirst
     % perform_cleanups
     % split_consts_into_concats_leftfirst)
