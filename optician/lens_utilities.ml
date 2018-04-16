open MyStdlib
open Lang
open Regex_utilities
open Lenscontext

(*let retrieve_transitive_referenced_lenses
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
  (retrieve_transitive_referenced_lenses_internal l)*)

let rec apply_at_every_level_lens (f:Lens.t -> Lens.t) (l:Lens.t) : Lens.t =
  let l =
    begin match l with
      | Lens.Concat (l1,l2) ->
        Lens.Concat (apply_at_every_level_lens f l1, apply_at_every_level_lens f l2)
      | Lens.Swap (l1,l2) ->
        Lens.Swap (apply_at_every_level_lens f l1, apply_at_every_level_lens f l2)
      | Lens.Union (l1,l2) ->
        Lens.Union (apply_at_every_level_lens f l1, apply_at_every_level_lens f l2)
      | Lens.Compose (l1,l2) ->
        Lens.Compose (apply_at_every_level_lens f l1, apply_at_every_level_lens f l2)
      | Lens.Iterate (l') ->
        Lens.Iterate (apply_at_every_level_lens f l')
      | Lens.Inverse (l') ->
        Lens.Inverse (apply_at_every_level_lens f l')
      | _ -> l
    end
  in
  f l

(*let rec make_lens_safe_in_smaller_context
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
    | Lens.LensClosed l' ->
      let l' = make_lens_safe_in_smaller_context rc_smaller rc_larger l' in
      Lens.LensClosed l'
  end*)

let distribute_inverses : Lens.t -> Lens.t =
  let distribute_inverses_current_level (l:Lens.t) : Lens.t =
    begin match l with
      | Lens.Inverse l' ->
        begin match l' with
          | Lens.Disconnect (r1,r2,s1,s2) ->
            Lens.Disconnect (r2,r1,s2,s1)
          | Lens.Concat (l1',l2') ->
            Lens.Concat (Lens.Inverse l1', Lens.Inverse l2')
          | Lens.Swap (l1',l2') ->
            Lens.Swap (Lens.Inverse l2', Lens.Inverse l1')
          | Lens.Union (l1',l2') ->
            Lens.Union (Lens.Inverse l1', Lens.Inverse l2')
          | Lens.Compose (l1',l2') ->
            Lens.Compose (Lens.Inverse l2', Lens.Inverse l1')
          | Lens.Iterate l'' ->
            Lens.Iterate (Lens.Inverse l'')
          | Lens.Identity r ->
            Lens.Identity r
          | Lens.Inverse l'' ->
            l''
          | Lens.Closed _ ->
            l
          | Lens.Permute (p,ls) ->
            Lens.Permute (Permutation.inverse p, ls)
        end
      | _ -> l
    end
  in
  apply_at_every_level_lens distribute_inverses_current_level


let simplify_lens : Lens.t -> Lens.t =
  let maximally_factor_lens : Lens.t -> Lens.t =
    Semiring.maximally_factor_element
      ~is_eq:(Lens.is_eq)
      lens_semiring
  in
  let distribute_identities (l:Lens.t) : Lens.t =
    let merge_concated_identities : Lens.t -> Lens.t =
      let rec retrieve_rightmost_identity
          (l:Lens.t)
        : (Lens.t option * Regex.t option) =
        begin match l with
          | Lens.Concat (l1,l2) ->
            begin match retrieve_rightmost_identity l2 with
              | (None, ro) -> (Some l1, ro)
              | (Some l2, ro) -> (Some (Lens.Concat (l1,l2)),ro)
            end
          | Lens.Identity r -> (None, Some r)
          | _ -> (Some l, None)
        end
      in
      let rec try_insert_into_leftmost_identity
          (l:Lens.t)
          (r1:Regex.t)
        : Lens.t option =
        begin match l with
          | Lens.Concat (l1,l2) ->
            Option.map
              ~f:(fun l1 -> Lens.Concat (l1,l2))
              (try_insert_into_leftmost_identity l1 r1)
          | Lens.Identity r2 ->
            Some (Lens.Identity (Regex.make_concat r1 r2))
          | _ -> None
        end
      in
      let merge_concated_identities_current_level
          (l:Lens.t)
        : Lens.t =
        begin match l with
          | Lens.Concat (l1,l2) ->
            begin match retrieve_rightmost_identity l1 with
              | (l1o,Some r1) ->
                begin match try_insert_into_leftmost_identity l2 r1 with
                  | None -> l
                  | Some l2 ->
                    begin match l1o with
                      | None -> l2
                      | Some l1 -> Lens.Concat (l1,l2)
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
          | (None, Some r) -> Lens.Identity r
          | (Some l, None) -> l
          | (Some l, Some r) ->
            Lens.Union (l, Lens.Identity r)
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
          | (Some r1, Some r2) -> Some (Regex.make_or r1 r2)
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
          | (Some l1, Some l2) -> Some (Lens.Union (l1,l2))
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
          | Lens.Disconnect _ -> (Some l, None)
          | Lens.Concat (l1,l2) ->
            let l1 = merge_ored_beneath l1 in
            let l2 = merge_ored_beneath l2 in
            (Some (Lens.Concat (l1,l2)), None)
          | Lens.Swap (l1,l2) ->
            let l1 = merge_ored_beneath l1 in
            let l2 = merge_ored_beneath l2 in
            (Some (Lens.Swap (l1,l2)), None)
          | Lens.Union (l1,l2) ->
            let (l1o,r1o) = merge_ored_identities_internal l1 in
            let (l2o,r2o) = merge_ored_identities_internal l2 in
            (or_lens_options l1o l2o, or_regex_options r1o r2o)
          | Lens.Compose (l1,l2) ->
            let l1 = merge_ored_beneath l1 in
            let l2 = merge_ored_beneath l2 in
            (Some (Lens.Compose (l1,l2)), None)
          | Lens.Iterate l' ->
            let l' = merge_ored_beneath l' in
            (Some (Lens.Iterate l'), None)
          | Lens.Identity r ->
            (None, if r = Regex.empty then None else Some r)
          | Lens.Inverse l' ->
            let l' = merge_ored_beneath l' in
            (Some (Lens.Inverse l'), None)
          | Lens.Closed _ -> (Some l, None)
          | Lens.Permute (p,ls) ->
            let ls = List.map ~f:merge_ored_beneath ls in
            (Some (Lens.Permute (p,ls)), None)
        end
      in
      let (lo,ro) = merge_ored_identities_internal l in
      or_lens_with_identity lo ro
    in

    let distribute_iteration : Lens.t -> Lens.t =
      let distribute_iteration_single_level (l:Lens.t) : Lens.t =
        begin match l with
          | Lens.Iterate (Lens.Identity r) -> Lens.Identity (Regex.make_star r)
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
        | Lens.Identity r -> Lens.Identity (simplify_regex r)
        | _ -> l
      end
    in
    apply_at_every_level_lens clean_identities_single_level
  in

  let identify_identity_consts : Lens.t -> Lens.t =
    let identify_identity_consts_current_level (l:Lens.t) : Lens.t =
      begin match l with
        | Lens.Disconnect(r1,r2,_,_) ->
          begin match (r1.node,r2.node) with
            | (Regex.RegExBase s1, Regex.RegExBase s2) ->
              if s1 = s2 then
                Lens.Identity (Regex.make_base s1)
              else
                l
            | _ -> l
          end
        | _ -> l
      end
    in
    apply_at_every_level_lens identify_identity_consts_current_level
  in

  let remove_identity_identities : Lens.t -> Lens.t =
    let remove_identity_identities_current_level (l:Lens.t) : Lens.t =
      begin match l with
        | Lens.Concat(Lens.Identity r, l') ->
          if is_equal (Regex.compare r Regex.one) then
            l'
          else
            l
        | Lens.Concat(l', Lens.Identity r) ->
          if is_equal (Regex.compare r Regex.one) then
            l'
          else
            l
        | Lens.Swap (Lens.Identity r, l') ->
          if is_equal (Regex.compare r Regex.one) then
            l'
          else
            l
        | Lens.Swap (l', Lens.Identity r) ->
          if is_equal (Regex.compare r Regex.one) then
            l'
          else
            l
        | Lens.Union (l', Lens.Identity r) ->
          if is_equal (Regex.compare r Regex.zero) then
            l'
          else
            l
        | Lens.Union (Lens.Identity r, l') ->
          if is_equal (Regex.compare r Regex.zero) then
            l'
          else
            l
        | _ -> l
      end
    in
    apply_at_every_level_lens remove_identity_identities_current_level
  in

  let split_consts_into_concats_leftfirst : Lens.t -> Lens.t =
    let split_consts_into_concats_leftfirst_current_level (l:Lens.t) : Lens.t =
      begin match l with
        | Lens.Disconnect(r1,r2,s1,s2) ->
          Lens.Concat(Lens.Disconnect(r1,Regex.make_base "",s1,""),Lens.Disconnect(Regex.make_base "",r2,"",s2))
        | _ -> l
      end
    in
    apply_at_every_level_lens split_consts_into_concats_leftfirst_current_level
  in

  let split_consts_into_concats_rightfirst : Lens.t -> Lens.t =
    let split_consts_into_concats_rightfirst_current_level (l:Lens.t) : Lens.t =
      begin match l with
        | Lens.Disconnect(r1,r2,s1,s2) ->
          Lens.Concat
            (Lens.Disconnect(r1,Regex.one,s1,"")
            ,Lens.Disconnect(Regex.one,r2,"",s2))
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
        | Lens.Disconnect(r1,r2,_,_) ->
          begin match (r1.node,r2.node) with
            | (Regex.RegExBase "", Regex.RegExBase "") -> l
            | (Regex.RegExBase "",Regex.RegExBase s2) ->
              let sl = string_to_singlecharstring_list s2 in
              let (fs,ls) = split_by_last_exn sl in
              List.fold_right
                ~f:(fun s acc ->
                    Lens.Concat(Lens.Disconnect(Regex.one,Regex.make_base s,"",s),acc))
                ~init:(Lens.Disconnect(Regex.one,Regex.make_base ls,"",ls))
                fs
            | (Regex.RegExBase s1,Regex.RegExBase "") ->
              let sl = string_to_singlecharstring_list s1 in
              let (fs,ls) = split_by_last_exn sl in
              List.fold_right
                ~f:(fun s acc ->
                    Lens.Concat(Lens.Disconnect(Regex.make_base s,Regex.one,s,""),acc))
                ~init:(Lens.Disconnect(Regex.make_base ls,Regex.one,ls,""))
                fs
            | _ -> l
          end
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
        | Lens.Concat (l1,l2) ->
          begin match retrieve_rightmost_const l2 with
            | (None, sso) -> (Some l1, sso)
            | (Some l2, sso) -> (Some (Lens.Concat (l1,l2)),sso)
          end
        | Lens.Disconnect(r1,r2,_,_) ->
          begin match (r1.node,r2.node) with
            | (Regex.RegExBase s1,Regex.RegExBase s2) ->
              (None, Some (s1,s2))
            | _ -> (Some l, None)
          end
        | _ -> (Some l, None)
      end
    in
    let rec try_insert_into_leftmost_const
        (l:Lens.t)
        (s1:string)
        (s2:string)
      : Lens.t option =
      begin match l with
        | Lens.Concat (l1,l2) ->
          Option.map
            ~f:(fun l1 -> Lens.Concat (l1,l2))
            (try_insert_into_leftmost_const l1 s1 s2)
        | Lens.Disconnect (r1,r2,_,_) ->
          begin match (r1.node,r2.node) with
            | (Regex.RegExBase t1,Regex.RegExBase t2) ->
              Some (Lens.Disconnect (Regex.make_base (s1^t1),Regex.make_base (s2^t2),s1^t1,s2^t2))
            | _ -> None
          end
        | _ -> None
      end
    in
    let merge_concated_consts_current_level
        (l:Lens.t)
      : Lens.t =
      begin match l with
        | Lens.Concat (l1,l2) ->
          begin match retrieve_rightmost_const l1 with
            | (l1o,Some (s1,s2)) ->
              begin match try_insert_into_leftmost_const l2 s1 s2 with
                | None -> l
                | Some l2 ->
                  begin match l1o with
                    | None -> l2
                    | Some l1 -> Lens.Concat (l1,l2)
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
    ~is_eq:Lens.is_eq
    (perform_cleanups
     % split_consts_into_concats_rightfirst
     % perform_cleanups
     % split_consts_into_concats_leftfirst)
