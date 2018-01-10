open Stdlib
open Lang
open Lenscontext

let rec type_lens (lc:LensContext.t) (l:Lens.t) : Regex.t * Regex.t =
  begin match l with
    | Lens.LensConst(s1,s2) -> (Regex.RegExBase s1, Regex.RegExBase s2)
    | Lens.LensConcat(l1,l2) ->
      let (r1,s1) = type_lens lc l1 in
      let (r2,s2) = type_lens lc l2 in
      (Regex.RegExConcat (r1,r2), Regex.RegExConcat (s1,s2))
    | Lens.LensSwap(l1,l2) ->
      let (r1,s1) = type_lens lc l1 in
      let (r2,s2) = type_lens lc l2 in
      (Regex.RegExConcat (r1,r2), Regex.RegExConcat (s2,s1))
    | Lens.LensUnion(l1,l2) ->
      let (r1,s1) = type_lens lc l1 in
      let (r2,s2) = type_lens lc l2 in
      (Regex.RegExOr (r1,r2), Regex.RegExOr (s1,s2))
    | Lens.LensCompose(l1,l2) ->
      let (_,s2) = type_lens lc l1 in
      let (r1,_) = type_lens lc l2 in
      (* TODO, check r2 = s1 *)
      (r1,s2)
    | Lens.LensIterate (l') ->
      let (r',s') = type_lens lc l' in
      (Regex.RegExStar r', Regex.RegExStar s')
    | Lens.LensIdentity r ->
      (r,r)
    | Lens.LensInverse l' ->
      let (r,s) = type_lens lc l' in
      (s,r)
    | Lens.LensVariable n ->
      LensContext.lookup_type_exn lc n
    | Lens.LensPermute (p,ls) ->
      let rdl = List.map ~f:(type_lens lc) ls in
      let (r1s,r2s) = List.unzip rdl in
      let r1 =
        fold_on_head_with_default
          (fun r1 r2 -> Regex.RegExConcat (r1,r2))
          (Regex.RegExBase "")
          r1s
      in
      let r2s_permed = Permutation.apply_to_list_exn p r2s in
      let r2 =
        fold_on_head_with_default
          (fun r1 r2 -> Regex.RegExConcat (r1,r2))
          (Regex.RegExBase "")
          r2s_permed
      in
      (r1,r2)
  end
