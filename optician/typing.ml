open Stdlib
open Lang
open Lenscontext

let rec type_lens
    (l:Lens.t)
  : Regex.t * Regex.t =
  begin match l with
    | Lens.Disconnect(r1,r2,s1,s2) -> (r1, r2)
    | Lens.Concat(l1,l2) ->
      let (r1,s1) = type_lens l1 in
      let (r2,s2) = type_lens l2 in
      (Regex.RegExConcat (r1,r2), Regex.RegExConcat (s1,s2))
    | Lens.Swap(l1,l2) ->
      let (r1,s1) = type_lens l1 in
      let (r2,s2) = type_lens l2 in
      (Regex.RegExConcat (r1,r2), Regex.RegExConcat (s2,s1))
    | Lens.Union(l1,l2) ->
      let (r1,s1) = type_lens l1 in
      let (r2,s2) = type_lens l2 in
      (Regex.RegExOr (r1,r2), Regex.RegExOr (s1,s2))
    | Lens.Compose(l1,l2) ->
      let (_,s2) = type_lens l1 in
      let (r1,_) = type_lens l2 in
      (* TODO, check r2 = s1 *)
      (r1,s2)
    | Lens.Iterate (l') ->
      let (r',s') = type_lens l' in
      (Regex.RegExStar r', Regex.RegExStar s')
    | Lens.Identity r ->
      (r,r)
    | Lens.Inverse l' ->
      let (r,s) = type_lens l' in
      (s,r)
    | Lens.Closed (_,l) ->
      Lens.get_left_right_regex_closed l
    | Lens.Permute (p,ls) ->
      let rdl = List.map ~f:(type_lens) ls in
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
