open MyStdlib
open Lang

let simplify_regex : Regex.t -> Regex.t =
  let maximally_factor_regex : Regex.t -> Regex.t =
    Semiring.maximally_factor_element
      ~is_eq:(comparer_to_equality_check Regex.compare)
      regex_semiring
  in
  let rec clean_regex (r:Regex.t) : Regex.t =
    begin match r.node with
      | Regex.RegExConcat(x,y) ->
        let x = clean_regex x in
        let y = clean_regex y in
        begin match (x.node,y.node) with
          | (Regex.RegExBase "",_) -> y
          | (_,Regex.RegExBase "") -> x
          | (Regex.RegExEmpty,_) -> Regex.empty
          | (_,Regex.RegExEmpty) -> Regex.empty
          | _ -> Regex.make_concat x y
        end
      | Regex.RegExOr(x,y) ->
        let x = clean_regex x in
        let y = clean_regex y in
        begin match (x.node,y.node) with
          | (Regex.RegExEmpty,_) -> y
          | (_,Regex.RegExEmpty) -> x
          | _ -> Regex.make_or x y
        end
      | Regex.RegExStar(x) ->
        let x = clean_regex x in
        begin match x.node with
          | Regex.RegExEmpty -> Regex.make_base ""
          | _ -> Regex.make_star x
        end
      | _ -> r
    end
  in

  let merge_concated_bases : Regex.t -> Regex.t =
    let rec retrieve_rightmost_concated_base
        (r:Regex.t)
      : (Regex.t option * string option) =
      begin match r.node with
        | Regex.RegExConcat (r1,r2) ->
          begin match retrieve_rightmost_concated_base r2 with
            | (None, so) -> (Some r1, so)
            | (Some r2, so) -> (Some (Regex.make_concat r1 r2),so)
          end
        | Regex.RegExBase s -> (None, Some s)
        | _ -> (Some r, None)
      end
    in
    let rec try_insert_into_leftmost_concated_base
        (r:Regex.t)
        (s1:string)
      : Regex.t option =
      begin match r.node with
        | Regex.RegExConcat (r1,r2) ->
          Option.map
            ~f:(fun r1 -> Regex.make_concat r1 r2)
            (try_insert_into_leftmost_concated_base r1 s1)
        | Regex.RegExBase s2 ->
          Some (Regex.make_base (s1^s2))
        | _ -> None
      end
    in
    let merge_concated_bases_current_level
        (r:Regex.t)
      : Regex.t =
      begin match r.node with
        | Regex.RegExConcat (r1,r2) ->
          begin match retrieve_rightmost_concated_base r1 with
            | (r1o,Some s1) ->
              begin match try_insert_into_leftmost_concated_base r2 s1 with
                | None -> r
                | Some r2 ->
                  begin match r1o with
                    | None -> r2
                    | Some r1 -> Regex.make_concat r1 r2
                  end
              end
            | (_, None) -> r
          end
        | _ -> r
      end
    in

    Regex.apply_at_every_level merge_concated_bases_current_level
  in

  fold_until_fixpoint
    ~is_eq:(comparer_to_equality_check Regex.compare)
    (merge_concated_bases
     % clean_regex
     % maximally_factor_regex)


let rec iteratively_deepen
    (r:Regex.t)
    (ss:string List.t)
  : Regex.t =
  begin match r.node with
    | Regex.RegExEmpty -> r
    | Regex.RegExBase s -> if List.mem ~equal:String.equal ss s then Regex.make_closed r else r
    | Regex.RegExConcat (r1,r2) ->
      let r1 = iteratively_deepen r1 ss in
      let r2 = iteratively_deepen r2 ss in
      Regex.make_closed (Regex.make_concat r1 r2)
    | Regex.RegExOr (r1,r2) ->
      let r1 = iteratively_deepen r1 ss in
      let r2 = iteratively_deepen r2 ss in
      Regex.make_closed (Regex.make_or r1 r2)
    | Regex.RegExStar r' ->
      let r' = iteratively_deepen r' ss in
      Regex.make_closed (Regex.make_star r')
    | Regex.RegExSkip r' ->
      let r' = iteratively_deepen r' ss in
      Regex.make_closed (Regex.make_skip r')
    | Regex.RegExClosed _ ->
      failwith "shouldn't happen"
    | Regex.RegExRequire r' ->
      let r' = iteratively_deepen r' ss in
      Regex.make_closed (Regex.make_require r')
  end

let get_dnf_size
    (r:Regex.t)
  : int =
  let rec get_dnf_size_internal
      (r:Regex.t)
    : int * int =
    begin match r.node with
      | Regex.RegExEmpty -> (0,0)
      | Regex.RegExBase _ -> (0,1)
      | Regex.RegExConcat (r1,r2) ->
        let (size1,or_size1) = get_dnf_size_internal r1 in
        let (size2,or_size2) = get_dnf_size_internal r2 in
        (size1*or_size2 + size2*or_size1, or_size1 * or_size2) 
      | Regex.RegExOr (r1,r2) ->
        let (size1,or_size1) = get_dnf_size_internal r1 in
        let (size2,or_size2) = get_dnf_size_internal r2 in
        (size1+size2,or_size1+or_size2)
      | Regex.RegExStar r' ->
        let (size,_) = get_dnf_size_internal r' in
        (size+1,1)
      | Regex.RegExClosed _ ->
        (1,1)
      | Regex.RegExSkip _ ->
        (1,1)
      | Regex.RegExRequire r ->
        get_dnf_size_internal r
    end
  in
  fst (get_dnf_size_internal r)

