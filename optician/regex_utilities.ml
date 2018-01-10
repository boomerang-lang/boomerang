open Stdlib
open Lang
open Regexcontext

let rec make_regex_safe_in_smaller_context
    (rc_smaller:RegexContext.t)
    (rc_larger:RegexContext.t)
  : Regex.t -> Regex.t =
  fold_until_fixpoint
    (Regex.fold
       ~empty_f:Regex.zero
       ~concat_f:Regex.make_times
       ~or_f:Regex.make_plus
       ~star_f:Regex.make_star
       ~base_f:Regex.make_base
       ~var_f:(fun v ->
           begin match (RegexContext.lookup rc_smaller v) with
             | None ->
               RegexContext.lookup_exn rc_larger v
             | Some _ -> Regex.make_var v
           end))

let simplify_regex : Regex.t -> Regex.t =
  let maximally_factor_regex : Regex.t -> Regex.t =
    Semiring.maximally_factor_element
      regex_semiring
  in
  let rec clean_regex (r:Regex.t) : Regex.t =
    begin match r with
      | Regex.RegExConcat(x,y) ->
        let x = clean_regex x in
        let y = clean_regex y in
        begin match (x,y) with
          | (Regex.RegExBase "",_) -> y
          | (_,Regex.RegExBase "") -> x
          | (Regex.RegExEmpty,_) -> Regex.RegExEmpty
          | (_,Regex.RegExEmpty) -> Regex.RegExEmpty
          | _ -> Regex.RegExConcat(x,y)
        end
      | Regex.RegExOr(x,y) ->
        let x = clean_regex x in
        let y = clean_regex y in
        begin match (x,y) with
          | (Regex.RegExEmpty,_) -> y
          | (_,Regex.RegExEmpty) -> x
          | _ -> Regex.RegExOr(x,y)
        end
      | Regex.RegExStar(x) ->
        let x = clean_regex x in
        begin match x with
          | Regex.RegExEmpty -> Regex.RegExBase ""
          | _ -> Regex.RegExStar x
        end
      | _ -> r
    end
  in

  let merge_concated_bases : Regex.t -> Regex.t =
    let rec retrieve_rightmost_concated_base
        (r:Regex.t)
      : (Regex.t option * string option) =
      begin match r with
        | Regex.RegExConcat (r1,r2) ->
          begin match retrieve_rightmost_concated_base r2 with
            | (None, so) -> (Some r1, so)
            | (Some r2, so) -> (Some (Regex.RegExConcat (r1,r2)),so)
          end
        | Regex.RegExBase s -> (None, Some s)
        | _ -> (Some r, None)
      end
    in
    let rec try_insert_into_leftmost_concated_base
        (r:Regex.t)
        (s1:string)
      : Regex.t option =
      begin match r with
        | Regex.RegExConcat (r1,r2) ->
          Option.map
            ~f:(fun r1 -> Regex.RegExConcat (r1,r2))
            (try_insert_into_leftmost_concated_base r1 s1)
        | Regex.RegExBase s2 ->
          Some (Regex.RegExBase (s1^s2))
        | _ -> None
      end
    in
    let merge_concated_bases_current_level
        (r:Regex.t)
      : Regex.t =
      begin match r with
        | Regex.RegExConcat (r1,r2) ->
          begin match retrieve_rightmost_concated_base r1 with
            | (r1o,Some s1) ->
              begin match try_insert_into_leftmost_concated_base r2 s1 with
                | None -> r
                | Some r2 ->
                  begin match r1o with
                    | None -> r2
                    | Some r1 -> Regex.RegExConcat (r1,r2)
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
    (merge_concated_bases
     % clean_regex
     % maximally_factor_regex)


let rec iteratively_deepen
    (rc:RegexContext.t)
    (r:Regex.t)
  : RegexContext.t * Regex.t =
  let regex_name =
    RegexContext.autogen_id
      rc
      r
  in
  let new_r =
    Regex.make_var regex_name
  in
  if (RegexContext.contains rc regex_name) then
    (rc, new_r)
  else
    begin match r with
      | Regex.RegExEmpty -> (rc,r)
      | Regex.RegExBase _ -> (rc,r)
      | Regex.RegExConcat (r1,r2) ->
        let (rc,r1) = iteratively_deepen rc r1 in
        let (rc,r2) = iteratively_deepen rc r2 in
        let regex_definition = Regex.make_concat r1 r2 in
        let rc =
          RegexContext.insert_exn
            rc
            regex_name
            regex_definition
            false
        in
        (rc, new_r)
      | Regex.RegExOr (r1,r2) ->
        let (rc,r1) = iteratively_deepen rc r1 in
        let (rc,r2) = iteratively_deepen rc r2 in
        let regex_definition = Regex.make_or r1 r2 in
        let rc =
          RegexContext.insert_exn
            rc
            regex_name
            regex_definition
            false
        in
        (rc, new_r)
      | Regex.RegExStar r' ->
        let (rc,r') = iteratively_deepen rc r' in
        let regex_definition = Regex.RegExStar r' in
        let rc =
          RegexContext.insert_exn
            rc
            regex_name
            regex_definition
            false
        in
        (rc, new_r)
      | Regex.RegExVariable _ ->
        (rc,r)
    end

let rec get_dnf_size
  : Regex.t -> int =
  fst
  %
  Regex.fold
    ~empty_f:(0,0)
    ~base_f:(fun _ -> (0,1))
    ~concat_f:(fun (size1,or_size1) (size2,or_size2) ->
        (size1*or_size2 + size2*or_size1, or_size1 * or_size2))
    ~or_f:(fun (size1,or_size1) (size2,or_size2) ->
        (size1+size2,or_size1+or_size2))
    ~star_f:(fun (size,_) ->
        (size+1,1))
    ~var_f:(fun _ -> (1,1))

