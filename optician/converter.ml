open MyStdlib
open Lang
open Eval
open Lens_put
open Normalized_lang
open Lenscontext
open Consts


let rec clean_exampledness_atom
    (choices:int list list example_data)
    (a:ExampledDNFRegex.exampled_atom)
  : ExampledDNFRegex.exampled_atom =
  begin match a with
    | EASkip (r,r') ->
      EASkip(clean_exampledness_dnf_regex choices r, r')
    | EAClosed (sorig,csel) ->
      let actual_choices_orig =
        merge_example_data
          (fun ud ch ->
             List.filter
               ~f:(fun (c,_) -> List.mem ~equal:(=) ch c)
               ud)
          csel
          choices
      in
      EAClosed (sorig,actual_choices_orig)
    | EAStar (r,p,cs,r_real) ->
      let actual_choices_orig =
        merge_example_data
          (fun ud ch ->
             List.filter
               ~f:(fun (c,_) -> List.mem ~equal:(=) ch c)
               ud)
          cs
          choices
      in
      let sub_choices =
        map_example_data
          (List.map ~f:fst)
          actual_choices_orig
      in
      EAStar
        (clean_exampledness_dnf_regex sub_choices r
        ,p
        ,actual_choices_orig,r_real)
  end
and clean_exampledness_clause
    (above_choices:int list list example_data)
    (((atoms,strings,current_choices),p,i):ExampledDNFRegex.exampled_clause * Probability.t * int)
  : ExampledDNFRegex.exampled_clause * Probability.t * int =
  let actual_strings_choices =
    merge_example_data
      (fun choi abo ->
         List.filter
           ~f:(fun (ch,_) -> List.mem ~equal:(=) abo ch)
           choi)
      current_choices
      above_choices
  in
  let actual_choices =
    map_example_data
      (List.map ~f:fst)
      actual_strings_choices
  in
  ((List.map ~f:(fun (a,b) -> (clean_exampledness_atom actual_choices a,b)) atoms,
    strings,
    actual_strings_choices)
  ,p
  ,i)


and clean_exampledness_dnf_regex
    (above_choices:int list list example_data)
    ((clauses,current_choices,i):ExampledDNFRegex.t)
  : ExampledDNFRegex.t =
  let rec is_suplist (lowerc:int list) (upperc:int list) : bool =
    begin match (lowerc,upperc) with
      | (h1::t1,h2::t2) ->
        if h1 = h2 then
          is_suplist t1 t2
        else
          false
      | (_,[]) -> true
      | _ -> false
    end
  in
  let contains_sublist (viable_choices:int list list) (lowerc:int list * string)
    : bool =
    List.exists ~f:(is_suplist (List.rev (fst lowerc))) (List.map ~f:List.rev
                                                     viable_choices)
  in
  let viable_choices =
    merge_example_data
      (fun ac cc ->
         List.filter
           ~f:(contains_sublist ac)
           cc)
      above_choices
      current_choices
  in
  let viable_choices_parsing = map_example_data (List.map ~f:fst) viable_choices in
  (List.map
     ~f:(clean_exampledness_clause (viable_choices_parsing))
     clauses
  ,viable_choices
  ,i)

let concat_exampled_dnf_regexs
    ((r1,sill1,num_left):ExampledDNFRegex.t)
    ((r2,sill2,num_right):ExampledDNFRegex.t)
  : ExampledDNFRegex.t =
  let cs_ps =
    cartesian_map
      ~f:(fun ((a1s,s1s,ilss1),p1,i1) ((a2s,s2s,ilss2),p2,i2) ->
          let parsings_strings =
            merge_example_data
              (intersect_map_lose_order_no_dupes
                 ~cmp:(fun (il1,_) (il2,_) ->
                     compare_list ~cmp:Int.compare il1 il2)
                 ~f:(fun (il1,s1) (_,s2) -> (il1,s1^s2)))
              ilss1
              ilss2
          in
          let choices_taken = extract_parsing parsings_strings in
          ((List.map
              ~f:((fun (a,b) -> (clean_exampledness_atom choices_taken a, b))))
             (a1s@a2s),
           weld_lists (^) s1s s2s,
           parsings_strings),p1*.p2,i2*(num_left) + i1)
      r1
      r2
  in
  let all_examples =
    merge_example_data_list
      (List.concat)
      (List.map ~f:(fun ((_,_,ilss),_,_) -> ilss) cs_ps)
  in
  

  (*TODO make test that checks that won't get the information propagated*)
  (cs_ps,all_examples,num_left*num_right)

let or_exampled_dnf_regexs
    ((r1,sils1,num_left):ExampledDNFRegex.t)
    ((r2,sils2,num_right):ExampledDNFRegex.t)
    (p:Probability.t)
  : ExampledDNFRegex.t =
  let r1 = List.map ~f:(fun (c,p1,i1) -> (c,p1 *. p,i1)) r1 in
  let r2 = List.map ~f:(fun (c,p2,i2) -> (c,p2 *. Probability.not p,num_left+i2)) r2 in
  let cs = r1@r2 in
  let all_examples =
    merge_example_data
      (@)
      sils1
      sils2
  in
  (cs,all_examples,num_left+num_right)

let exampled_atom_to_exampled_dnf_regex
    (a:ExampledDNFRegex.exampled_atom)
  : ExampledDNFRegex.t =
  let sils = ExampledDNFRegex.extract_atom_parsing_data a in
  ([(([a,false],["";""],sils),1.,0)],sils,1)

let star_exampled_dnf_regex
    (ill:int list list example_data)
    (dr:ExampledDNFRegex.t)
    (p:Probability.t)
    (s:StochasticRegex.t)
  : ExampledDNFRegex.t =
  let (cs,sils,_) = dr in
  let keyed_sils =
    map_example_data
      (List.map
         ~f:(fun (il,s) ->
             let (ih,itl) = split_by_first_exn il in
             (itl,(ih,s))))
      sils
  in
  let keygroup_sils =
    map_example_data
      (group_by_keys
         ~is_eq:(comparison_to_equality
                 %% (compare_list ~cmp:Int.compare)))
      keyed_sils
  in
  let new_sils =
    map_example_data
      (List.map
         ~f:(fun (il,isl) ->
             let sorted_isl =
               List.sort
                 ~compare:(fun (i1,_) (i2,_) -> Int.compare i1 i2)
                 isl
             in
             (il
             ,List.fold_left
                ~f:(fun accs (_,s) ->
                    accs ^ s)
                ~init:""
                sorted_isl)))
      keygroup_sils
  in
  let old_new_example_data =
    merge_example_data
      (fun ill isll ->
         let nonempty_ill = List.map ~f:fst isll in
         let empty_ills =
           List.filter
             ~f:(fun il ->
                 not (List.mem ~equal:(fun x y -> 0 = (compare_list ~cmp:Int.compare) x y) nonempty_ill il))
             ill
         in
         let empty_isls =
           List.map
             ~f:(fun il -> (il,""))
             empty_ills
         in
         empty_isls@isll)
      ill
      new_sils
  in
  (*let empty_sils =
    map_example_data
      (fun isll ->
         let nonempty_ill = List.map ~f:fst isll in
         let empty_ills =
           List.filter
             ~f:(fun il ->
                 not (List.mem ~equal:Int.equal nonempty_ill il))
             ill
         in)*)
  exampled_atom_to_exampled_dnf_regex
    (ExampledDNFRegex.EAStar
       (dr,p,old_new_example_data,s))

let rec exampled_regex_to_regex
    (er:ExampledRegex.t)
  : StochasticRegex.t =
  begin match (ExampledRegex.node er) with
    | ERegExEmpty -> StochasticRegex.empty
    | ERegExBase (s,_) -> StochasticRegex.make_base s
    | ERegExConcat (er1,er2,_) ->
      StochasticRegex.make_concat
        (exampled_regex_to_regex er1)
        (exampled_regex_to_regex er2)
    | ERegExOr (er1,er2,_,p) ->
      StochasticRegex.make_or
        (exampled_regex_to_regex er1)
        (exampled_regex_to_regex er2)
        p
    | ERegExStar (er,_,p) ->
      StochasticRegex.make_star
        (exampled_regex_to_regex er)
        p
    | ERegExSkip er ->
      StochasticRegex.make_skip
        (exampled_regex_to_regex er)
    | ERegExRequire er ->
      StochasticRegex.make_require
        (exampled_regex_to_regex er)
    | ERegExClosed (r,_,_) ->
      StochasticRegex.make_closed
        r
  end

let rec exampled_regex_to_stochastic_regex
    (er:ExampledRegex.t)
  : StochasticRegex.t =
  begin match (ExampledRegex.node er) with
    | ERegExEmpty -> StochasticRegex.empty
    | ERegExBase (s,_) -> StochasticRegex.make_base s
    | ERegExConcat (er1,er2,_) ->
      StochasticRegex.make_concat
        (exampled_regex_to_stochastic_regex er1)
        (exampled_regex_to_stochastic_regex er2)
    | ERegExOr (er1,er2,_,p) ->
      StochasticRegex.make_or
        (exampled_regex_to_stochastic_regex er1)
        (exampled_regex_to_stochastic_regex er2)
        p
    | ERegExStar (er,_,p) ->
      StochasticRegex.make_star
        (exampled_regex_to_stochastic_regex er)
        p
    | ERegExSkip er ->
      StochasticRegex.make_skip
        (exampled_regex_to_stochastic_regex er)
    | ERegExRequire er ->
      StochasticRegex.make_require
        (exampled_regex_to_stochastic_regex er)
    | ERegExClosed (r,_,_) ->
      StochasticRegex.make_closed
        r
  end

module UnfixedExampledRegexToExampledDnfRegex =
struct
  module Arg = ExampledRegex
  module Result = ExampledDNFRegex

  let f
      (recursive_f:Arg.t -> Result.t)
      (r:ExampledRegex.t)
    : ExampledDNFRegex.t =
    (*let ini = ExampledRegex.extract_example_data r in*)
    (*let ans = *)
    begin match r.node with
      | ERegExEmpty ->
        ([],empty_parsing_example_data,0)
      | ERegExBase (c, ill) ->
        let sill = map_example_data (List.map ~f:(fun il -> (il,c))) ill in
        ([(([],[c],sill),1.,0)],sill,1)
      | ERegExConcat (r1,r2,_) ->
        let ans = concat_exampled_dnf_regexs
          (recursive_f r1)
          (recursive_f r2)
        in
        ans
      | ERegExOr (r1,r2,_,p) ->
        let ans = (or_exampled_dnf_regexs
           (recursive_f r1)
           (recursive_f r2)
           p) in
        ans
      | ERegExStar (r',ill,p) ->
        star_exampled_dnf_regex
          ill
          (recursive_f r')
          p
          (exampled_regex_to_regex r)
      | ERegExSkip (r') ->
        exampled_atom_to_exampled_dnf_regex
          (ExampledDNFRegex.EASkip (recursive_f r', exampled_regex_to_regex r))
      | ERegExClosed (s,ss,ill) ->
        let ilss =
          merge_example_data
            List.zip_exn
            ill
            ss
        in
        exampled_atom_to_exampled_dnf_regex
          (EAClosed (s,ilss))
      | ERegExRequire r' ->
        ExampledDNFRegex.make_required (recursive_f r')
    end
    (*in
      let endi = ExampledDNFRegex.extract_example_data ans in
    ans*)
end

module Memo = FixHCMemoizerOf(UnfixedExampledRegexToExampledDnfRegex)

let regex_to_exampled_dnf_regex
    (lc:LensContext.t)
    (r:StochasticRegex.t)
    (es:(int * string) list example_data)
  : ExampledDNFRegex.t option =
  let er_option = regex_to_exampled_regex r es in
  Option.map ~f:(Memo.evaluate) er_option
