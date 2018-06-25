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
    (((atoms,strings,current_choices),p):ExampledDNFRegex.exampled_clause * Probability.t)
  : ExampledDNFRegex.exampled_clause * Probability.t =
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
  ((List.map ~f:(clean_exampledness_atom actual_choices) atoms,
    strings,
    actual_strings_choices)
  ,p)


and clean_exampledness_dnf_regex
    (above_choices:int list list example_data)
    ((clauses,current_choices):ExampledDNFRegex.t)
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
  (List.map ~f:(clean_exampledness_clause viable_choices_parsing) clauses,viable_choices)
  
let concat_exampled_dnf_regexs
    ((r1,sill1):ExampledDNFRegex.t)
    ((r2,sill2):ExampledDNFRegex.t)
  : ExampledDNFRegex.t =
  let cs_ps =
    cartesian_map
      ~f:(fun ((a1s,s1s,ilss1),p1) ((a2s,s2s,ilss2),p2) ->
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
              ~f:(clean_exampledness_atom choices_taken))
             (a1s@a2s),
           weld_lists (^) s1s s2s,
           parsings_strings),p1*.p2)
      r1
      r2
  in
  let all_examples =
    merge_example_data_list
      (List.concat)
      (List.map ~f:(fun ((_,_,ilss),_) -> ilss) cs_ps)
  in

  (*TODO make test that checks that won't get the information propagated*)
  (cs_ps,all_examples)

let or_exampled_dnf_regexs
    ((r1,sils1):ExampledDNFRegex.t)
    ((r2,sils2):ExampledDNFRegex.t)
    (p:Probability.t)
  : ExampledDNFRegex.t =
  let r1 = List.map ~f:(fun (c,p1) -> (c,p1 *. p)) r1 in
  let r2 = List.map ~f:(fun (c,p2) -> (c,p2 *. Probability.not p)) r2 in
  let cs : (ExampledDNFRegex.exampled_clause * Probability.t) list = r1@r2 in
  let all_examples =
    merge_example_data
      (@)
      sils1
      sils2
  in
  (cs,all_examples)

let exampled_atom_to_exampled_dnf_regex
    (a:ExampledDNFRegex.exampled_atom)
  : ExampledDNFRegex.t =
  let sils = ExampledDNFRegex.extract_atom_parsing_data a in
  ([(([a],["";""],sils),1.)],sils)

let star_exampled_dnf_regex
    (dr:ExampledDNFRegex.t)
    (p:Probability.t)
    (s:StochasticRegex.t)
  : ExampledDNFRegex.t =
  let (cs,sils) = dr in
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
                 ~cmp:(fun (i1,_) (i2,_) -> Int.compare i1 i2)
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
  exampled_atom_to_exampled_dnf_regex
    (ExampledDNFRegex.EAStar
       (dr,p,new_sils,s))

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
    begin match r.node with
      | ERegExEmpty -> ([],empty_parsing_example_data)
      | ERegExBase (c, ill) ->
        let sill = map_example_data (List.map ~f:(fun il -> (il,c))) ill in
        ([(([],[c],sill),1.)],sill)
      | ERegExConcat (r1,r2,_) ->
        concat_exampled_dnf_regexs
          (recursive_f r1)
          (recursive_f r2)
      | ERegExOr (r1,r2,_,p) ->
        (or_exampled_dnf_regexs
           (recursive_f r1)
           (recursive_f r2)
           p)
      | ERegExStar (r',ill,p) ->
        star_exampled_dnf_regex
          (recursive_f r')
          p
          (exampled_regex_to_regex r)
      | ERegExClosed (s,ss,ill) ->
        let ilss =
          merge_example_data
            List.zip_exn
            ill
            ss
        in
        exampled_atom_to_exampled_dnf_regex
          (EAClosed (s,ilss))
    end
end

module Memo = FixHCMemoizerOf(UnfixedExampledRegexToExampledDnfRegex)

let regex_to_exampled_dnf_regex
    (lc:LensContext.t)
    (r:StochasticRegex.t)
    (es:(int * string) list example_data)
  : ExampledDNFRegex.t option =
  let er_option = regex_to_exampled_regex r es in
  Option.map ~f:(Memo.evaluate) er_option
