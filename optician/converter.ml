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
    | EAClosed (sorig,el,cs) ->
      let orig_choice_zip = merge_example_data List.zip_exn el cs in
      let actual_choices_orig =
        merge_example_data
          (fun ud ch ->
             List.filter
               ~f:(fun (_,c) -> List.mem ~equal:(=) ch c)
               ud)
          orig_choice_zip
          choices
      in
      let (strs_orig,cs) = unzip_example_data (map_example_data List.unzip actual_choices_orig) in
      EAClosed (sorig,strs_orig,cs)
    | EAStar (r,cs,r_real) ->
      let actual_choices =
        merge_example_data
          (fun cm ch -> List.filter
             ~f:(fun c -> List.mem ~equal:(=) ch c)
             cm)
          cs
          choices
      in
      
      EAStar (clean_exampledness_dnf_regex actual_choices r, actual_choices,r_real)
  end
and clean_exampledness_clause
    (above_choices:int list list example_data)
    (((atoms,strings,current_choices),p):ExampledDNFRegex.exampled_clause * Probability.t)
  : ExampledDNFRegex.exampled_clause * Probability.t =
  
  
  let actual_choices =
    merge_example_data
      (fun choi abo ->
         List.filter
           ~f:(fun ch -> List.mem ~equal:(=) abo ch)
           choi)
      above_choices
      current_choices
  in

  ((List.map ~f:(clean_exampledness_atom actual_choices) atoms,
    strings,
    actual_choices)
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
  let contains_sublist (viable_choices:int list list) (lowerc:int list) 
    : bool =
    List.exists ~f:(is_suplist (List.rev lowerc)) (List.map ~f:List.rev
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
  (List.map ~f:(clean_exampledness_clause viable_choices) clauses,viable_choices)
  
let concat_exampled_dnf_regexs
    ((r1,_):ExampledDNFRegex.t)
    ((r2,_):ExampledDNFRegex.t)
  : (ExampledDNFRegex.exampled_clause * Probability.t) list =
  cartesian_map
    ~f:(fun ((a1s,s1s,c1s),p1) ((a2s,s2s,c2s),p2) ->
        let choices_taken =
          merge_example_data
            (intersect_lose_order_no_dupes
               (compare_list ~cmp:Int.compare))
            c1s
            c2s
        in
        ((List.map ~f:(clean_exampledness_atom choices_taken)) (a1s@a2s),
        weld_lists (^) s1s s2s,
        choices_taken),p1*.p2)
    r1
    r2 (*TODO make test that checks that won't get the information propagated*)
    
let or_exampled_dnf_regexs
    ((r1,_):ExampledDNFRegex.t)
    ((r2,_):ExampledDNFRegex.t)
    (p:Probability.t)
  : (ExampledDNFRegex.exampled_clause * Probability.t) list =
  let r1 = List.map ~f:(fun (c,p1) -> (c,p1 *. p)) r1 in
  let r2 = List.map ~f:(fun (c,p2) -> (c,p2 *. Probability.not p)) r2 in
  r1@r2
  
let exampled_atom_to_exampled_dnf_regex
    (a:ExampledDNFRegex.exampled_atom)
    (ill:int list list example_data)
  : ExampledDNFRegex.t =
  ([(([a],["";""],ill),1.)],ill)

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
        | ERegExBase (c, ill) -> ([(([],[c],ill),1.)],ill)
        | ERegExConcat (r1,r2,ill) ->
          (concat_exampled_dnf_regexs
             (recursive_f r1)
             (recursive_f r2),ill)
        | ERegExOr (r1,r2,ill,p) ->
          (or_exampled_dnf_regexs
             (recursive_f r1)
             (recursive_f r2)
             p,ill)
        | ERegExStar (r',ill,p) ->
          exampled_atom_to_exampled_dnf_regex
            (EAStar
               (recursive_f r'
               ,ill
               ,(exampled_regex_to_regex r)))
            ill
        | ERegExClosed (s,ss,ill) ->
          exampled_atom_to_exampled_dnf_regex
            (ExampledDNFRegex.EAClosed (s,ss,ill))
            ill
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
