open MyStdlib
open Lang
open Eval
open Lens_put
open Normalized_lang
open Lenscontext
open Consts


let rec clean_exampledness_atom
    (choices:int list list example_data)
    (a:exampled_atom)
  : exampled_atom =
  begin match a with
    | EAClosed (s,sorig,l,el,cs,el_orig) ->
      let udef_choice_zip =
        merge_example_data
          List.zip_exn
          el
          cs
      in
      let actual_choices =
        merge_example_data
          (fun ud ch ->
             List.filter
               ~f:(fun (_,c) -> List.mem ~equal:(=) ch c)
               ud)
          udef_choice_zip
          choices
      in
      let orig_choice_zip = merge_example_data List.zip_exn el_orig cs in
      let actual_choices_orig =
        merge_example_data
          (fun ud ch ->
             List.filter
               ~f:(fun (_,c) -> List.mem ~equal:(=) ch c)
               ud)
          orig_choice_zip
          choices
      in
      let (strs,cs) = unzip_example_data (map_example_data List.unzip actual_choices) in
      let (strs_orig,_) = unzip_example_data (map_example_data List.unzip actual_choices_orig) in
      EAClosed (s,sorig,l,strs,cs,strs_orig)
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
    (((atoms,strings,current_choices),p):exampled_clause * Probability.t)
  : exampled_clause * Probability.t =
  
  
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
    ((clauses,current_choices):exampled_dnf_regex)
  : exampled_dnf_regex =
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
    ((r1,_):exampled_dnf_regex)
    ((r2,_):exampled_dnf_regex)
  : (exampled_clause * Probability.t) list =
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
    ((r1,_):exampled_dnf_regex)
    ((r2,_):exampled_dnf_regex)
    (p:Probability.t)
  : (exampled_clause * Probability.t) list =
  let r1 = List.map ~f:(fun (c,p1) -> (c,p1 *. p)) r1 in
  let r2 = List.map ~f:(fun (c,p2) -> (c,p2 *. Probability.not p)) r2 in
  r1@r2
  
let exampled_atom_to_exampled_dnf_regex
    (a:exampled_atom)
    (ill:int list list example_data)
  : exampled_dnf_regex =
  ([(([a],["";""],ill),1.)],ill)

let rec exampled_regex_to_regex
    (er:exampled_regex)
  : StochasticRegex.t =
  begin match er with
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
    (er:exampled_regex)
  : StochasticRegex.t =
  begin match er with
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

let rec exampled_regex_to_exampled_dnf_regex
    (lc:LensContext.t)
    (r:exampled_regex) :
  exampled_dnf_regex =
  begin match r with
    | ERegExEmpty -> ([],empty_parsing_example_data)
    | ERegExBase (c, ill) -> ([(([],[c],ill),1.)],ill)
    | ERegExConcat (r1,r2,ill) ->
      (concat_exampled_dnf_regexs
         (exampled_regex_to_exampled_dnf_regex lc r1)
         (exampled_regex_to_exampled_dnf_regex lc r2),ill)
    | ERegExOr (r1,r2,ill,p) ->
      (or_exampled_dnf_regexs
         (exampled_regex_to_exampled_dnf_regex lc r1)
         (exampled_regex_to_exampled_dnf_regex lc r2)
         p,ill)
    | ERegExStar (r',ill,p) ->
      exampled_atom_to_exampled_dnf_regex
        (EAStar
           (exampled_regex_to_exampled_dnf_regex lc r'
           ,ill
           ,(exampled_regex_to_regex r)))
        ill
    | ERegExClosed (s,ss,ill) ->
      if !use_lens_context then
        let (rep_type,converter) = LensContext.shortest_path_to_rep_elt lc (StochasticRegex.to_regex s) in
        let ss' =
          map_example_data
            (List.map ~f:(lens_creater converter))
            ss
        in
        exampled_atom_to_exampled_dnf_regex
          (EAClosed (rep_type,s,converter,ss',ill,ss))
          ill
      else
        let rx = StochasticRegex.to_regex s in
        exampled_atom_to_exampled_dnf_regex
          (EAClosed (rx,s,Lens.Identity (Regex.make_closed rx),ss,ill,ss))
          ill
  end

let regex_to_exampled_dnf_regex
    (lc:LensContext.t)
    (r:StochasticRegex.t)
    (es:(int * string) list example_data)
  : exampled_dnf_regex option =
  let er_option = regex_to_exampled_regex r es in
  Option.map ~f:(exampled_regex_to_exampled_dnf_regex lc) er_option
