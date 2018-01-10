open Stdlib
open Lang
open Eval
open Lens_put
open Normalized_lang
open Regexcontext
open Lenscontext
open Consts


let rec clean_exampledness_atom
    (choices:int list list)
    (a:exampled_atom)
  : exampled_atom =
  begin match a with
    | EAVariable (s,sorig,l,el,cs) ->
      let udef_choice_zip = List.zip_exn el cs in
      let actual_choices =
        List.filter
          ~f:(fun (_,c) -> List.mem ~equal:(=) choices c)
          udef_choice_zip
      in
      let (strs,cs) = List.unzip actual_choices in
      EAVariable (s,sorig,l,strs,cs)
    | EAStar (r,cs) ->
      
      let actual_choices =
        List.filter
          ~f:(fun ch -> List.mem ~equal:(=) choices ch)
          cs
      in
      
      EAStar (clean_exampledness_dnf_regex actual_choices r, actual_choices)
  end
and clean_exampledness_clause (above_choices:int list list)
    ((atoms,strings,current_choices):exampled_clause) : exampled_clause =
  
  
  let actual_choices =
    List.filter
      ~f:(fun ch -> List.mem ~equal:(=) above_choices ch)
      current_choices
  in
  
  (
    List.map ~f:(clean_exampledness_atom actual_choices) atoms,
    strings,
    actual_choices)
  

and clean_exampledness_dnf_regex (above_choices:int list list)
    ((clauses,current_choices):exampled_dnf_regex)  : exampled_dnf_regex =
  
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
  
  let viable_choices = List.filter ~f:(contains_sublist above_choices)
      current_choices in
  
  
  
  (List.map ~f:(clean_exampledness_clause viable_choices) clauses,viable_choices)
  
let concat_exampled_dnf_regexs ((r1,_):exampled_dnf_regex)
    ((r2,_):exampled_dnf_regex)
  : exampled_clause list =
  cartesian_map
    ~f:(fun (a1s,s1s,c1s) (a2s,s2s,c2s) ->
       let choices_taken = intersect_lose_order_no_dupes
           (compare_list ~cmp:compare)
           c1s
           c2s in
       (List.map ~f:(clean_exampledness_atom choices_taken)(a1s@a2s),
        weld_lists (^) s1s s2s,
        choices_taken))
    r1
    r2 (*TODO make test that checks that won't get the information propagated*)
    
let or_exampled_dnf_regexs ((r1,_):exampled_dnf_regex)
    ((r2,_):exampled_dnf_regex)
  : exampled_clause list =
  r1@r2
  
let exampled_atom_to_exampled_dnf_regex
    (a:exampled_atom)
    (ill:int list list)
  : exampled_dnf_regex =
  ([([a],["";""],ill)],ill)
  
let rec exampled_regex_to_exampled_dnf_regex (rc:RegexContext.t) (lc:LensContext.t) (r:exampled_regex) :
  exampled_dnf_regex =
  begin match r with
    | ERegExEmpty -> ([],[])
    | ERegExBase (c, ill) -> ([([],[c],ill)],ill)
    | ERegExConcat (r1,r2,ill) ->
      (concat_exampled_dnf_regexs
         (exampled_regex_to_exampled_dnf_regex rc lc r1)
         (exampled_regex_to_exampled_dnf_regex rc lc r2),ill)
    | ERegExOr (r1,r2,ill) ->
      (or_exampled_dnf_regexs
         (exampled_regex_to_exampled_dnf_regex rc lc r1)
         (exampled_regex_to_exampled_dnf_regex rc lc r2),ill)
    | ERegExStar (r',ill) ->
      exampled_atom_to_exampled_dnf_regex
        (EAStar (exampled_regex_to_exampled_dnf_regex rc lc r',ill))
        ill
    | ERegExVariable (s,ss,ill) ->
      if !use_lens_context then
        let (rep_type,converter) = LensContext.shortest_path_to_rep_elt lc s in
        let ss = List.map ~f:(lens_putr rc lc converter) ss in
        exampled_atom_to_exampled_dnf_regex
          (EAVariable (rep_type,s,converter,ss,ill))
          ill
      else
        exampled_atom_to_exampled_dnf_regex
          (EAVariable (s,s,Lens.LensIdentity (Regex.RegExVariable s),ss,ill))
          ill
  end

let regex_to_exampled_dnf_regex
    (c:RegexContext.t)
    (lc:LensContext.t)
    (r:Regex.t)
    (es:string list)
  : exampled_dnf_regex option =
  let er_option = regex_to_exampled_regex c r es in
  Option.map ~f:(exampled_regex_to_exampled_dnf_regex c lc) er_option
