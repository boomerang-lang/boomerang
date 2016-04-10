(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2007-2008                                                    *)
(* J. Nathan Foster and Benjamin C. Pierce                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or              *)
(* modify it under the terms of the GNU Lesser General Public                 *)
(* License as published by the Free Software Foundation; either               *)
(* version 2.1 of the License, or (at your option) any later version.         *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *)
(* Lesser General Public License for more details.                            *)
(******************************************************************************)
(* /src/bcheck.ml                                                             *)
(* Boomerang type checker                                                     *)
(* $Id: bcheck.ml 4998 2011-03-16 21:53:34Z mgree $ *)
(******************************************************************************)

(* ------ module imports and abbreviations ----- *)
open Bsyntax
open Bident
open Benv
open Bprint
open Bsubst
open Berror
module L = Blenses.MLens
module C = Blenses.Canonizer
module V = Bvalue
module G = Bregistry

(* ----- function abbreviations ------ *)
let sprintf = Printf.sprintf  
let msg = Util.format
let (@) = Safelist.append
let v_of_rv = G.value_of_rv 

(* ----- helpers for sort checking ----- *)

(* [fresh_id x s] generates a identifier from [x] that is fresh for
   [s] by adding primes to [x]. *)
let rec fresh_id x s = 
  let i = Id.info_of_t x in 
  if Id.Set.mem x s then 
    fresh_id (Id.mk i (Id.string_of_t x ^ "'")) s
  else x

(* [fresh_qid x s] generates a qualified identifier for [x] that is
   fresh for the set of qualified identifiers [s] by adding primes to
   [x]). *)
let rec fresh_qid x s = 
  let q = Qid.t_of_id x in 
  let i = Id.info_of_t x in 
  if Qid.Set.mem q s then 
    fresh_qid (Id.mk i (Id.string_of_t x ^ "'")) s
  else 
    q

(* [get_con i sev li] looks up the type from constructor [li]. It
   returns:
      - the fully-qualified name
      - the free sort variables
      - and its constructors 
   For example 
       [get_con i sev Nil] 
     = ([List.t],['a],[(List.Nil, None);List.Cons ('a * List.t)])
*)
let get_con i sev li = match SCEnv.lookup_con sev li with
  | None -> static_error i 
      (fun () -> msg "@[unbound@ constructor@ %s@]" (Qid.string_of_t li))
  | Some r -> r

(* [get_type f i q] is like get_con, but looks up the type using [f]. *)
let get_type f i q = match f q with
  | None -> static_error i 
      (fun () -> msg "@[unbound@ type@ %s@]" (Qid.string_of_t q))
  | Some r -> r

(* [inst_cases subst cl] instantiates a list of cases [cl] by applying
   [subst] to the variables in [cl]. *)
let inst_cases subst cl = 
  Safelist.map 
    (fun (li,so) -> (li,Misc.map_option (subst_sort subst) so)) 
    cl

(* ------ compatibility / casting ----- *)

(* determine whether a term is (immediately interpreted as) a value *)
let rec is_value e0 = match e0 with
  | EVar _
  | EFun _ 
  | ETyFun _
  | EUnit _
  | EBoolean _
  | EInteger _
  | EChar _ 
  | EString _ 
  | ECSet _ -> true
  | EPair(_,e1,e2) -> is_value e1 && is_value e2
  | _ -> false

let rec needs_coercion f t = match f,t with
  | SChar,SString 
  | SChar,SRegexp
  | SChar,SAregexp
  | SChar,SLens
  | SString,SRegexp 
  | SString,SAregexp 
  | SString,SLens 
  | SRegexp,SAregexp
  | SRegexp,SLens
      -> true
  | SProduct(s11,s12),SProduct(s21,s22) -> 
      needs_coercion s11 s21 || needs_coercion s12 s22
  | SData(sl1,qx),SData(sl2,qy) -> 
      (* check qx equals qy and sl1 and sl2 pairwise compatible *)
      if not (Qid.equal qx qy) then false
      else
        let ok,sl12 = 
          try false, Safelist.combine sl1 sl2 
          with Invalid_argument _ -> (false,[]) in 
        Safelist.fold_left
          (fun b (s1i,s2i) -> b || needs_coercion s1i s2i)
          ok sl12
  | SFunction(_,s11,s12),SFunction(_,s21,s22) ->
      needs_coercion s21 s11 || needs_coercion s12 s22
  | _ -> false

let mk_coercion s i f t e =
  let f_base = erase_sort f in
  let t_base = erase_sort t in
  if needs_coercion f_base t_base
  then
    let cast = ECast (i,f_base,t_base,i,e) in
    Trace.debug "coerce"
      (fun () -> 
	 msg "@[%s: " s;
	 format_exp cast;
	 msg "@]@\n");
    cast
  else e

let rec compatible f t = match f,t with
  (* identity at base types *)
  | SUnit,SUnit       
  | SInteger,SInteger 
  | SBool,SBool       
  | SChar,SChar
  | SString,SString 
  | SRegexp,SRegexp 
  | SAregexp,SAregexp
  | SSkeletons,SSkeletons
  | SResources,SResources
  | SLens,SLens 
  | SCanonizer,SCanonizer
      -> true
  | SPrefs f, SPrefs t when f = t -> true
  (* coercions between {char,string,regexp,aregexp,lens} *)
  | SChar,SString 
  | SChar,SRegexp
  | SChar,SAregexp
  | SChar,SLens
  | SString,SRegexp 
  | SString,SAregexp 
  | SString,SLens 
  | SRegexp,SAregexp
  | SRegexp,SLens
      -> true
  | SFunction(_,s11,s12),SFunction(_,s21,s22) -> 
      (* note: contravariant in argument! *)
      compatible s21 s11 && compatible s12 s22
  | SProduct(s11,s12),SProduct(s21,s22) -> 
      compatible s11 s21 && compatible s12 s22
  | SData(sl1,qx),SData(sl2,qy) -> 
      (* check qx equals qy and sl1 and sl2 pairwise compatible *)
      if not (Qid.equal qx qy) then false
      else
        let ok,sl12 = 
          try true, Safelist.combine sl1 sl2 
          with Invalid_argument _ -> (false,[]) in 
        Safelist.fold_left
          (fun b (s1i,s2i) -> b && compatible s1i s2i)
          ok sl12
  | SVar x, SVar y -> 
      Id.equal x y
  | SForall(x,s1),SForall(y,s2) -> 
      if Id.equal x y then compatible s1 s2
      else   
        (* alpha vary f and t if needed *) 
        let f_fvs = free_sort_vars s1 in 
        let t_fvs = free_sort_vars s2 in 
        let z = fresh_id x (Id.Set.union f_fvs t_fvs) in
        let f_subst = [x,SVar z] in 
        let t_subst = [y,SVar z] in 
        compatible (subst_sort f_subst s1) (subst_sort t_subst s2)
  | SRefine(_,_,s11,_),s2 -> 
      compatible s11 s2
  | s1,SRefine(_,_,s21,_) -> 
      compatible s1 s21
  | _ -> false

let rec trivial_cast f t =  
  let res = 
  f == t || syneq_sort f t ||
  match f,t with
    | SRefine(_,_,f',_),_ -> 
        trivial_cast f' t (* out of a refinement *)
    | SFunction(x,f1,f2), SFunction(y,t1,t2) ->
	trivial_cast t1 f1 &&
	let f_fvs = free_exp_vars_in_sort f2 in
	let t_fvs = free_exp_vars_in_sort t2 in
	let qz = fresh_qid x (Qid.Set.union f_fvs t_fvs) in
	let e_z = mk_qid_var qz in 
	let f_subst = [Qid.t_of_id x,e_z] in
	let t_subst = [Qid.t_of_id y,e_z] in
        trivial_cast 
          (subst_exp_in_sort f_subst f2) 
          (subst_exp_in_sort t_subst t2)
      | SProduct(f1,f2),SProduct(t1,t2) ->
	  trivial_cast f1 t1 && trivial_cast f2 t2
      | SForall(a,f'),SForall(b,t') -> 
          let f_fvs = free_sort_vars f in 
          let t_fvs = free_sort_vars t in 
          let z = fresh_id a (Id.Set.union f_fvs t_fvs) in
          let f_subst = [a,SVar z] in 
          let t_subst = [b,SVar z] in 
	  trivial_cast 
            (subst_sort f_subst f') 
            (subst_sort t_subst t')
      | SData(fl,x),SData(tl,y) ->
	  let rec all_trivial acc fl tl =
	    acc && match fl,tl with
	      | f::fl',t::tl' -> all_trivial (trivial_cast f t) fl' tl'
	      | [],[] -> true
	      | _ -> false in
	  all_trivial (Qid.equal x y) fl tl
      | _ -> false in 
    res
let rec mk_cast s i f t e = 
  let cast = ECast(i,f,t,i,e) in
  let trivial = trivial_cast f t in
  Trace.debug (if trivial then "trivial" else "cast")
    (fun () -> 
       msg "@[%s: " s;
       format_exp cast;
       msg "@]@\n");
  if trivial
  then e
  else cast

(* generate the "negative" cast: <S => base(S)> *)
let mk_neg_cast m i s0 e1 = 
  let s0_base = erase_sort s0 in
  (s0_base, mk_cast m i s0 s0_base e1)

(* generate the "positive" cast: <base(S) => S> *)
let mk_pos_cast m i s0 e1 = 
  (s0, mk_cast m i (erase_sort s0) s0 e1)

let mk_bulletproof_cast m i s0 e1 =
  let (_,e1') = mk_pos_cast m i s0 e1 in
  mk_neg_cast m i s0 e1'

(* resolve_label: helper for static_match. takes a base qid [li] a
   target qid [lj] and a context [os]. it dots [li] with elements of
   [os] until it finds a match for [lj]. *)
let rec resolve_label li lj = function
  | [] -> None
  | o::os -> 
      let qi = Qid.t_dot_t o li in 
      if Qid.equal qi lj then Some qi
      else resolve_label li lj os 

(* static_match: determine if a value with a given sort *could* match
   a pattern; annotate PVars with their sorts, return the list of sort
   bindings for variables. *)
let rec static_match i sev p0 s = 
  let err p str s = 
    static_error i 
      (fun () -> 
         msg "@[in@ pattern@ %s:@ expected %s,@ but@ found@ %s@]"
           (string_of_pat p) str (string_of_sort s)) in 
  match p0 with 
    | PWld _ -> Some (p0,[])
    | PVar(i,x,_) -> Some (PVar(i,x,Some s),[(x,s)])
    | PUnt _ ->
        if not (compatible s SUnit) then err p0 "unit" s;
        Some (p0,[])
    | PInt _ -> 
        if not (compatible s SInteger) then err p0 "int" s;
        Some (p0,[])
    | PBol _ ->
        if not (compatible s SBool) then err p0 "bool" s;
        Some (p0,[])
    | PCex (pi,p1) -> 
        if not (compatible s SBool) then err p0 "bool" s;
	begin match static_match i sev p1 SString with
	  | None -> err p1 "string" SString
	  | Some (new_p1,binds) -> Some (PCex(pi,new_p1),binds)
	end
    | PStr _ -> 
        if not (compatible s SString) then err p0 "string" s;
        Some (p0,[])
    | PVnt(pi,li,ptio) -> 
        begin match expose_sort s with 
          | SData(sl1,qy) -> 
              (* lookup the type from constructor [li]. *)
              let qx,(svl,cl) = get_con i sev li in
              (* check qx equals qy and instantiate [cl] with [sl1]. *)
              let cl_inst =
		if not (Qid.equal qx qy)
		then let s_expected = SData(sl_of_svl svl,qx) in
                  err p0 (string_of_sort s_expected) s
		else
                  let subst = Safelist.combine svl sl1 in
                  inst_cases subst cl
              in
              (* loop over [cl_inst] to find the constructor *)
              let rec find_label = function
                | [] -> None
                | (lj,sjo)::rest ->  
		    let go qi = match ptio,sjo with 
                      | None,None -> 
                          Some(PVnt(pi,qi,ptio),[])
                      | Some pti,Some sj -> 
                          begin match static_match i sev pti sj with
			    | Some (new_pti,binds) -> 
                                Some (PVnt(pi,qi,Some new_pti),binds)
			    | None -> 
                                find_label rest
                          end
                      | _ -> 
                          static_error i 
			    (fun () -> 
                               msg "@[wrong@ number@ of@ arguments@ to@ %s@]" 
                                 (Qid.string_of_t lj)) in 
		    if Qid.equal li lj then go li
		    else match resolve_label li lj (SCEnv.get_ctx sev) with
		      | None -> find_label rest
		      | Some qi -> go qi in 
              find_label cl_inst
          | _ -> err p0 "data type" s
        end
            
    | PPar(pi,p1,p2) -> 
        begin match s with 
          | SProduct(s1,s2) -> 
              begin match
                static_match i sev p1 s1, 
                static_match i sev p2 s2 
              with 
                | Some (new_p1,l1),Some(new_p2,l2) -> 
                    Some (PPar(pi,new_p1,new_p2),l1 @ l2)
                | _ -> None 
              end
          | _ -> err p0 "product" s
        end

(* ------ normalized representation of grammars ----- *)
(* tagged atoms *)
type tatom =
  | Bare of exp
  | Labeled of Qid.t

(* normalized rules and productions *)
type nrule =
  | RecRule of Info.t * exp * Qid.t
  | NonRecRule of Info.t * exp

type nprod = NProd of Info.t * Qid.t * nrule list

(* NOTE FROM MMG TO FUTURE SELF (or anyone else, I suppose)
 * 
 * All of the real casting action is in the EFun case, where a
 * "bulletproofing" cast (a pair of postive and negative checks) is
 * wrapped around the function.  Putting it on the function (and
 * treating external function types as their erasure) avoids the nasty
 * dependent application blowup from terms like:
 * 
 * op (op (op e1 e2) e3) e4
 * 
 * Do not be confused by the erasure in the EVar case!  It's not
 * breaking anything.
 * 
 * There is still room for optimization: this checking strategy hides
 * type information as it goes, so we're losing the opportunity to
 * eliminate some casts by, e.g., reflexivity.
 *
 *)

(* ------ sort resolution and compilation ----- *)
(* check_sort: resolves qids in SDatas and checks expressions in SRefines *)
let rec check_sort i sev s0 = 
  let rec go s0 = match s0 with 
    | SUnit | SBool | SInteger | SChar | SString 
    | SRegexp | SAregexp | SSkeletons | SResources | SLens | SCanonizer | SPrefs _ | SVar _ -> 
        s0
    | SFunction(x,s1,s2) -> 
        let new_s1 = go s1 in 
        let sev1 = SCEnv.update sev (Qid.t_of_id x) (G.Sort new_s1) in 
        let new_s2 = check_sort i sev1 s2 in  
        SFunction(x,new_s1,new_s2)
    | SProduct(s1,s2)  -> 
        SProduct(go s1,go s2)
    | SData(sl,qx) ->
        let qx', (slx, _) =
          match SCEnv.lookup_type sev qx with
          | None ->
	      static_error i
                (fun () -> msg "@[cannot@ resolve@ sort@ %s@]" (Qid.string_of_t qx))
          | Some q -> q
        in
        if not (Safelist.length slx = Safelist.length sl)
        then static_error i
          (fun () -> msg "@[wrong@ number@ of@ argument@ for@ %s@]" (Qid.string_of_t qx))
        else SData (Safelist.map go sl ,qx')
    | SForall(x,s1) -> SForall(x,go s1)
    | SRefine(x,b1,s1,e1) -> 
        let sev1 = SCEnv.update sev (Qid.t_of_id x) (G.Sort (erase_sort s1)) in 
        let s1' = check_sort i sev1 s1 in 
        let sev2 = SCEnv.update sev (Qid.t_of_id x) (G.Sort s1') in 
        let e1_sort,new_e1 = check_exp sev2 e1 in  
        if not (compatible e1_sort SBool) then
          static_error i 
	    (fun () -> msg "@[in@ refinement: expected@ %s@ but@ found@ %s@]"
               (string_of_sort SBool)
               (string_of_sort e1_sort));
          SRefine(x,b1,s1',new_e1) in 
    go s0

(* ------ sort checking expressions ----- *)
and check_exp_app i sev (e1_sort,new_e1) (e2_sort,new_e2) =
  match expose_sort e1_sort with 
    | SFunction(x,param_sort,return_sort) -> 
        let i2 = info_of_exp new_e2 in 
          if not (compatible e2_sort param_sort) then 
            static_error i2
	      (fun () ->                    
                 msg "@[in@ application:@ expected@ %s@ but@ found@ %s@]"
                   (string_of_sort param_sort)
                   (string_of_sort e2_sort));
	  (* coerce the argument *)
	  let coerce_e2 = mk_coercion "application argument"
	    i2 e2_sort param_sort new_e2 in
	  let new_e0 = EApp(i,new_e1,coerce_e2) in 
	  (* compute return sort *)
	  let e0_sort =
            if Id.equal x Id.wild 
	    then return_sort (* simple arrows *)
	    else run_error (info_of_exp new_e1)
	      (fun () ->
		 msg "@[in@ application:@ found a dependent arrow %s@]"
		   (string_of_sort e1_sort))
	      (* DEP substitution would go here *)
	  in
          (e0_sort,new_e0)
    | _ -> 
        static_error (info_of_exp new_e1)
          (fun () ->              
             msg "@[in@ application:@ expected@ function@ sort@ but@ found@ %s.@]"
	       (string_of_sort e1_sort))

and lookup_var sev i q =
  match SCEnv.lookup_o sev q with
  | Some (full_q, G.Sort s) -> full_q, s
  | Some (full_q, G.Unknown) ->
      run_error i
        (fun () -> msg "@[%s is bound to an unknown type@]"
           (Qid.string_of_t full_q))
  | None ->
      static_error i
        (fun () -> msg "@[%s is not bound@]"
           (Qid.string_of_t q))

and check_exp ?(in_let=false) sev e0 = 
  match e0 with
    | EVar(i,q) ->
        (* lookup the sort in the context *)
        let full_q, e0_sort = lookup_var sev i q in
        let e0_sort_base = erase_sort e0_sort in
        (e0_sort_base, (EVar(i,full_q)))

    | EOver(i,op,es) -> begin 
        (* type check and instrument es *)
        let rs = 
          Safelist.fold_right
            (fun ei rs -> 
	       let ri = check_exp sev ei in 
                 ri::rs)
            es [] in
        let err () =
          static_error i (
            fun () -> msg "@[could@ not@ resolve@ %s@ for@ %a@]"
              (string_of_op op)
              (fun _ -> Misc.format_list "@ and@ " format_sort) (Safelist.map fst rs))
        in 
        (* rules for overloaded symbols *)
	let bin_rules =
	  [ ODot, 
	    [ SString, "string_concat";
	      SRegexp, "regexp_concat";
	      SAregexp, "aregexp_concat";
	      SLens, "lens_concat";
	      SCanonizer, "canonizer_concat" ]
	  ; OTilde,
	    [ SLens, "lens_swap";
	      SCanonizer, "canonizer_swap" ] 
	  ; OMinus,
	    [ SRegexp, "diff";
	      SInteger, "minus" ]
	  ; OBar,
	    [ SRegexp, "regexp_union";
              SAregexp, "aregexp_union";
	      SLens, "lens_disjoint_union";
	      SCanonizer, "canonizer_union" ]
	  ; OAmp,    [ SRegexp, "inter" ]
	  ; OBarBar, [ SLens, "lens_union";
		       SBool, "lor";
		     ]
	  ; OAmpAmp, [ SBool, "land";
		     ]
	  ; ODarrow, [ SLens, "set" ]
	  ; ODeqarrow, [ SLens, "rewrite" ]
	  ; OLt, [ SInteger, "blt" ] 
	  ; OLeq, [ SInteger, "bleq" ] 
	  ; OGt, [ SInteger, "bgt" ] 
	  ; OGeq, [ SInteger, "bgeq" ] ] in
        (* helper to find rule *)
        let rec find_rule r rs = match r with 
          | [] -> None
          | (s,op_name)::t -> 
	      if Safelist.for_all (fun (si,_) -> compatible si s) rs then 
                Some op_name
	      else 
                find_rule t rs in 
        (* rewrite the overloaded symbol using the rules above; 
           the treatment of [OIter] is special *)
        let (op_s,op_e) = match op,rs with
            | OIter(min,max),[r1] ->
		let (e1_sort,_) = r1 in
		let mk_iter iter_id min max =
		  let r_min  = check_exp sev (mk_int i min) in
		  let r_max  = check_exp sev (mk_int i max) in
		  let r_iter = check_exp sev (mk_core_var i iter_id) in
		    check_exp_app i sev 
                      (check_exp_app i sev 
                         (check_exp_app i sev r_iter r1) r_min) r_max in
		if compatible e1_sort SRegexp
		then mk_iter "regexp_iter" min max
                else if compatible e1_sort SAregexp
                then mk_iter "aregexp_iter" min max
		else if compatible e1_sort SLens
		then
		  let checked_app f_name =
		    let r_f = check_exp sev (mk_core_var i f_name) in
		    check_exp_app i sev r_f r1 in 
		  match min,max with
		    | (0,0) -> 
                        check_exp_app i sev 
                          (check_exp sev (mk_core_var i "copy"))
                          (check_exp sev (mk_core_var i "EPSILON"))
                    | (1,1) -> r1
		    | (0,1) -> checked_app "lens_option"
		    | (0,-1) -> checked_app "lens_star"
		    | (0,max) -> 
                        check_exp_app i sev
                          (check_exp sev (mk_core_var i "lens_option"))
                          (mk_iter "lens_iter" 1 max)
                    | (1,-1) -> checked_app "lens_plus"
                    | (min,-1) -> 
                        check_exp_app i sev
                          (check_exp_app i sev
                             (check_exp sev (mk_core_var i "lens_concat"))
                             (mk_iter "lens_iter" min min))
                          (checked_app "lens_star")
		    | _ -> mk_iter "lens_iter" min max
		else if compatible e1_sort SCanonizer
		then mk_iter "canonizer_iter" min max
		else err ()
            | OEqual,[e1_sort,e1; _,e2] ->
                (SBool,
                 mk_app3 i 
                   (mk_tyapp i (mk_qid_var (Qid.mk_core_t i "equals")) e1_sort) 
                   e1 e2)
            (* !!! why is this logic duplicated?  can these not fit in the table above? --MMG *)
            | OMatch, [tag; exp] ->
                let tag_sort = SData ([], V.tag_qid) in
                if not (compatible (fst tag) tag_sort) then err ();
                let rules = [ SLens, "lens_match";
                              SAregexp, "aregexp_match" ]
                in
                let rec find_rule r =
                  match r with
                  | [] -> None
                  | (s, op_name)::r ->
                      if compatible (fst exp) s
                      then Some op_name
                      else find_rule r
                in
                (match find_rule rules with
                 | Some op_id ->
                     let app = check_exp_app i sev in
                     let r_op = check_exp sev (mk_core_var i op_id) in
                     app (app r_op tag) exp
                 | None -> err ()
                )
            | OWeight, [force;weight;se] ->
                if not (compatible (fst force) SBool) then err ();
                if not (compatible (fst weight) SInteger) then err ();
                let rules = [ SLens, "lens_weight";
                              SAregexp, "aregexp_weight" ]
                in
                let rec find_rule r =
                  match r with
                  | [] -> None
                  | (s, op_name)::r ->
                      if compatible (fst se) s
                      then Some op_name
                      else find_rule r
                in
                (match find_rule rules with
                 | Some op_id ->
                     let r_op = check_exp sev (mk_core_var i op_id) in
                     let app = check_exp_app i sev in
                     app (app (app r_op force) weight) se
                 | None -> err ()
                )
            | op,[r1; r2] -> begin
                let rules = try Safelist.assoc op bin_rules with _ -> err () in 
                  match find_rule rules rs with 
                    | Some op_id ->
			let r_op = check_exp sev (mk_core_var i op_id) in
                        check_exp_app i sev (check_exp_app i sev r_op r1) r2
		    | None -> err ()
	      end
            | _ -> err () in 
	  (op_s,op_e)
      end

    | EFun(i,Param(p_i,p_x,p_s),ret_sorto,body) ->
        (* resolve the parameter sort *)
        let new_p_s = check_sort p_i sev p_s in 
        (* create the environment for the body *)
        let body_sev = SCEnv.update sev (Qid.t_of_id p_x) (G.Sort new_p_s) in
        let new_ret_sorto,(body_sort,new_body) = 
          match ret_sorto with 
            | None -> 
                (* if no return sort declared, just check the body 
		   to handle nested function definitions, we pass on
		   the value of in_let *)
                None,(check_exp ~in_let:in_let body_sev body)
            | Some ret_sort ->
                (* otherwise, resolve the declared return sort *)
                let new_ret_sort = check_sort i body_sev ret_sort in 
                (* then check the body -- if we were in a let, then so
                   is the body (needed for nested functions) *)
                let body_sort,new_body = check_exp ~in_let:in_let body_sev body in
                (* and check that the declared return sort is a subsort 
                   of the actual body sort *)
                  if not (compatible body_sort new_ret_sort) then
                    static_error i
		      (fun () ->
                         msg "@[in@ function:@ %s@ expected@ but@ %s@ found@]"
                           (string_of_sort new_ret_sort)
                           (string_of_sort body_sort));
                  let cast_body = 
                    mk_cast "function body" 
                      (info_of_exp body) body_sort new_ret_sort new_body in
                  (Some new_ret_sort,(new_ret_sort,cast_body)) in 
	(* make the function type dependent if needed *)
        let dep_x = 
          if Qid.Set.mem (Qid.t_of_id p_x) (free_exp_vars_in_sort body_sort) 
	  then p_x
          else 
	    (Trace.debug "dep"
	      (fun () -> 
		 msg "@[free vars in@ ";
		 format_sort body_sort;
		 msg "@ {@ ";
		 Qid.Set.iter (fun q -> msg "%s@ " (Qid.string_of_t q)) (free_exp_vars_in_sort body_sort);
		 msg "}@]\n");
	    Id.wild) in
        let e0_sort = 
          SFunction(dep_x,new_p_s,body_sort) in
        let new_p = Param(p_i,p_x,new_p_s) in
        let new_e0 = EFun(i,new_p,new_ret_sorto,new_body) in
	(* apply positive and negative casts (if we're not immediately in a let) *)
	if not in_let 
	then mk_bulletproof_cast "fun" i e0_sort new_e0
	else (e0_sort,new_e0)

    | ELet(i,b,e) ->
        (* for let-expressions, check the bindings *)
        let bevs,xs,Bind(new_bi,new_bp,new_bso,new_be) = check_binding ~in_let:true sev b in
        (* use the resulting environment to check the exp *)
        let e_sort,new_e = check_exp bevs e in
	(* put a positive cast on the bound term *)
	let be_sort = match new_bso with 
	  | Some new_bs -> new_bs
	  | None -> run_error i 
	      (fun () -> 
		 msg "@[couldn't@ unpack@ sort@ in@ let-binding@]") in
	let (be_sort,cast_be) = mk_pos_cast "let" new_bi be_sort new_be in 
        (* put in the bound-term and inner term *)
        let new_e0 = ELet(i,Bind(new_bi,new_bp,Some be_sort,cast_be),new_e) in 
	(* DEP substitution would go here *)
	if not (Qid.Set.equal (free_exp_vars_in_sort e_sort) Qid.Set.empty)
	then run_error i
	    (fun () ->
	       msg "@[in@ let:@ found a binding pattern %s@]"
		 (string_of_sort e_sort));
        (e_sort,new_e0)

    | EPair(i,e1,e2) -> 
        (* for pairs, recursively check e1 and e2 *)
        let e1_sort,new_e1 = check_exp sev e1 in 
        let e2_sort,new_e2 = check_exp sev e2 in 
        let e0_sort = SProduct(e1_sort,e2_sort) in 
        let new_e0 = EPair(i,new_e1,new_e2) in 
          (e0_sort,new_e0)

    | EUnit(_) -> 
        (* units have sort SUnit *)
        (SUnit,e0)

    | EBoolean(i,Some e1) ->
	let e1_sort,new_e1 = check_exp sev e1 in
	if not (compatible e1_sort SString) then
          static_error i
	    (fun () -> msg "@[in@ counterexample: expected@ %s@ but@ found@ %s@]"
               (string_of_sort SString)
               (string_of_sort e1_sort));
	(SBool,EBoolean(i,Some new_e1))

    | EBoolean(_,None) -> 
        (* boolean constants have sort SBool *)
        (SBool,e0)

    | EInteger(_) -> 
        (* integer constants have sort SInteger *)
        (SInteger,e0)

    | EChar(_) -> 
        (* character constants have sort SChar *)
        (SChar,e0)

    | EString(_) -> 
        (* string constants have sort SString *)
        (SString,e0)

    | ECSet(_) -> 
        (* character sets have sort SRegexp *)
        (SRegexp,e0)

    | EApp(i,e1,e2) ->       
        let (e1_sort,new_e1) as r1 = check_exp sev e1 in 
        let (e2_sort,new_e2) as r2 = check_exp sev e2 in 
	check_exp_app i sev r1 r2

    | ECase(i,e1,pl,ps) ->
        assert (pl != []);
        (* helper function for printing error messages *)
        let err2 i p s1 s2 = static_error i (fun () -> msg p s1 s2) in 
        (* check the expression being matched *)
        let e1_sort,new_e1 = check_exp sev e1 in
        let new_pl_rev = Safelist.fold_left
          (fun new_pl_rev (pi,ei) ->
             match static_match i sev pi e1_sort with 
	       | None -> 
                   (* if the branch is useless, raise an exception *)
                   err2 i "@[pattern@ %s@ does@ not@ match@ sort@ %s@]" 
                     (string_of_pat pi) 
                     (string_of_sort e1_sort)
	       | Some (new_pi,binds) ->                
                   (* otherwise, extend the environment with bindings for pattern vars *)
                   let ei_sev = Safelist.fold_left 
                     (fun ei_sev (qj,sj) -> SCEnv.update ei_sev (Qid.t_of_id qj) (G.Sort sj))
                     sev binds in 
                   (* sort check the expression *) 
                   let ei_sort,new_ei = check_exp ei_sev ei in
                   (ei,ei_sort,new_ei,new_pi)::new_pl_rev)
          [] pl
        in
        let new_ps, refinement_check =
          begin match ps with
            | Some ps -> (* resolve the sort *)
                check_sort i sev ps, false
            | None -> (* use the first expression for the type *)
                let _,e0_sort,_,_ = Safelist.hd new_pl_rev in
                e0_sort, true (* should check if all types are not refined *)
          end
        in
        let new_pl = Safelist.fold_left
          (fun new_pl (ei,ei_sort,new_ei,new_pi) ->
             let ii = info_of_exp ei in
               (* check that the type is not refined when omitting the type of the match *)
               if refinement_check && is_refined ei_sort then
                 static_error ii
                   (fun () -> msg "@[refined@ type@ %s@ used@ in@ match@ without@ explicit@ type@ declaration@]"
                      (string_of_sort ei_sort));
               (* and check that it is compatible with the sorts of the other branches *)
               if not (compatible ei_sort new_ps) then 
		 err2 ii
                   "@[in@ match:@ %s@ expected@ but@ %s@ found@]"
                   (string_of_sort new_ps)
                   (string_of_sort ei_sort);
               let cast_ei = mk_cast "match branch" ii ei_sort new_ps new_ei in
               (new_pi,cast_ei)::new_pl)
          [] new_pl_rev
        in
        let new_e0 = ECase(i, new_e1, new_pl, Some new_ps) in
        (new_ps, new_e0)

    | ETyFun(i,x,e1) -> 
        let e1_sort,new_e1 = check_exp sev e1 in 
        let new_e0 = ETyFun(i,x,new_e1) in 
        let e0_sort = SForall(x,e1_sort) in 
        (e0_sort,new_e0)

    | ETyApp(i,e1,s2) -> 
        let e1_sort,new_e1 = check_exp sev e1 in 
        let new_s2 = check_sort i sev s2 in 
        let e0_sort = match e1_sort with 
          | SForall(x,s11) -> subst_sort [x,new_s2] s11
          | _ -> static_error i
	      (fun () -> msg "@[in@ type@ application:@ expected@ universal@ type@ but@ %s@ found.@]"
                 (string_of_sort e1_sort)) in 
        let new_e0 = ETyApp(i,new_e1,new_s2) in 
        (e0_sort,new_e0)

    | ECast(i,f,t,b,e) -> 
        static_error i (fun () -> msg "@[unexpected@ cast@ expression@ in@ source@ term@]")

    | EGrammar(i,ps) ->
        (* helpers for constructing asts *)
        let mk_concat e1 e2 =
          mk_app3 i (mk_core_var i "lens_concat") e1 e2 in
        let mk_rx_concats es = match es with
          | [] -> mk_core_var i "EPSILON"
          | [e1] -> e1
          | e1::rest ->
              Safelist.fold_left
                (fun acc ei -> mk_app3 i (mk_core_var i "regexp_concat") acc ei)
                e1 rest in
        let mk_union e1 e2 =
          mk_app3 i (mk_core_var i "lens_disjoint_union") e1 e2 in
        let mk_unions = function
          | [] -> (mk_core_var i "EMPTY")
          | e1::rest -> Safelist.fold_left mk_union e1 rest in
        let mk_star e1 =
          mk_app i (mk_core_var i "lens_star") e1 in
        let mk_list s es =
          Safelist.fold_right
            (fun ei acc ->
               mk_app i
                 (ETyApp(i,mk_list_var i "Cons",s))
                 (EPair(i,ei,acc)))
            es (ETyApp(i,mk_list_var i "Nil",s)) in
        let mk_qset ds is = match ds,is with
          | [],[] -> mk_core_var i "EPSILON"
          | _ ->
              mk_app3 i (mk_prelude_var i "qset")
                (mk_rx_concats ds) (mk_rx_concats is) in
        let mk_permute ps ls = match ps,ls with
          | [],[] -> mk_core_var i "EMPTY"
          | [0],[l1] -> l1
          | _ ->
              mk_app3 i
                (mk_prelude_var i "lens_permute")
                (mk_list SInteger (Safelist.map (fun x -> EInteger(i,x)) ps))
                (mk_list SLens ls) in

        (* helper: assert that expression e has sort s *)
        let assert_sort e s =
          let e_sort,_ = check_exp sev e in
          if not (compatible e_sort s) then
            static_error (info_of_exp e)
       (fun () -> msg "@[in@ grammar@ atom:@ expected@ %s@ but@ found@ %s@]"
                 (string_of_sort s) (string_of_sort e_sort)) in

        let assert_non_recursive e pxs =
          let overlap = Qid.Set.inter pxs (free_exp_vars e) in
          if not (Qid.Set.is_empty overlap) then

            static_error (info_of_exp e)
       (fun () -> msg "@[in@ grammar@ rule:@ not@ right-recursive in {%s}@]"
                 (Misc.concat_list "," (Safelist.map Qid.string_of_t (Qid.Set.elements overlap)))) in

        (* --- static checks --- *)
        (* collect up production names *)
        let pxs = Safelist.fold_left
          (fun acc (Prod(_,x,_)) -> Qid.Set.add (Qid.t_of_id x) acc)
          Qid.Set.empty ps in

        (* check and tag_atoms as recursive or non-recursive *)
        let rec tag_atoms bindings labels tagged_atoms = function
          | [] ->
              (None,labels,Safelist.rev tagged_atoms)
          | EVar(_,qi)::rest when Qid.Map.mem qi bindings ->
              (* labels *)
              begin match Qid.Map.find qi bindings with
                | EVar(_,qj) when rest = [] && Qid.Set.mem qj pxs ->
                    (* recursive *)
                    (Some qj,Qid.Set.add qi labels,Safelist.rev tagged_atoms)
                | ej ->
                    (* non-recursive *)
                    assert_sort ej SLens;
                    assert_non_recursive ej pxs;
                    tag_atoms bindings (Qid.Set.add qi labels) (Labeled qi::tagged_atoms) rest
              end
          | ei::rest ->
              (* non labels *)
              assert_sort ei SRegexp;
              assert_non_recursive ei pxs;
              tag_atoms bindings labels (Bare ei::tagged_atoms) rest in

        let lensify_rule (Rule(i,xs,ys,bs)) =
          (* compute bindings *)
          let bindings = Safelist.fold_left
            (fun bs (li,ei) ->
               let qli = Qid.t_of_id li in
               if Qid.Map.mem qli bs then
                 static_error i (fun () -> msg "@[lin@ grammar@ rule:@ duplicate@ label@ %s@]" (Qid.string_of_t qli));
               Qid.Map.add qli ei bs)
            Qid.Map.empty bs in

          (* tag atoms *)
          let qo1,ls1,txs = tag_atoms bindings Qid.Set.empty [] xs in
          let qo2,ls2,tys = tag_atoms bindings Qid.Set.empty [] ys in

          (* check labels *)
          if not (Qid.Set.equal ls1 ls2) then
            static_error i
              (fun () ->
                 msg "@[lin@ grammar@ rule:@ both@ sides@ must@ ";
                 msg "reference@ the@ same@ labels: {%s} <> {%s} @]"
                   (Misc.concat_list "," (Safelist.map Qid.string_of_t (Qid.Set.elements ls1)))
                   (Misc.concat_list "," (Safelist.map Qid.string_of_t (Qid.Set.elements ls2))));

          (* compute permutation *)
          let perm_table = Hashtbl.create 8 in
          let pos = ref 1 in
          Safelist.iter
            (fun tyi -> match tyi with
               | Bare _ -> ()
               | Labeled qi ->
                   Hashtbl.add perm_table (Qid.string_of_t qi) !pos;
                   pos := !pos + 2)
            tys;

          let lensified_rule =
            let rec aux j ds is ps ls tas1 tas2 = match tas1,tas2 with
              | (Bare ei)::rest1,_ ->
                  aux j (ei::ds) is ps ls rest1 tas2
              | _,(Bare ei)::rest2 ->
                  aux j ds (ei::is) ps ls tas1 rest2
              | (Labeled qi)::rest1,(Labeled _)::rest2 ->
                  let ei = Qid.Map.find qi bindings in
                  let pi = Hashtbl.find perm_table (Qid.string_of_t qi) in
                  let ps' = pi::j::ps in
                  let ls' = ei::mk_qset (Safelist.rev ds) (Safelist.rev is)::ls in
                  aux (j+2) [] [] ps' ls' rest1 rest2
              | [],[] ->
                  let ps' = Safelist.rev (j::ps) in
                  let ls' = Safelist.rev (mk_qset (Safelist.rev ds) (Safelist.rev is)::ls) in
                  mk_permute ps' ls'
              | _ ->
                  run_error i
             (fun () -> msg "@[in@ rule:@ found@ different@ sets@ of@ labels@]") in
            aux 0 [] [] [] [] txs tys in
          begin match qo1,qo2 with
            | None,None -> NonRecRule(i,lensified_rule)
            | Some q1,Some q2 ->
                if not (Qid.equal q1 q2) then
                  static_error i
                    (fun () ->
                       msg "@[lin@ grammar@ rule:@ both@ sides@ must@ ";
                       msg "be@ recursive@ in@ same@ labels:@ %s@ <>@ %s@]"
                         (Qid.string_of_t q1) (Qid.string_of_t q2));
                RecRule(i,lensified_rule,q1)
            | _ ->
                static_error i (fun () -> msg "@[lin@ grammar@ rule:@ both@ sides@ must@ be@ recursive@]")
          end in

        let ps_map =
          Safelist.fold_left
            (fun acc (Prod(i,x,rs)) ->
               let qx = Qid.t_of_id x in
               let nrs = Safelist.map lensify_rule rs in
               Qid.Map.add qx nrs acc)
            Qid.Map.empty ps in

        (* substitution helpers *)
        let smash_rule i l = function
          | RecRule(_,e1,q1) -> RecRule(i,mk_concat l e1,q1)
          | NonRecRule(_,e1) -> NonRecRule(i,mk_concat l e1) in
        let subst_rules qy rs l = function
          | RecRule(i,e,q) when Qid.equal q qy ->
              NonRecRule(i,mk_concat e l)::(Safelist.map (smash_rule i e) rs)
          | r -> [r] in

        let rec elaborate_full qs qx (non_rec,rec_x,rec_other) = function
          | [] ->
              (rec_other,mk_concat (mk_star (mk_unions rec_x)) (mk_unions non_rec))
          | NonRecRule(_,e)::rest ->
              elaborate_full qs qx (e::non_rec,rec_x,rec_other) rest
          | RecRule(i,e,q)::rest when Qid.equal q qx ->
              elaborate_full qs qx (non_rec,e::rec_x,rec_other) rest
          | (RecRule(i,e,q) as r)::rest when Qid.Set.mem q qs ->
              elaborate_full qs qx (non_rec,rec_x,r::rec_other) rest
          | RecRule(i,e,q)::rest ->
              let rs,l = elaborate_full (Qid.Set.add q qs) q ([],[],[]) (Qid.Map.find q ps_map) in
              let rs' = Safelist.map (smash_rule i e) rs in
              let rest' = Safelist.fold_left (fun acc ri -> subst_rules q rs' l ri@acc) [] rest in
              elaborate_full qs qx (mk_concat e l::non_rec,rec_x,rec_other) (rs'@rest') in

        let elaborate qx =
          match elaborate_full Qid.Set.empty qx ([],[],[]) (Qid.Map.find qx ps_map) with
            | ([],l) -> l
            | _ -> run_error i (fun () -> msg "@[elaboration failed@]") in

        let pre_e0 =
          Safelist.fold_left
            (fun acc (Prod(_,x,_)) ->
               let x_lens = elaborate (Qid.t_of_id x) in
               match acc with
                 | EUnit _ -> x_lens
                 | _ -> EPair(i,x_lens,acc))
            (EUnit i)
            (Safelist.rev ps) in

(*         msg "@[ELABORATED @[%a@]@]" (fun _ -> Bprint.format_exp) pre_e0 *)

        check_exp sev pre_e0

and check_binding ?(in_let=false) sev b0 = match b0 with
  | Bind(i,p,so,e) ->
      let e_sort,new_e = check_exp ~in_let:in_let sev e in        
      let new_s,cast_e = match so with 
        | None -> (e_sort,new_e)
        | Some s -> 
            let new_s = check_sort i sev s in
	      if not (compatible e_sort new_s) then 
                static_error i
                  (fun () ->
                     msg "@[in@ let-binding:@ %s@ expected@ but@ %s@ found@]"
		       (string_of_sort new_s)
		       (string_of_sort e_sort));
	      let cast_e = mk_cast "let binding" (info_of_exp e) e_sort new_s new_e in 
                (new_s,cast_e) in 
      let new_p,xs,bsev = match static_match i sev p new_s with 
        | None -> 
            static_error i 
	      (fun () -> 
                 msg "@[in@ let-binding:@ %s@ does not match@ %s@]"
                   (string_of_pat p)
                   (string_of_sort new_s))
        | Some(new_p,binds) ->             
            let xs_rev,bsev = Safelist.fold_left 
	      (fun (xsi,sevi) (xj,sj) -> 
                 let qj = Qid.t_of_id xj in 
                   (qj::xsi,SCEnv.update sevi qj (G.Sort sj)))
	      ([],sev) binds in
            (new_p,Safelist.rev xs_rev,bsev) in 
      let new_b = Bind(i,new_p,Some new_s,cast_e) in 
        (bsev,xs,new_b)

(* type check a single declaration *)
let rec check_decl sev ms d0 = 
  let res = match d0 with
    | DLet(i,b) ->
	let bsev,xs,Bind(b_i,b_p,b_so,b_e) = check_binding ~in_let:true sev b in
	(* put a positive cast on the bound term *)
	let b_s = match b_so with 
	  | Some b_s -> b_s
	  | None -> run_error i 
	      (fun () -> 
		 msg "@[couldn't@ unpack@ sort@ in@ let-binding@]") in
	let (b_s,cast_be) = mk_pos_cast "dlet" b_i b_s b_e in 
	let cast_b = Bind(b_i,b_p,Some b_s,cast_be) in
	let new_d = DLet(i,cast_b) in
	  (bsev,xs,new_d)
    | DMod(i,n,ds) ->
	let qmn = Qid.t_dot_id (SCEnv.get_mod sev) n in 
	let msev = SCEnv.set_mod (SCEnv.push_ctx sev qmn) qmn in 
	let ms = ms @ [n] in 
	  (* check the module *)
	let msev,names,new_ds = check_module_aux msev ms ds in
	let nsev, names_rev = Safelist.fold_left 
          (fun (nsev, names) q -> 
             match SCEnv.lookup msev q with
		 None -> run_error i
                   (fun () -> msg "@[declaration@ for@ %s@ missing@]"
		      (Qid.string_of_t q))
	       | Some s ->
                   (* prefix the qualifiers in each name with n *)
                   let nq = Qid.id_dot n q in
                     (SCEnv.update nsev nq s, nq::names))
          (msev,[]) names in 
	let new_d = DMod(i,n,new_ds) in 
        let nsev = SCEnv.set_mod (SCEnv.pop_ctx nsev) (Qid.parent_t qmn) in
        (nsev, Safelist.rev names_rev, new_d)

    | DType(i,svl,qx,cl) -> 
	(* get module prefix *)
	let qm = SCEnv.get_mod sev in       
	let new_qx = Qid.t_dot_t qm qx in      
	  (* install a dummy for qx in environment *)
	let sev2 = SCEnv.update_type sev svl new_qx [] in 
	  (* then resolve sorts in cl *)
	let new_cl_rev = 
          Safelist.fold_left  
            (fun acc (x,so) -> 
	       let x_so' = match so with 
		 | None -> (x,so)
		 | Some s -> (x,Some (check_sort i sev2 s)) in 
		 x_so'::acc)
            [] cl in 
	let new_cl = Safelist.rev new_cl_rev in 
	let new_qcl = Safelist.map (fun (x,so) -> (Qid.t_of_id x,so)) new_cl in
	  (* put the real qx in environment -- note! must discard dummy sev2 *)
	let sev3 = SCEnv.update_type sev svl new_qx new_qcl in
	  (* build the datatype *)
	let sx = SData(sl_of_svl svl,new_qx) in
	let mk_univ s = Safelist.fold_right (fun svi acc -> SForall(svi,acc)) svl s in 
	  (* add constructors to sev *)
	let sev4,xs_rev = Safelist.fold_left 
          (fun (sevi,acc) (li,sio) ->            
             let li_sort = match sio with 
	       | None -> mk_univ sx
	       | Some si -> mk_univ (SFunction(Id.wild,si,sx)) in 
             let qli = Qid.t_of_id li in 
	       (SCEnv.update sevi qli (G.Sort li_sort),qli::acc))
          (sev3,[]) new_cl_rev in
	let new_d = DType(i,svl,new_qx,Safelist.rev new_cl_rev) in 
	  (sev4,Safelist.rev xs_rev,new_d)
            
    | DTest(i,e1,tr) -> 
	(* check the expression *)
	let e1_sort,new_e1 = check_exp sev e1 in
	let new_tr = match tr with 
          | TestError | TestPrint -> tr
          | TestEqual e2 -> 
	      (* for values, check that the exps have compatible types *)
	      let e2_sort,new_e2 = check_exp sev e2 in 
	      let e2_i = info_of_exp e2 in
		if not (compatible e2_sort e1_sort) then
		  static_error e2_i
                    (fun () -> 
		       msg "@[in@ type test:@ %s@ expected@ but@ %s@ found@]"
			 (string_of_sort e1_sort)
			 (string_of_sort e2_sort));
		TestEqual new_e2
          | TestSortPrint _ -> TestSortPrint (Some e1_sort)
          | TestSortEqual(_,s2) ->
	      let s2' = check_sort i sev s2 in 
              TestSortEqual(Some e1_sort,s2') in
	let new_d = DTest(i,new_e1,new_tr) in 
	  (sev,[],new_d) in 
  let _,_,new_d0 = res in 
    Trace.debug "check"
      (fun () -> 
	 msg "@[";
	 format_decl d0;
	 msg "@\n~~>@\n";
	 format_decl new_d0;
	 msg "@]@\n");
    res

      
and check_module_aux sev m ds = 
  let msev, names, new_ds_rev = 
    Safelist.fold_left 
      (fun (sevi, names, new_ds_rev) di -> 
         let dsev,new_names,new_di = check_decl sevi m di in
           dsev,names@new_names,new_di::new_ds_rev)
      (sev,[],[]) ds in 
    (msev, names, Safelist.rev new_ds_rev)

(* entry point to static analysis / instrumentation *)
let check_module m0 = match m0 with 
  | Mod(i,m,nctx,ds) -> 
      let qm = Qid.t_of_id m in 
      let sev = SCEnv.set_ctx (SCEnv.empty qm) (qm::nctx@G.pre_ctx) in
      let checked_sev,_,checked_ds = check_module_aux sev [m] ds in 
      let res = Mod(i,m,nctx,checked_ds) in 
	Trace.debug "instr+" (fun () -> format_module res; msg "@\n");
	res

