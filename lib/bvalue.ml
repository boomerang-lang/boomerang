(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2008 J. Nathan Foster and Benjamin C. Pierce                 *)
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
(* /src/bvalue.ml                                                             *)
(* Boomerang run-time values                                                  *)
(* $Id: bvalue.ml 4998 2011-03-16 21:53:34Z mgree $ *)
(******************************************************************************)

(* module imports and abbreviations *)
open Bident
module L = Blenses.MLens
module C = Blenses.Canonizer

type prefs =
  | PrBol of bool Prefs.t
  | PrInt of int Prefs.t
  | PrStr of string Prefs.t
  | PrSLi of string list Prefs.t

let string_of_prefs p =
  match p with
  | PrBol _ -> "bool"
  | PrInt _ -> "int"
  | PrStr _ -> "string"
  | PrSLi _ -> "stringList" 

let format_prefs p = Util.format "<%sPrefs>" (string_of_prefs p)

(* function abbreviations *)
let sprintf = Printf.sprintf 
let (@) = Safelist.append 

(* run-time values; correspond to each sort *)
type t = 
  | Unt of Info.t 
  | Bol of Info.t * string option (* None = true ; 
                                     Some s = false, w/ counterexample s *)
  | Int of Info.t * int
  | Chr of Info.t * char
  | Str of Info.t * string
  | Rx  of Info.t * Brx.t
  | Arx of Info.t * Barx.t
  | Kty of Info.t * Blenses.ktype
  | Mty of Info.t * Blenses.mtype
  | Lns of Info.t * L.t
  | Can of Info.t * C.t
  | Prf of Info.t * prefs
  | Fun of Info.t * (((unit -> t) -> unit) option -> t -> t) (* NB closure takes (optional) workqueue enqueue function *)
  | Par of Info.t * t * t
  | Vnt of Info.t * Qid.t * Id.t * t option

let info_of_t = function
  | Unt(i)       -> i
  | Int(i,_)     -> i
  | Bol(i,_)     -> i
  | Chr(i,_)     -> i
  | Str(i,_)     -> i
  |  Rx(i,_)     -> i
  | Arx(i,_)     -> i
  | Kty(i,_)     -> i
  | Mty(i,_)     -> i
  | Lns(i,_)     -> i
  | Can(i,_)     -> i
  | Prf(i,_)     -> i
  | Fun(i,_)     -> i
  | Par(i,_,_)   -> i
  | Vnt(i,_,_,_) -> i         

let rec equal v1 v2 = match v1,v2 with
  | Unt _, Unt _ -> 
      true
  | Bol(_,b1), Bol(_,b2) -> 
      begin match b1,b2 with
	| None,None -> true
	| Some _, Some _ -> true
	| _ -> false
      end
  | Int(_,n1), Int(_,n2) -> 
      n1=n2
  | Chr(_,c1), Chr(_,c2) -> 
      c1=c2
  | Chr(_,c), Str(_,s) | Str(_,s), Chr(_,c) -> 
      (String.make 1 c) = s
  | Chr(_,c), Rx(_,r) | Rx(_,r), Chr(_,c) -> 
      Brx.equiv (Brx.mk_string (String.make 1 c)) r
  | Str(_,s1), Str(_,s2) -> 
      s1 = s2  
  | Str(_,s), Rx(_,r) | Rx(_,r), Str(_,s) -> 
      Brx.equiv (Brx.mk_string s) r
  | Rx(_,r1), Rx(_,r2) -> 
      Brx.equiv r1 r2
  | Arx(_,r1), Arx(_,r2) -> 
      Barx.equiv r1 r2
  | Kty(_,k1), Kty(_,k2) ->
      Blenses.ktype_equiv k1 k2
  | Mty(_,m1), Mty(_,m2) ->
      Blenses.mtype_equiv m1 m2
  | Lns _, Lns _ -> 
      Error.simple_error (sprintf "Cannot test equality of lenses.")
  | Can _, Can _ ->
      Error.simple_error (sprintf "Cannot test equality of canonizers.")
  | Fun _, Fun _ -> 
      Error.simple_error (sprintf "Cannot test equality of functions.")
  | Par(_,v1,v2),Par(_,v1',v2') -> 
      (equal v1 v1') && (equal v2 v2')
  | Vnt(_,qx,l,None), Vnt(_,qx',l',None) -> 
      Qid.equal qx qx' && Id.equal l l'
  | Vnt(_,qx,l,Some v), Vnt(_,qx',l',Some v') ->
      (Qid.equal qx qx') && (Id.equal l l') && (equal v v')
  | Vnt _,Vnt _ -> false
  | _, _ -> 
      format v1; Util.format "@\n"; 
      format v2; Util.format "@\n";
      Error.simple_error (sprintf "Cannot test equality of values with different sorts.")

and format = function
  | Unt(_)       -> Util.format "()"
  | Int(_,n)     -> Util.format "%d" n
  | Bol(_,None)  -> Util.format "true"
  | Bol(_,Some "") ->
      Util.format "false"
  | Bol(_,Some s) -> 
      Util.format "false (with counterexample: %s)" s
  | Chr(_,c)     -> Util.format "'%s'" (Char.escaped c)
  | Str(_,rs)    -> Util.format "\"%s\"" rs
  | Rx(_,r)      -> Brx.format_t r
  | Arx(_,a)     -> Barx.format_t a
  | Kty(_,k)     -> Blenses.format_ktype k
  | Mty(_,m)     -> Blenses.format_mtype m
  | Lns(_,l)     -> L.format_t l
  | Can(_,c)     -> C.format_t c
  | Prf(_,p)     -> format_prefs p
  | Fun(_,f)     -> Util.format "<function>"
  | Par(_,v1,v2) -> 
      Util.format "@[(";
      format v1;
      Util.format ",@ ";
      format v2;
      Util.format ")@]"
  | Vnt(_,_,l,None) -> Util.format "%s" (Id.string_of_t l)
  | Vnt(_,_,l,Some v) ->  
      Util.format "@[(%s@ " (Id.string_of_t l);
      format v;
      Util.format ")@]"        

let string_of_t v = Util.format_to_string (fun () -> format v)
        
let rec sort_string_of_t = function
  | Unt _ -> "unit"
  | Bol _ -> "bool" 
  | Int _ -> "int" 
  | Chr _ -> "char"
  | Str _ -> "string"
  | Rx _  -> "regexp"
  | Arx _ -> "aregexp"
  | Kty _ -> "skeleton_set"
  | Mty _ -> "resource_set"
  | Lns _ -> "lens"
  | Can _ -> "canonizer"
  | Prf (_, p) -> string_of_prefs p ^ "Prefs"
  | Fun _ -> "<function>"
  | Par(_,v1,v2) -> sprintf "(%s,%s)" (sort_string_of_t v1) (sort_string_of_t v2)
  | Vnt(_,qx,_,_) -> sprintf "%s" (Qid.string_of_t qx)

(* --------- conversions between run-time values ---------- *)
let conversion_error s1 v1 = 
  Error.simple_error 
    (sprintf "%s: Conversion error; expected %s, but found %s" 
        (Info.string_of_t (info_of_t v1)) 
        s1
        (string_of_t v1))

let get_u v = match v with
  | Unt(_) -> ()
  | _ -> conversion_error "unit" v

let get_b v = match v with 
  | Bol(_,None) -> true
  | Bol(_,Some _) -> false
  | _ -> conversion_error "boolean" v

let get_x v = match v with
  | Bol(_,None) -> None
  | Bol(_,Some s) -> Some s
  | _ -> conversion_error "boolean" v

let get_i v = match v with
  | Int(_,n) -> n
  | _ -> conversion_error "integer" v

let get_c v = match v with
  | Chr(b,c) -> c
  | _ -> conversion_error "char" v

let get_s v = match v with
  | Str(_,s) -> s
  | _ -> conversion_error "string" v

let get_r v = match v with
    Rx(_,r)  -> r
  | _ -> conversion_error "regexp" v
      
let get_a v = match v with
    Arx(_,a)  -> a
  | _ -> conversion_error "aregexp" v
      
let get_k v = match v with
    Kty(_,k)  -> k
  | _ -> conversion_error "skeleton_set" v
      
let get_m v = match v with
    Mty(_,m)  -> m
  | _ -> conversion_error "resource_set" v
      
let get_l v = match v with 
  | Lns(_,l) -> l
  | _ -> conversion_error "lens" v

let get_q v = match v with
  | Can(_,q) -> q
  | _ -> conversion_error "canonizer" v

let get_bP v = match v with
  | Prf(_, PrBol bp) -> bp
  | _ -> conversion_error "boolPrefs" v

let get_iP v = match v with
  | Prf(_, PrInt ip) -> ip
  | _ -> conversion_error "intPrefs" v

let get_sP v = match v with
  | Prf(_, PrStr sp) -> sp
  | _ -> conversion_error "stringPrefs" v

let get_szP v = match v with
  | Prf(_, PrSLi szp) -> szp
  | _ -> conversion_error "stringListPrefs" v

let get_p v = match v with
  | Par(_,v1,v2) -> (v1,v2)
  | _ -> conversion_error "pair" v

let get_v v = match v with
  | Vnt(_,_,l,v) -> (l,v)
  | _ -> conversion_error "variant" v

let get_f v = match v with
  | Fun(_,f) -> f
  | _ -> conversion_error "function" v

(* --------- constructors for functions on run-time values ---------- *)
let mk_u i u = Unt(i)
let mk_b i b = if b then Bol(i,None) else Bol(i,Some "")
let mk_x i x = Bol(i,x)
let mk_i i n = Int(i,n)
let mk_c i c = Chr(i,c)
let mk_s i s = Str(i,s)
let mk_r i r = Rx(i,r)
let mk_a i a = Arx(i,a)
let mk_k i k = Kty(i,k)
let mk_m i m = Mty(i,m)
let mk_l i l = Lns(i,l)
let mk_q i q = Can(i,q)
let mk_bP i bp = Prf(i,PrBol bp)
let mk_iP i ip = Prf(i,PrInt ip)
let mk_sP i sp = Prf(i,PrStr sp)
let mk_szP i szp = Prf(i,PrSLi szp)
let mk_p i (p1,p2) = Par(i,p1,p2)
let mk_f i f = Fun(i,fun wq v -> f v)

let mk_ufun i f = Fun(i,(fun wq v -> f (get_u v)))
let mk_bfun i f = Fun(i,(fun wq v -> f (get_b v)))
let mk_xfun i f = Fun(i,(fun wq v -> f (get_x v)))
let mk_ifun i f = Fun(i,(fun wq v -> f (get_i v)))
let mk_cfun i f = Fun(i,(fun wq v -> f (get_c v)))
let mk_sfun i f = Fun(i,(fun wq v -> f (get_s v)))
let mk_rfun i f = Fun(i,(fun wq v -> f (get_r v)))
let mk_afun i f = Fun(i,(fun wq v -> f (get_a v)))
let mk_kfun i f = Fun(i,(fun wq v -> f (get_k v)))
let mk_mfun i f = Fun(i,(fun wq v -> f (get_m v)))
let mk_lfun i f = Fun(i,(fun wq v -> f (get_l v)))
let mk_qfun i f = Fun(i,(fun wq v -> f (get_q v)))
let mk_bPfun i f = Fun(i,(fun wq v -> f (get_bP v)))
let mk_iPfun i f = Fun(i,(fun wq v -> f (get_iP v)))
let mk_sPfun i f = Fun(i,(fun wq v -> f (get_sP v)))
let mk_szPfun i f = Fun(i,(fun wq v -> f (get_szP v)))
let mk_pfun i f = Fun(i,(fun wq v -> f (get_p v)))
let mk_vfun i f = Fun(i,(fun wq v -> f (get_v v)))
let mk_ffun i f = Fun(i,(fun wq v -> f (get_f v wq)))

let string_of_t v = Util.format_to_string (fun () -> format v)

(* list utilities *)
let nil = Id.mk (Info.M "Nil built-in") "Nil"
let cons = Id.mk (Info.M "Cons built-in") "Cons"
let list_qid =
  let i = Info.M "List.t built-in" in
  Qid.mk [Id.mk i "List"] (Id.mk i "t")
 
let get_list v = 
  let rec aux v acc = 
    let li,vio = get_v v in 
    if Id.equal li nil then Safelist.rev acc 
    else if Id.equal li cons then 
      let vh,vrest = match vio with 
        | None -> conversion_error "list" v
        | Some vi -> get_p vi in
      aux vrest (vh::acc)
    else conversion_error "list" v in 
  aux v []

let mk_list i l = 
  let rec aux l v = match l with
    | [] -> v 
    | h::rest -> aux rest (Vnt(i,list_qid,cons,Some (mk_p i (h,v)))) in 
  aux (Safelist.rev l) (Vnt(i,list_qid,nil,None))
        

let mk_listfun i f = 
  Fun(i,(fun wq v -> f (get_list v)))

(* option utilities *)
let none = Id.mk (Info.M "None built-in") "None"
let some = Id.mk (Info.M "Some built-in") "Some"
let option_qid =
  let i = Info.M "option built-in" in
  Qid.mk [Id.mk i "Prelude"] (Id.mk i "option")
 
let get_option v =
  let i, xo = get_v v in
  match i with
  | _ when Id.equal i none -> None
  | _ when Id.equal i some ->
      (match xo with
       | None -> conversion_error "option" v
       | Some x -> Some x
      )
  | _ -> conversion_error "option" v

let mk_option i xo =
  match xo with
  | None -> Vnt (i, option_qid, none, None)
  | Some x -> Vnt (i, option_qid, some, Some x)

let mk_optionfun i f = 
  Fun (i, (fun wq v -> f (get_option v)))

(* species utilities *)
let positional = Id.mk (Info.M "Positional built-in") "Positional"
let diffy = Id.mk (Info.M "Diffy built-in") "Diffy"
let greedy = Id.mk (Info.M "Greedy built-in") "Greedy"
let setlike = Id.mk (Info.M "Setlike built-in") "Setlike"
let species_qid =
  let i = Info.M "species built-in" in
  Qid.mk [Id.mk i "Core"] (Id.mk i "species")
 
let get_species v =
  match get_v v with
  | i,None   when Id.equal i positional -> Btag.Positional
  | i,Some x when Id.equal i diffy      -> Btag.Diffy (get_b x)
  | i,None   when Id.equal i greedy     -> Btag.Greedy
  | i,None   when Id.equal i setlike    -> Btag.Setlike
  | _ -> conversion_error "species" v

let mk_species i sp =
  match sp with
  | Btag.Positional -> Vnt (i, species_qid, positional, None)
  | Btag.Diffy b -> Vnt (i, species_qid, diffy, Some (mk_b i b))
  | Btag.Greedy -> Vnt (i, species_qid, greedy, None)
  | Btag.Setlike -> Vnt (i, species_qid, setlike, None)

let mk_speciesfun i f =
  Fun (i, (fun wq v -> f (get_species v)))

(* predicate utilities *)
let threshold = Id.mk (Info.M "Threshold built-in") "Threshold"
let predicate_qid =
  let i = Info.M "predicate built-in" in
  Qid.mk [Id.mk i "Core"] (Id.mk i "predicate")
 
let get_predicate v =
  let i, xo = get_v v in
  match i with
  | _ when Id.equal i threshold ->
      (match xo with
       | None -> conversion_error "predicate" v
       | Some x -> Btag.Threshold (get_i x)
      )
  | _ -> conversion_error "predicate" v

let mk_predicate i p =
  match p with
  | Btag.Threshold t -> Vnt (i, predicate_qid, threshold, Some (mk_i i t))

let mk_predicatefun i f =
  Fun (i, (fun wq v -> f (get_predicate v)))

(* default key utilities (default weight value for the chunk) *)
let key = Id.mk (Info.M "Key built-in") "Key"
let nokey = Id.mk (Info.M "NoKey built-in") "NoKey"
let keyannot_qid =
  let i = Info.M "key_annotation built-in" in
  Qid.mk [Id.mk i "Core"] (Id.mk i "key_annotation")
 
let get_key v =
  match get_v v with
  | i, None when Id.equal i nokey -> Bannot.Weight.zero
  | i, None when Id.equal i key   -> Bannot.Weight.one
  | _ -> conversion_error "key_annotation" v

let mk_key i w =
  match Bannot.Weight.to_int w with
    | 0 -> Vnt (i, keyannot_qid, nokey, None)
    | 1 -> Vnt (i, keyannot_qid, key, None)
    | _ -> assert false

let mk_keyfun i f =
  Fun (i, (fun wq v -> f (get_key v)))

(* tag utilities *)
let tag = Id.mk (Info.M "Tag built-in") "Tag"
let tag_qid =
  let i = Info.M "tag built-in" in
  Qid.mk [Id.mk i "Core"] (Id.mk i "tag")
 
let get_tag v =
  let i, xo = get_v v in
  match i with
  | _ when Id.equal i tag ->
      (match xo with
       | None -> conversion_error "tag" v
       | Some x ->
           let x, name = get_p x in
           let x, keyannot = get_p x in
           let species, predicate = get_p x in
           Btag.of_elements (get_species species) [get_predicate predicate] (get_key keyannot) (get_s name)
      )
  | _ -> conversion_error "tag" v

let mk_tag i t =
  let species, predicates, weight, name = Btag.to_elements t in
  let predicate =
    match predicates with
    | [] -> Btag.Threshold 0
    | [p] -> p
    | _ -> assert false
  in
  Vnt (i, tag_qid, tag, Some (mk_p i (mk_p i (mk_p i (mk_species i species, mk_predicate i  predicate), mk_key i weight), mk_s i name)))

let mk_tagfun i f =
  Fun (i, (fun wq v -> f (get_tag v)))

