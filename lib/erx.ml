(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2008                                                         *)
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
(* /src/erx.ml                                                                *)
(* Boomerang extended regular expressions (for sync)                          *)
(* $Id *)
(******************************************************************************)

module RxImpl = Brx

let msg = Util.format

type tag = string 

module TagMapPlus = Mapplus.Make(
  struct
    type t = tag
    let compare (s1:tag) s2 = Pervasives.compare s1 s2
  end)
module TM = TagMapPlus.Map
module TS = TagMapPlus.KeySet
module TagMap = TM
module TagSet = TS
              
type u =
  | Box of tag * t
  | BoxStar of tag * string * t * string
  | Seq of t * t
  | Alt of t * t
  | Star of t 
  | Key
  | Leaf
and t = 
    { desc:u;
      bare:RxImpl.t;
      boxes:int;
      has_key:bool }

let mk u r bs hk = { desc=u; bare=r; boxes=bs; has_key=hk }

let rank t = match t.desc with
  | Leaf   -> RxImpl.rank t.bare
  | Key    -> RxImpl.rank t.bare
  | Star _ -> RxImpl.Srnk
  | Seq _  -> RxImpl.Crnk
  | Alt _  -> RxImpl.Urnk
  | Box _ | BoxStar _ -> RxImpl.Arnk
let rec format_t t1 = match t1.desc with 
  | Box(tg,t11) -> 
      msg "@[%s%s" "<" (if tg = "" then "" else (Misc.whack tg) ^ ":");
      format_t t11;
      msg "%s@]" ">" 
  | BoxStar(tg,w1,t11,w2) -> 
      msg "@[%s%s%s" "<" (if tg = "" then "" else tg ^ ":") w1;
      format_t t11;
      msg "%s%s*@]" w2 ">" 
  | Seq(t11,t12) ->
      msg "@[";
      Bprint.maybe_wrap format_t (RxImpl.lpar (rank t11) (rank t1)) t11;
      msg ".@,";
      Bprint.maybe_wrap format_t (RxImpl.rpar (rank t12) (rank t1)) t12;
      msg "@]"
  | Alt(t11,t12) ->
      msg "@[";
      Bprint.maybe_wrap format_t (RxImpl.lpar (rank t11) (rank t1)) t11;
      msg "@,|";
      Bprint.maybe_wrap format_t (RxImpl.rpar (rank t12) (rank t1)) t12;
      msg "@]"
  | Star(t11) -> 
      msg "@[";
      Bprint.maybe_wrap format_t (RxImpl.lpar (rank t11) (rank t1)) t11;
      msg "*@]"
  | Key | Leaf -> 
      msg "@[%s" (if t1.desc = Key then "key " else "");
      RxImpl.format_t t1.bare;
      msg "@]"

let string_of_t t = 
  Util.format_to_string (fun () -> format_t t)

type spine_elt =
  | SBox of tag
  | SBoxStar of string * tag * string
  | SString of string

type spine = spine_elt list

let format_spine_elt = function
  | SBox(tg) -> msg "@[%s%s%s@]" "<" tg ">"
  | SBoxStar(w1,tg,w2) -> msg "@[%s%s:%s.%s%s*@]" "<" tg w1 w2 ">"
  | SString(w) -> msg "@[%s@]" (Misc.whack w)

let format_spine sp = 
  msg "@[[";
  Misc.format_list " " format_spine_elt sp;
  msg "]@]"
let string_of_spine sp = 
  Util.format_to_string (fun () -> format_spine sp)
   
type key = string
               
type box_content = (key * string) list
                 
let format_box_content bc = 
  msg "@[[";
  Misc.format_list ", " 
    (fun (k,w) -> msg "%s = %s" (Misc.whack k) (Misc.whack w))
    bc;
  msg "]@]"
let string_of_box_content bc = 
  Util.format_to_string (fun () -> format_box_content bc)

type skeleton = spine * box_content TM.t

let format_tag_map tm = 
  ignore 
    (TM.fold 
       (fun t bc is_fst -> 
          if not is_fst then msg ",";
          msg "%s -> " (Misc.whack t);
          format_box_content bc;
          false)
       tm true)

let format_skeleton (sp,tm) = 
  msg "@[";
  msg "skel(";
  format_spine sp;
  msg ", {";
  format_tag_map tm;
  msg "})@]"
let string_of_skeleton sk = 
  Util.format_to_string (fun () -> format_skeleton sk)
    
let rec boxes t = t.boxes

let rec has_key t = t.has_key

let rec flatten t = match t.desc with
  | Box(tg,t1) -> 
      Some ("",Some(tg,t1,""))
  | Key | Leaf -> 
      let r = t.bare in 
      if RxImpl.is_singleton r then         
        let w = match RxImpl.representative r with 
          | None -> assert false 
          | Some w -> w in 
        Some (w,None)
      else None
  | Seq(t1,t2) -> 
      begin match flatten t1,flatten t2 with
        | Some(w11,Some(tg,t11,w12)),Some(w2,None) -> 
            Some(w11,Some (tg,t11,w12 ^ w2))
        | Some(w1,None),Some(w21,Some(tg,t21,w22)) -> 
            Some(w1 ^ w21,Some(tg,t21,w22))
        | Some(w1,None),Some(w2,None) -> 
            Some(w1 ^ w2,None)
        | _ -> None end
  | Alt(t1,t2) -> None
  | BoxStar(_) -> None
  | Star(t1) -> None

(* lifted operations ... *)
let iterable t1 = 
  match flatten t1 with 
    | Some _ -> true
    | None -> t1.boxes = 0

let mk_star t1 = 
  let u,bs,hk = match flatten t1 with
    | Some(w1,Some(tg,t11,w2)) ->
        (BoxStar(tg,w1,t11,w2),t11.boxes + 1,false)
    | _ when t1.boxes <> 0 -> 
        Berror.run_error (Info.M "mk_star") 
          (fun () -> msg "@[mk_star: %s contains a box, but cannot be flattened@]" 
             (string_of_t t1))
    | _ when t1.has_key -> (Star(t1),0,true)
    | _ -> (Leaf,0,false) in
  let r = RxImpl.mk_star t1.bare in
  mk u r bs hk 

let mk_seq t1 t2 = 
  let bs = t1.boxes + t2.boxes in 
  let hk = t1.has_key || t2.has_key in
  let u = if bs <> 0 || hk then Seq(t1,t2) else Leaf in 
  let r = RxImpl.mk_seq t1.bare t2.bare in 
  mk u r bs hk

let mk_alt t1 t2 = 
  let bs = t1.boxes + t2.boxes in 
  let hk = t1.has_key || t2.has_key in
  let u = if bs <> 0 || hk then Alt(t1,t2) else Leaf in 
  let r = RxImpl.mk_alt t1.bare t2.bare in 
  mk u r bs hk

let mk_key r1 = 
  let bs = 0 in 
  let hk = true in 
  let u = Key in 
  let r = r1 in 
  mk u r bs hk

let mk_leaf r1 = 
  let bs = 0 in 
  let hk = false in 
  let u = Leaf in 
  let r = r1 in 
  mk u r bs hk
  
let mk_box tg t1 = 
  let bs = 1 in 
  let hk = false in 
  let u = Box(tg,t1) in 
  let r = t1.bare in 
  mk u r bs hk

let bare t = t.bare
let lift1 f t = f (bare t)

let match_string = lift1 RxImpl.match_string 
let star_split = lift1 RxImpl.star_split

let seq_split t1 t2 w = 
  match RxImpl.seq_split t1.bare t2.bare w with 
    | None -> 
        Berror.run_error (Info.M "Erx.seq_split")
          (fun () -> msg "@[the concatenation of %s and %s is ambiguous@]" 
             (RxImpl.string_of_t t1.bare) (RxImpl.string_of_t t2.bare))
    | Some p -> p

(* operations *)
let rec key t w = match t.desc with
  | Box(_)       -> ""
  | BoxStar(_)   -> ""
  | Key          -> w
  | Seq(t1,t2)   ->
      let w1,w2 = seq_split t1 t2 w in
      key t1 w1 ^ key t2 w2
  | Alt(t1,t2)   ->
      if match_string t1 w then key t1 w
      else key t2 w
  | Leaf         -> ""
  | Star(t1)      -> 
      Safelist.fold_left 
        (fun acc wi -> acc ^ key t1 wi) 
        "" (star_split t1 w)
    
(* LATER: this contains many horrible appends/combines... optimize *)        
let rec parse t w = match t.desc with
    | Box(tg,t1) ->        
      let k = key t1 w in
      ([SBox tg], TM.add tg [k,w] TM.empty)
    | BoxStar(tg,w1,t1,w2) ->
        let n1 = String.length w1 in 
        let n2 = String.length w2 in 
        let l1 = mk_leaf (RxImpl.mk_string w1) in 
        let l2 = mk_leaf (RxImpl.mk_string w2) in 
        let pad_t1 = mk_seq l1 (mk_seq t1 l2) in 
        let wl = star_split pad_t1 w in 
        let tm =
          Safelist.fold_left
            (fun tmacc wi ->
               let unpad_wi = String.sub wi n1 (String.length wi - n1 - n2) in 
               let ki = key t1 unpad_wi in 
               let tm_tg = TM.safe_find tg tmacc [] in
               TM.add tg (tm_tg@[ki,unpad_wi]) tmacc)
            TM.empty wl in
        ([SBoxStar(w1,tg,w2)],tm)                
    | Key | Leaf | Star _ -> 
       ([SString w],TM.empty)
    | Seq(t1,t2) ->
        let w1,w2 = seq_split t1 t2 w in
        let sp1,tm1 = parse t1 w1 in
        let sp2,tm2 = parse t2 w2 in
        let tm12 = 
          TM.fold 
            (fun tgi bci acc ->                
               let tm1_tgi = TM.safe_find tgi acc [] in                  
               TM.add tgi (tm1_tgi @ bci) acc)
            tm2 tm1 in           
          (sp1@sp2,tm12)

    | Alt(t1,t2) ->
        if match_string t1 w then parse t1 w
        else parse t2 w

let box_map_of_spine = 
  let rec aux acc = function
  | [] -> acc 
  | SBox(tg)::rest -> 
      let (i,jo) = TM.safe_find tg acc (0,Some 0) in 
      let acc' = TM.add tg (succ i, Misc.map_option succ jo) acc in
      aux acc' rest      
  | SBoxStar(_,tg,_)::rest -> 
      let (i,jo) = TM.safe_find tg acc (0,Some 0) in 
      let acc' = TM.add tg (i,None) acc in
      aux acc' rest
  | _::rest -> aux acc rest in 
  aux TM.empty         

let unparse_spine_elt tm bm spi = 
  let err tg = 
    Berror.run_error (Info.M "unparse_spine_elt") 
      (fun () -> msg "@[wrong number of boxes for %s@]" tg) in 
  let get tm tg n = 
    let rec aux acc i l =
      if i=0 then (Safelist.rev acc,l)
      else match l with 
        | [] -> err tg
        | h::t -> aux (h::acc) (pred i) t in 
    let l,tm_rest = aux [] n (TM.safe_find tg tm []) in
    (l,TM.add tg tm_rest tm) in 
     
  match spi with
  | SBox(tg) ->
      begin match get tm tg 1 with
        | [_,w],tm -> 
            let tg_save_min,tg_save_maxo = TM.safe_find tg bm (0,Some 0) in
            (w,tm,TM.add tg (pred tg_save_min,Misc.map_option pred tg_save_maxo) bm)
        | _ -> err tg end
        
  | SBoxStar(w1,tg,w2) ->      
      let (tg_save_min,tj_save_maxo) = TM.safe_find tg bm (0,Some 0) in 
      let tg_have = Safelist.length (TM.safe_find tg tm []) in
      let tg_get = tg_have-tg_save_min in 
      let () = if tg_get < 0 then err tg in
      let wl,rest = get tm tg tg_get in
      let w = Misc.concat_list "" 
        (Safelist.map (fun (_,wi) -> w1 ^ wi ^ w2) wl) in 
      (w,rest,bm)

  | SString(w) -> (w,tm,bm)
    
let unparse sk = 
  let sp,tm = sk in 
  let w,tm',_ = 
    Safelist.fold_left 
      (fun (acc,tmi,bmi) spi -> 
         let wi,tmj,bmj = unparse_spine_elt tmi bmi spi in 
         (acc ^ wi,tmj,bmj)) 
      ("",tm,box_map_of_spine sp) sp in 
  let () = 
    TM.iter 
      (fun tg kl ->
         if kl <> [] then 
           Berror.run_error (Info.M "unparse") 
             (fun () -> msg "@[wrong number of chunks for %s: %s@]" tg
                (Misc.concat_f_list "," (fun (k,w) -> k ^ "=" ^ w) kl)))
      tm' in 
  w
 
let valid sk = 
  let sp,tm = sk in 
  let bm = box_map_of_spine sp in 
  TM.fold 
    (fun tg bc ok -> 
       ok &&
       let i,jo = TM.safe_find tg bm (0,Some 0) in
       let n = Safelist.length bc in 
       i <= n && 
       (match jo with None -> true | Some j -> n <= j))
    tm true
     
let box_content (_,tm) tg =
  TM.safe_find tg tm []

let rec box_type t0 tg = match t0.desc with
 | Box(tg',t1) | BoxStar(tg',_,t1,_) ->
     if tg = tg' then Some t1
     else None
 | Key | Leaf | Star _ -> None
 | Seq(t1,t2) ->
     (match box_type t1 tg,box_type t2 tg with
     | Some t,None | None, Some t -> Some t
     | Some t,Some _ -> Some t
     | _ -> None)
 | Alt(t1,t2) ->
     (match box_type t1 tg,box_type t2 tg with
     | Some t,_ | _, Some t -> Some t
     | _ -> None)

let spine_tags sp =
  Safelist.fold_left
    (fun acc spi -> match spi with
    | SBox(tg) | SBoxStar(_,tg,_) -> TS.add tg acc
    | _ -> acc)
    TS.empty sp



               
