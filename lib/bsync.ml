(*****************************************************************************)
(* The Harmony Project                                                       *)
(* harmony@lists.seas.upenn.edu                                              *)
(*****************************************************************************)
(* Copyright (C) 2008 J. Nathan Foster and Benjamin C. Pierce                *)
(*                                                                           *)
(* This library is free software; you can redistribute it and/or             *)
(* modify it under the terms of the GNU Lesser General Public                *)
(* License as published by the Free Software Foundation; either              *)
(* version 2.1 of the License, or (at your option) any later version.        *)
(*                                                                           *)
(* This library is distributed in the hope that it will be useful,           *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU         *)
(* Lesser General Public License for more details.                           *)
(*****************************************************************************)
(* /src/bsync.ml                                                             *)
(* Boomerang synchronization                                                 *)
(* $Id: bsync.ml 4607 2009-08-03 16:53:28Z ddavi $ *)
(*****************************************************************************)

(* --- imports --- *)
let msg = Util.format
let sprintf = Printf.sprintf 

module Diff3 = Bdiff3.Make(
  struct
    type elt = string * string
    let equal (k1,_) (k2,_) = k1 = k2
  end)
  
let debug thk = Trace.debug "sync+" thk  
let report thk = thk ()

type act = Added | Deleted 

type dir = A | B | Both

let strings_of_dir = function
  | A  -> "the archive and replica B", "A"
  | B -> "the archive and replica A", "B"
  | Both  -> "replicas A and B", "both"   

type path_elt = Tag of Erx.tag | Key of Erx.key 

let string_of_path_elt = function
  | Tag t -> if t <> "" then ":" ^ (Misc.whack t) else (Misc.whack t)
  | Key k -> "/" ^ (Misc.whack k)
let root = []

let string_of_path p = 
  if p = [] then "." 
  else Safelist.fold_left (fun acc pi -> string_of_path_elt pi ^ acc) "" p

let keys_string kl = Misc.concat_list "," (Safelist.map (fun (ki,_) -> Misc.whack ki) kl)
let skel_string sk = Erx.string_of_skeleton sk
let tags_string ts = Misc.concat_list "," (Erx.TagSet.fold (fun ti acc -> Misc.whack ti::acc) ts [])

let string_of_dir = function
  | A     -> "A    "
  | B     -> "B    "
  | Both  -> "A & B" 

let string_of_act = function
  | Added   -> "added  "
  | Deleted -> "deleted"

(* reporting *)
let report_spine_conflict buf spo spa spb p = 
  Buffer.add_string buf 
    (sprintf "Conflict on spines [%s] [%s] [%s] at [%s]\n" 
       (Erx.string_of_spine spo) (Erx.string_of_spine spa) (Erx.string_of_spine spb) (string_of_path p))

let report_tags_action buf r a ts p = 
  Erx.TagSet.iter 
    (fun ti -> Buffer.add_string buf 
       (sprintf "%s %s tag at [%s]\n" 
          (string_of_dir r) (string_of_act a) (string_of_path (Tag ti::p)))) 
    ts

let report_tags_del_mod buf r rb tso tsr tsrb p = 
  Buffer.add_string buf 
    (sprintf "Delete(%s) / modify (%s) conflict on tags {%s} {%s} {%s} at [%s].\n"
       r rb (tags_string tso) (tags_string tsr) (tags_string tsrb) (string_of_path p))

let report_keys_action buf r a kl p = 
  Safelist.iter
    (fun (ki,_) -> 
       Buffer.add_string buf
         (sprintf "%s %s key at [%s]\n" 
            (string_of_dir r) (string_of_act a) (string_of_path (Key ki::p))))
    kl

let report_keys_del_mod buf r rb okl rkl rbkl p = 
  Buffer.add_string buf
    (sprintf "Delete(%s) / modify (%s) conflict on keys [%s] [%s] [%s] at [%s].\n"
       r rb (keys_string okl) (keys_string rkl) (keys_string rbkl) (string_of_path p))

let report_keys_conflict buf okl akl bkl p = 
  Buffer.add_string buf 
    (sprintf "Conflict on keys [%s] [%s] [%s] at [%s].\n"
       (keys_string okl) (keys_string akl) (keys_string bkl) (string_of_path p))

let report_schema_conflict buf okl akl bkl p = 
  Buffer.add_string buf 
    (sprintf "Schema conflict on skeletons [%s] [%s] [%s] at [%s].\n"
       (skel_string okl) (skel_string akl) (skel_string bkl) (string_of_path p))

let atomic_sync o a b = 
  if a=b then Some(a,Both)
  else if o=b then Some(a,A)
  else if o=a then Some(b,B)
  else None

let isync buf p ty o a b = 
  let rec isync_aux p ty o a b = 
    let (spo,tmo) as sko = Erx.parse ty o in 
    let (spa,tma) as ska = Erx.parse ty a in
    let (spb,tmb) as skb = Erx.parse ty b in 
    let o',a',b' = 
      begin match atomic_sync spo spa spb with
        | None -> 
            report_spine_conflict buf spo spa spb p;
            (o,a,b)
        | Some(s,dir) -> begin
            let s_tags = Erx.spine_tags s in
            let o_tags = Erx.spine_tags spo in 
            let a_tags = Erx.spine_tags spa in 
            let b_tags = Erx.spine_tags spb in 
            let r,r_tags,tmr,rb,rb_tags,tmrb = 
              if dir = A then "A",a_tags,tma,"B",b_tags,tmb
              else "B",b_tags,tmb,"A",a_tags,tma in
              (* compute deletes / modified tags *)
            let del_tags = Erx.TagSet.diff rb_tags s_tags in  
            let add_tags = Erx.TagSet.diff r_tags s_tags in 
            let mod_tags = 
              Erx.TagSet.filter 
                (fun ti -> Erx.TagMap.safe_find ti tmrb [] <> Erx.TagMap.safe_find ti tmo [])
                rb_tags in
            let del_mod_tags = Erx.TagSet.inter del_tags mod_tags in 
              (* check for delete / modify conflict *)
              if not (Erx.TagSet.is_empty del_mod_tags) then 
                begin 
                  report_tags_del_mod buf r rb o_tags r_tags rb_tags p;
                  (o,a,b)
                end
              else
                begin                
                  report_tags_action buf dir Deleted del_tags p;
                  report_tags_action buf dir Added add_tags p;
                  let tmo,tma,tmb =
                  Erx.TagSet.fold
                    (fun t (tmo,tma,tmb) ->
                       let pt = Tag t::p in 
                       let ot = Erx.box_content sko t in 
                       let at = Erx.box_content ska t in 
                       let bt = Erx.box_content skb t in 
                       let chunks = Diff3.parse ot at bt in
                       let ot',at',bt' =
                         Safelist.fold_left
                           (fun (ot',at',bt') chunk ->
                              match chunk with
                                | Diff3.Stable((k,oi),(_,ai),(_,bi)) -> 
                                    if false then Buffer.add_string buf (sprintf "STABLE %s at [%s]\n" (Misc.whack k) (string_of_path pt));                                    
                                    let ty_t = match Erx.box_type ty t with 
                                      | None -> assert false
                                      | Some ty' -> ty' in 
                                    let oi',ai',bi' = isync_aux (Key k::pt) ty_t oi ai bi in
                                      (ot'@[k,oi'],at'@[k,ai'],bt'@[k,bi'])
                                | Diff3.AChange(oti,ati,bti) ->
(*                                     if false then Buffer.add_string buf (sprintf "ACHANGE [%s] [%s] [%s] at [%s]\n"  *)
(*                                                              (keys_string oti) (keys_string ati) (keys_string bti) *)
(*                                                              (string_of_path pt)); *)
                                    if oti = bti then 
                                      begin
                                        report_keys_action buf A Deleted oti pt;
                                        report_keys_action buf A Added ati pt;
                                        (ot'@ati,at'@ati,bt'@ati)
                                      end                            
                                    else 
                                      begin                                         
                                        report_keys_del_mod buf r rb oti ati bti pt;
                                        (ot'@oti,at'@ati,bt'@bti)
                                      end
                                | Diff3.BChange(oti,ati,bti) ->
(*                                     if false then Buffer.add_string buf (sprintf "BCHANGE [%s] [%s] [%s] at [%s]\n"  *)
(*                                                              (keys_string oti) (keys_string ati) (keys_string bti) *)
(*                                                              (string_of_path pt)); *)
                                    if oti = ati then 
                                      begin
                                        report_keys_action buf B Deleted oti pt;
                                        report_keys_action buf B Added bti pt;
                                        (ot'@bti,at'@bti,bt'@bti)
                                      end                            
                                    else 
                                      begin                                         
                                        report_keys_del_mod buf r rb oti ati bti pt;
                                        (ot'@oti,at'@ati,bt'@bti)
                                      end
                                | Diff3.Conflict(oti,ati,bti) ->
(*                                     if false then Buffer.add_string buf (sprintf "CONFLICT [%s] [%s] [%s] at [%s]\n"  *)
(*                                                              (keys_string oti) (keys_string ati) (keys_string bti) *)
(*                                                              (string_of_path pt)); *)
                                    if ati=bti then 
                                      begin                                         
                                        report_keys_action buf Both Deleted oti pt;
                                        report_keys_action buf Both Added ati pt;
                                        (ot'@ati,at'@ati,bt'@ati)
                                      end
                                    else
                                      begin
                                        report_keys_conflict buf oti ati bti pt;
                                        (ot'@oti,at'@ati,bt'@bti)
                                      end)
                           ([],[],[]) chunks in
                         (Erx.TagMap.add t ot' tmo,
                          Erx.TagMap.add t at' tma,
                          Erx.TagMap.add t bt' tmb))
                    (s_tags) (Erx.TagMap.empty,Erx.TagMap.empty,Erx.TagMap.empty) in
                  let sko' = (s,tmo) in 
                  let ska' = (s,tma) in 
                  let skb' = (s,tmb) in 
                  if Erx.valid sko' && Erx.valid ska' && Erx.valid skb' then 
                    (Erx.unparse (s,tmo),
                     Erx.unparse (s,tma),
                     Erx.unparse (s,tmb))
                  else 
                    begin 
                      report_schema_conflict buf sko' ska' skb' p;
                      (o,a,b)
                    end
              end                   
        end 
      end in 
    (o',a',b') in
  let o',a',b' = isync_aux p ty o a b in 
  (Buffer.contents buf,o',a',b')

let sync_opt t oo ao bo = match oo,ao,bo with
  | None,_,_
  | _,None,_
  | _,_,None ->
      begin match atomic_sync oo ao bo with
        | None       -> 
            (sprintf "Conflict at [%s].\n" (string_of_path root),oo,ao,bo)
        | Some (r,d) -> 
            ("",r,r,r)
      end
  | Some o,Some a,Some b ->
      let acts,o',a',b' = isync (Buffer.create 101) root t o a b in
      (acts,Some o',Some a',Some b')
          
let sync t o a b = isync (Buffer.create 101) root t o a b
