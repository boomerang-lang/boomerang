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
(* /src/benv.ml                                                               *)
(* Boomerang environments                                                     *)
(* $Id: benv.ml 4646 2009-09-10 14:33:53Z jnfoster $ *)
(******************************************************************************)

(* ---------------------------------------------------------------------------*)
(* IMPORTS AND ABBREVIATIONS *)

open Bident
module G = Bregistry
module V = Bvalue

(* ---------------------------------------------------------------------------*)
(* COMMON ENVIRONMENT SIGNATURE *)

module type CEnvSig = 
sig
  type t 
  type v
  val empty : Qid.t -> t
  val get_ev : t -> Bregistry.REnv.t
  val set_ev : t -> Bregistry.REnv.t -> t
  val get_ctx : t -> Qid.t list
  val set_ctx : t -> Qid.t list -> t
  val push_ctx : t -> Qid.t -> t
  val pop_ctx : t -> t
  val get_mod : t -> Qid.t 
  val set_mod : t -> Qid.t -> t
  val format : t -> unit
  val lookup : t -> Qid.t -> v option
  val lookup_both : t -> Qid.t -> (v * t) option
  val lookup_o : t -> Qid.t -> (Qid.t * v) option
  val lookup_type : t -> Qid.t -> (Qid.t * Bregistry.tspec) option 
  val lookup_con : t -> Qid.t -> (Qid.t * Bregistry.tspec) option
  val update : t -> Qid.t -> v -> t
  val update_list : t -> (Qid.t * v) list -> t
  val update_type : t -> Id.t list -> Qid.t -> Bregistry.tcon list -> t
  val fold : (Qid.t -> v -> 'a -> 'a) -> t -> 'a -> 'a
end

(* ---------------------------------------------------------------------------*)
(* COMPILATION ENVIRONMENTS *)

module CEnv = 
struct
  type t = (Qid.t list * Qid.t) * G.REnv.t
  type v = G.rv

  let empty m = (([],m), G.REnv.empty ())

  (* accessors / setters *)
  let get_ev cev = let (_,ev) = cev in ev
  let set_ev cev ev = let (os,_) = cev in (os,ev)
  let get_mod cev = let ((_,m),_) = cev in m
  let set_mod cev m = let ((os,_),ev) = cev in ((os,m),ev)
  let get_ctx cev = let ((os,_),_) = cev in os
  let set_ctx cev os = let ((_,m),ev) = cev in ((os,m),ev)
  let push_ctx ((os,m),ev) x = (x::os,m),ev
  let pop_ctx ((os,m),ev) = (List.tl os,m),ev

  let format ((os, m), _) =
    Printf.printf "[%s] %s\n"
      (snd
         (Safelist.fold_left
            (fun (b, s) o ->
               true, s ^ (if b then ";" else "") ^ (Qid.string_of_t o)
            )
            (false, "")
            os))
      (Qid.string_of_t m)

  (* lookup from cev, then from library *)
  let lookup_generic 
      (select : (Qid.t * 'v) -> 'r)
      (lookup_fun : G.REnv.t -> Qid.t -> 'v option)
      (lookup_library_fun : Qid.t list -> Qid.t -> 'r option)
      (cev : t)
      (q : Qid.t) : 'r option = 
    let ev = get_ev cev in 
    let ctx = get_ctx cev in 
    let rec aux nctx q2 = match lookup_fun ev q2 with
      | Some r -> Some (select (q2, r))
      | None -> begin  match nctx with
          | [] -> None
          | o::orest -> aux orest (Qid.t_dot_t o q)
        end in 
    match aux ctx q with
      | Some x -> Some x
      | None -> lookup_library_fun ctx q
          
  let lookup cev q = 
    lookup_generic snd
      G.REnv.lookup
      G.lookup_library_ctx
      cev q

  let lookup_both cev q : (G.rv * t) option =    
    let aux (v,ev) = (v,set_ev cev ev) in 
    lookup_generic 
      (fun (_,p) -> aux p)
      G.REnv.lookup_both
      (fun ctx q -> match G.lookup_both_library_ctx ctx q with
         | Some p -> Some (aux p)
         | None -> None)
      cev q

  let lookup_o cev q = 
    lookup_generic (fun x -> x)
      G.REnv.lookup
      G.lookup_library_ctx_o
      cev q

  let lookup_type cev q = 
    lookup_generic snd
      G.REnv.lookup_type
      G.lookup_type_library_ctx 
      cev q 

  let lookup_con cev q = 
    lookup_generic snd
      G.REnv.lookup_con
      G.lookup_con_library_ctx 
      cev q 

  let update cev q rv =
    set_ev cev (G.REnv.update (get_ev cev) q rv)

  let update_list cev qs = 
    Safelist.fold_left 
      (fun cev (q,sv) -> update cev q sv)
      cev qs
            
  let update_type cev svars q cl = 
    set_ev cev (G.REnv.update_type (get_ev cev) svars q cl)

  let fold f cev a = G.REnv.fold f (get_ev cev) a
end

(* ---------------------------------------------------------------------------*)
(* SORT CHECKING ENVIRONMENTS *)

module SCEnv = 
struct
  type t = CEnv.t
  type v = G.rs

  let dummy_value = V.Unt (Info.M "dummy value")
  let empty = CEnv.empty        
  let get_ev = CEnv.get_ev
  let set_ev = CEnv.set_ev   
  let get_mod = CEnv.get_mod
  let set_mod = CEnv.set_mod
  let get_ctx = CEnv.get_ctx
  let set_ctx = CEnv.set_ctx
  let push_ctx = CEnv.push_ctx
  let pop_ctx = CEnv.pop_ctx

  let format = CEnv.format

  let lookup sev q = match CEnv.lookup sev q with 
    | None -> None
    | Some (s,_) -> Some s
        
  let lookup_both sev q = match CEnv.lookup_both sev q with
      | None -> None
      | Some ((s,_),sev) -> Some (s,sev)
    
  let lookup_o sev q = 
    match CEnv.lookup_o sev q with 
    | None -> None
    | Some (q,(s,_)) -> Some (q,s)
  let lookup_type = CEnv.lookup_type
  let lookup_con = CEnv.lookup_con 
  let update sev q s = CEnv.update sev q (s,dummy_value)
  let update_list sev qs = 
    Safelist.fold_left
      (fun sev (q,s) -> update sev q s)
      sev qs
  let update_type sev svars q cs = CEnv.update_type sev svars q cs
  let fold f sev a = CEnv.fold (fun q (s,_) a -> f q s a) sev a
end
