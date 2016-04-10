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
(* /src/benv.mli                                                              *)
(* Boomerang environments interface                                           *)
(* $Id: benv.mli 4646 2009-09-10 14:33:53Z jnfoster $ *)
(******************************************************************************)

(* imports *)
open Bident
open Bregistry 

(* common environment type *)
module type CEnvSig = 
sig
  type t 
  type v
  val empty : Qid.t -> t
  val get_ev : t -> REnv.t
  val set_ev : t -> REnv.t -> t
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
  val lookup_type : t -> Qid.t -> (Qid.t * tspec) option
  val lookup_con : t -> Qid.t -> (Qid.t * tspec) option
  val update : t -> Qid.t -> v -> t
  val update_list : t -> (Qid.t * v) list -> t
  val update_type : t -> Id.t list -> Qid.t -> tcon list -> t
  val fold : (Qid.t -> v -> 'a -> 'a) -> t -> 'a -> 'a
end

(* compilation environments *)
module CEnv : CEnvSig with type v = rv

(* sort checking environments *)
module SCEnv : CEnvSig with type v = rs
