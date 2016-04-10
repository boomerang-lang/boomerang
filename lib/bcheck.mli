(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                 *)
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
(* /src/bcheck.mli                                                            *)
(* Boomerang type checker interface                                           *)
(* $Id: bcheck.mli 4644 2009-09-06 18:43:13Z jnfoster $ *)
(******************************************************************************)

open Bident
open Bsyntax

val get_type : (Qid.t -> 'a option) -> Info.t -> Qid.t -> 'a 

val inst_cases : 
  (Id.t * sort) list ->  
  ('a * sort option) list -> 
  ('a * sort option) list

val compatible : sort -> sort -> bool
(** [compatible f t] returns true iff [f] and [t] are compatible *)

val trivial_cast : sort -> sort -> bool
(** [trivial_cast f t] returns true if the cast is trivial, i.e.,
    always satisfied *)

val mk_cast : string -> Info.t -> sort -> sort -> exp -> exp

val check_module: modl -> modl

