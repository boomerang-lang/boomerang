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
(* /src/bsubst.ml                                                             *)
(* Boomerang substitutions interface                                          *)
(* $Id *)
(******************************************************************************)

open Bsyntax
open Bident

val subst_sort : (Id.t * sort) list -> sort -> sort

val subst_exp_in_sort : (Qid.t * exp) list -> sort -> sort

val subst_exp : (Qid.t * exp) list -> exp -> exp

val free_sort_vars : sort -> Id.Set.t

val free_exp_vars_in_sort : sort -> Qid.Set.t

val free_exp_vars : exp -> Qid.Set.t

val free_exp_vars_pat : pat -> Qid.Set.t

val erase_sort : sort -> sort 

val expose_sort : sort -> sort

val qualify_sort : (Qid.t -> Qid.t) -> Qid.t list -> sort -> sort
(** [qualify_sort resolve bound s0] qualifies unbound, unqualified ids in [s0]
    with the resolution function [resolve].  For top-level calls, [bound] 
    should be empty. *)

val syneq_exp : exp -> exp -> bool

val syneq_sort : sort -> sort -> bool

val gensym : Info.t -> exp -> Id.t

