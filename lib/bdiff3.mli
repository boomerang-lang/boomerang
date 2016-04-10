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
(* /src/bdiff3.ml                                                             *)
(* Boomerang Diff3 implementation                                             *)
(* $Id: bdiff3.mli 4607 2009-08-03 16:53:28Z ddavi $ *)
(******************************************************************************)

(* ----- functor argument ----- *)
module type Diff3Arg = sig
  type elt
  val equal : elt -> elt -> bool
end

(* ------ main module ----- *)
module Make(A:Diff3Arg) : sig 
  type elt = A.elt
  type seq = elt list  
  type chunk = 
    | Stable of elt * elt * elt
    | AChange of seq * seq * seq
    | BChange of seq * seq * seq
    | Conflict of seq * seq * seq
  val parse : seq -> seq -> seq -> chunk list
end
