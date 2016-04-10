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
(* src/barx.mli                                                               *)
(* Annotated regular expressions                                              *)
(* $Id: barx.mli 4901 2010-05-13 21:14:49Z cretin $ *)
(******************************************************************************)


(* generic helper for iterating a regexp / lens / canonizer *)
val generic_iter :
     'a               (* epsilon *)
  -> ('a -> 'a -> 'a) (* union *)
  -> ('a -> 'a -> 'a) (* concat *)
  -> ('a -> 'a)       (* star *)
  -> int -> int       (* min / max *)
  -> 'a               (* x *)
  -> 'a


(* ---------------------------------------------------------------------------*)
(* ANNOTATED REGEXP *)

type t
val rxtype : t -> Brx.t
val annot_weight : bool * Bannot.Weight.t -> t -> t
val mk_rx : Brx.t -> t
val mk_box : Btag.t -> t -> t
val mk_seq : t -> t -> t
val mk_alt : t -> t -> t
val mk_star : t -> t
val mk_iter : t -> int -> int -> t
val empty : t
val epsilon : t
val extended : t -> Brx.t
val format_t : t -> unit
val string_of_t : t -> string
val equiv : t -> t -> bool
val equiv_cex : t -> t -> string option
val match_compatible_cex : Btag.t -> t -> string option
val compatible_cex : t -> t -> string option
val parse : t -> Bstring.t -> Bstring.at
val drop : t -> Brx.t
val to_tags : t -> Btag.Set.t
val no_chunks : t -> bool
