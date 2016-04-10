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
(* src/balign.mli                                                             *)
(* Alignment and Permutation                                                  *)
(* $Id: balign.mli 4901 2010-05-13 21:14:49Z cretin $ *)
(******************************************************************************)



module Alignment :
sig
  type t
  val empty : t
  val to_cost : t -> Bcost.t
  val to_error_option : t -> (unit -> unit) option
  val print_space : t -> string -> unit
  val print : t -> unit
  val merge : t -> t -> t
  val get_new : t -> Btag.t -> int -> int option (* for one tag, gives i such that Link(i, j) is in g *)
(*   val add_lnk : Btag.t -> int * int -> t -> Bstring.cat Btag.MapIntMapA.t -> Bstring.cat Btag.MapIntMapA.t -> t *)
(*   val add_crt_deep : Btag.t -> int -> t -> Bstring.cat Btag.MapIntMapA.t -> Bstring.cat Btag.MapIntMapA.t -> t *)
(*   val add_del_deep : Btag.t -> int -> t -> Bstring.cat Btag.MapIntMapA.t -> Bstring.cat Btag.MapIntMapA.t -> t *)
end

module Permutation :
sig
  type t
  val print : t -> unit
  val empty : t
  val add : Btag.t -> (int * int) -> t -> t
  val apply : Btag.t -> int -> t -> int
  val inv : t -> t (* fast! *)
  val shift : t -> Btag.MapInt.t -> Btag.MapInt.t -> t -> t
  val compose : t -> t -> t -> t
end

type 'a resource = 'a Btag.MapIntMapA.t

val align :
     Bcost.t (* this is a cost limit to avoid too much computing ; if
     [align] realizes that the alignment it's computing will have a
     cost higher than the limit, it will return an infinite cost
     alignment ; should be initiated with Bcost.infinite *)
  -> (Bstring.cat * Bstring.chunkmap) (* the new view *)
  -> (Bstring.cat * Bstring.chunkmap) (* the old view *)
  -> Alignment.t (* an accumulator ; should be initiated with
     Aligment.empty *)
  -> Alignment.t

(* auxiliary functions *)

val print_res : ('a -> unit) -> 'a resource -> unit
val align_compose_res : 'a resource -> Alignment.t -> 'a resource
val res_compose_perm : 'a resource -> Permutation.t -> 'a resource
val res_zip : (('a * 'a) -> 'a) -> 'a resource -> 'a resource -> 'a resource -> 'a resource
val res_unzip : ('a -> ('a * 'a)) ->'a resource -> Btag.MapInt.t -> Btag.MapInt.t -> ('a resource * 'a resource * 'a resource)
