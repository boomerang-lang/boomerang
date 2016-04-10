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
(* /src/amapblist.mli                                                         *)
(* 'a map 'b list                                                             *)
(* $Id: amapblist.mli 4607 2009-08-03 16:53:28Z ddavi $ *)
(******************************************************************************)

module type S =
sig
  type key
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val rev : 'a t -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val foldi : (key -> int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val next : key -> 'a t -> 'a * 'a t
  val add_list : key -> 'a list -> 'a t -> 'a t
  val fold_list : (key -> 'a list -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val find_list : key -> 'a t -> 'a list
  val map_list : ('a list -> 'b list) -> 'a t -> 'b t
  val iter_list : (key -> 'a list -> unit) -> 'a t -> unit
  val mem_list : key -> 'a t -> bool
  val print_list : (key -> unit) -> ('a list -> unit) -> 'a t -> unit
end

module Make (Ord:Map.OrderedType) : S with type key = Ord.t
