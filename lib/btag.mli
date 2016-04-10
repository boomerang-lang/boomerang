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
(* src/btag.mli                                                               *)
(* Tags                                                                       *)
(* $Id: btag.mli 4636 2009-08-26 19:28:59Z cretin $ *)
(******************************************************************************)

type species =
  | Positional
  | Diffy of bool
  | Greedy
  | Setlike

type predicate =
  | Threshold of int

type tag
type t = tag

val to_string : t -> string
val of_elements : species -> predicate list -> Bannot.Weight.t -> string -> t
val to_elements : t -> species * predicate list * Bannot.Weight.t * string
val get_weight : t -> Bannot.Weight.t (* default weight *)
val get_predicates : t -> predicate list
val get_species : t -> species
val get_name : t -> string
val key_through : t -> bool
val format_t : t -> unit
val equiv : t -> t -> bool

module MapA :
sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val compatible : tag -> 'a t -> bool
  val addable : tag -> 'a t -> bool
  val add : tag -> 'a -> 'a t -> 'a t
  val find : tag -> 'a t -> 'a
  val mem : tag -> 'a t -> bool
  val iter : (tag -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (tag -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (tag -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module Set :
sig
  type t
  val empty : t
  val is_empty : t -> bool
  val compatible : tag -> t -> bool
  val addable : tag -> t -> bool
  val add : tag -> t -> t
  val union : t -> t -> t
  val fold : (tag -> 'a -> 'a) -> t -> 'a -> 'a
end

module MapAList :
sig
  type 'a t
  val empty : 'a t
  val compatible : tag -> 'a t -> bool
  val addable : tag -> 'a t -> bool
  val add : tag -> 'a -> 'a t -> 'a t
  val add_list : tag -> 'a list -> 'a t -> 'a t
  val fold_list : (tag -> 'a list -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val find_list : tag -> 'a t -> 'a list
  val iter_list : (tag -> 'a list -> unit) -> 'a t -> unit
  val rev : 'a t -> 'a t
  val print_list : (tag -> unit) -> ('a list -> unit) -> 'a t -> unit
end

module MapInt :
sig
  type t
  val empty : t
  val find : tag -> t -> int
  val incr : tag -> t -> t
  val plus : t -> t -> t
  val le : t -> t -> bool
  val equal : t -> t -> bool
  val fold : (tag -> int -> 'a -> 'a) -> t -> 'a -> 'a
  val of_TmA : int MapA.t -> t
end

(* TODO: add a module IntMapA *)
(* and then some functions to MapIntMapA like fold_tag, find_tag, add_tag, ... *)
(* and then look at the uses of MapIntMapA.foldi to see if it is possible to speed up using the new stuff *)
module MapIntMapA :
sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : tag -> int -> 'a -> 'a t -> 'a t
  val remove : tag -> int -> 'a t -> 'a t
  val find : tag -> int -> 'a t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val foldi : (tag -> int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val next : tag -> int -> 'a t -> ('a option * 'a t)
  val append : 'a t -> MapInt.t -> 'a t -> 'a t
  val add_ima : tag -> 'a Intmapa.t -> 'a t -> 'a t
  val mapi_ima : (tag -> 'a Intmapa.t -> 'b Intmapa.t) -> 'a t -> 'b t
  val find_ima : tag -> 'a t -> 'a Intmapa.t
  val fold_ima : (tag -> 'a Intmapa.t -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val print : (tag -> unit) -> (int -> 'a -> unit) -> 'a t -> unit
end

