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
(* /src/amapblist.ml                                                          *)
(* 'a map 'b list                                                             *)
(* $Id: amapblist.ml 4607 2009-08-03 16:53:28Z ddavi $ *)
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

module Make =
  functor (Ord:Map.OrderedType) ->
struct
  module OMap = Map.Make (Ord)
  type key = Ord.t
  type 'a t = 'a list OMap.t
  let empty = OMap.empty
  let is_empty t =
    OMap.fold (fun _ l b -> b && l = []) t true
  let add key v t =
    let l =
      try OMap.find key t
      with Not_found -> []
    in OMap.add key (v::l) t
  let rev t =
    OMap.map Safelist.rev t
  let map f t =
    OMap.map (Safelist.map f) t
  let fold f t e =
    OMap.fold (fun _ l a -> Safelist.fold_left (fun x y -> f y x) a l) t e
  let foldi f t e =
    OMap.fold (fun k l a -> snd (Safelist.fold_left (fun (i, x) y -> succ i, f k i y x) (0, a) l)) t e
  let next key t =
    match OMap.find key t with
    | [] -> raise Not_found
    | v::l -> v, OMap.add key l t
  let add_list = OMap.add
  let fold_list = OMap.fold
  let find_list key t =
    try OMap.find key t
    with Not_found -> []
  let map_list f t =
    OMap.map f t
  let iter_list f t =
    OMap.iter f t
  let mem_list key t =
    OMap.mem key t
  let print_list fk fl t =
    OMap.iter (fun k l -> fk k; fl l) t
end
