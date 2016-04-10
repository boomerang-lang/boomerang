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
(* src/bstring.mli                                                            *)
(* Annotated strings                                                          *)
(* $Id: bstring.mli 4901 2010-05-13 21:14:49Z cretin $ *)
(******************************************************************************)

type t (* a read-only string *)
type a (* annotation on read-only strings *)
type at = a * t (* an annotated string *)

(* [attmp] is a temporary structure used in Barx.parse to build an
[at] from a schema ([Barx.t]) and a string in the schema *)
type attmp

(* an annotated string which is a chunk *)
type cat

(* a [chunkmap] is a map from positions to a pair of costs and the
chunk at the given position ; the first cost is the cost of the
skeleton, and the second cost is the cost of the whole chunk
(including its subchunk and recursively) *)
type chunkmap = ((int * int) * cat) Btag.MapIntMapA.t

(* [toplevel_chunks] returns the first level subchunks of a chunk *)
val toplevel_chunks : cat -> int Btag.MapAList.t

(* [at_to_chunktree] converts an annotated string to a chunk with a
map from its positions to the subchunks *)
val at_to_chunktree : at -> cat * chunkmap

val empty : t
val of_string : string -> t
val to_attmp : t -> attmp
val to_string : t -> string
val of_at : at -> t
val of_attmp : attmp -> t
val of_cat : cat -> t
val length : t -> int
val dist : int option -> string -> string -> int option
val at_print_flat : at -> string
val cat_print_flat : cat -> string
val at_print_all : at -> string
val cat_to_key : cat -> string
val cat_fold_on_locs : (Btag.t -> int -> 'a -> 'a) -> cat -> 'a -> 'a
val at_to_locs : at -> Btag.MapInt.t
val match_rx : Brx.t -> t -> bool
(* val at_to_weight_flat : at -> Bannot.Weight.t array * string *)
(* val at_dist : at -> at -> int *)
(* val cat_dist : cat -> cat -> int *)
val concat_ambiguous_split : int -> Brx.t -> Brx.t -> t -> t * t
val find_concat_split : Brx.t -> Brx.t -> int -> string -> int
val concat_split : Brx.t -> Brx.t -> t -> t * t
val star_ambiguous_split : int list -> Brx.t -> t -> t list
val find_star_split : Brx.t -> int list -> string -> int list
val star_split : Brx.t -> t -> t list
val do_concat : Brx.t -> Brx.t -> (attmp -> attmp) -> (attmp -> attmp) -> attmp -> attmp
val do_star : Brx.t -> (attmp -> attmp) -> attmp -> attmp
val annot_leaf : Bannot.Weight.t -> attmp -> attmp
val before_node : attmp -> attmp
val annot_node : Btag.t -> attmp -> attmp
val at_of_attmp : attmp -> at
