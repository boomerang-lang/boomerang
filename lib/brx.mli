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
(* /src/brx.ml                                                                *)
(* Boomerang RegExp engine                                                    *)
(* $Id: brx.mli 4643 2009-09-03 18:34:32Z cretin $ *)
(******************************************************************************)
type t

(* constants *)
val epsilon : t 
val empty : t
val ascii_set : t

(* constructors *)
val mk_cset : (int * int) list -> t
val mk_neg_cset : (int * int) list -> t
val mk_neg_ascii_cset : (int * int) list -> t
val mk_string : string -> t
val mk_alt : t -> t -> t
val mk_seq : t -> t -> t
val mk_star : t -> t
val mk_iter : t -> int -> int -> t
val mk_diff : t -> t -> t
val mk_complement: t -> t
val mk_inter : t -> t -> t
val mk_reverse : t -> t
val mk_expand : t -> int -> t -> t

(* pretty printing *)
(* ranks *)
type r = 
  | Urnk (* union *)
  | Drnk (* diff *)
  | Irnk (* inter *)
  | Crnk (* concat *)
  | Srnk (* star *)
  | Arnk (* atomic *)
val rank : t -> r
val lpar : r -> r -> bool
val rpar : r -> r -> bool

val format_t : t -> unit
val string_of_char_code : int -> string
val string_of_t : t -> string

(* operations *)
val is_empty : t -> bool
val is_final : t -> bool
val is_singleton : t -> bool
val disjoint_cex : t -> t -> string option
val disjoint : t -> t -> bool
val equiv : t -> t -> bool
val representative : t -> string option

(* string matching *)
val match_sub_string : t -> string -> int -> int -> bool
val match_string : t -> string -> bool
val match_string_positions : t -> string -> Int.Set.t
val match_string_reverse_positions : t -> string -> Int.Set.t

(* ambiguity *)
val derivative : t -> string -> t
val mk_reverse : t -> t
val splittable_cex : t -> t -> ((string * string * string * string),t) Misc.alternative
val splittable : t -> t -> bool
val iterable_cex : t -> ((string * string * string * string),t) Misc.alternative
val iterable : t -> bool

(* splitting *)
val split_positions : t -> t -> string -> Int.Set.t
val bad_prefix_position : t -> string -> int
val split_bad_prefix : t -> string -> string * string
val seq_split : t -> t -> string -> (string * string) option
val star_split : t -> string -> string list

(* statistics *)
val print_stats : unit -> unit

(* extended charset *)
val langle_code : int
val rangle_code : int
val colon_code : int

