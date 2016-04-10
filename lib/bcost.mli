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
(* src/bcost.mli                                                              *)
(* Costs of alignments                                                        *)
(* $Id: bcost.mli 4628 2009-08-17 20:39:00Z cretin $ *)
(******************************************************************************)

type t
val to_option : t -> int option
val to_string : t -> string
val zero : t
val one : t
val infinite : t
val is_infinite : t -> bool
val plus : t -> t -> t
val minus : t -> t -> t
val limit_minus : t -> t -> t
val of_weighted_int : Bannot.Weight.t -> int -> t
val of_int : int -> t
val succ : Bannot.Weight.t -> t -> t
val min : t -> t -> t
val lt : t -> t -> bool
val le : t -> t -> bool
val compare : t -> t -> int
val equal : t -> t -> bool
