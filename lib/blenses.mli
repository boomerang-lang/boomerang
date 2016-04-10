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
(* /src/blenses.mli                                                           *)
(* Boomerang lens combinators interface                                       *)
(* $Id: blenses.mli 4901 2010-05-13 21:14:49Z cretin $ *)
(******************************************************************************)

module Permutations : sig
  val valid_permutation : int list -> 'a list -> bool
    (** [valid_permutation sigma ls] is true iff sigma is a valid permutation for ls,
        i.e. if they are both length k and sigma is a rearrangement of the list
        [0;1;...;k-1] *)
  val permutations : int -> int list list
    (** [permutations k] returns all valid permutations of lists of length k
        The identity permutation [0;1;...;k-1] will be first in this list. *)
  val invert_permutation : int list -> int list
    (** [invert_permutation i sigma] inverts sigma *)
  val permute_list : int list -> 'a list -> 'a list
    (** [permute_list sigma ls] permutes [ls] according to [sigma]; an error will
        be signalled if it is not the case that [valid_permutation sigma ls] *)
end

module Canonizer : sig
  type t

  (* meta data *)
  val info : t -> Info.t
  val format_t : t -> unit
  val string_of_t : t -> string
  (* types *)
  val uncanonized_type : t -> Brx.t
  val canonized_type : t -> Brx.t
  val uncanonized_atype : t -> Barx.t
  val canonized_atype : t -> Barx.t
  val cnrel_identity : t -> bool
  (* components *)
  val canonize : t -> Bstring.t -> string
  val choose : t -> Bstring.t -> string

  (* constructors *)
  val copy : Info.t -> Brx.t -> t
  val concat : Info.t -> t -> t -> t
  val union : Info.t -> t -> t -> t
  val star : Info.t -> t -> t
  val normalize : Info.t -> Brx.t -> Brx.t -> (string -> string) ->  t
  val sort : Info.t -> Barx.t list -> t
  val columnize : Info.t -> int -> Brx.t -> char -> string -> t
  val iter : Info.t -> t -> int -> int -> t
end

type ktype
val ktype_equiv : ktype -> ktype -> bool
val format_ktype : ktype -> unit

type mtype
val mtype_equiv : mtype -> mtype -> bool
val format_mtype : mtype -> unit
val mtype_match_compatible_cex : Btag.t -> ktype -> mtype -> string option
val mtype_compatible_cex : mtype -> mtype -> string option
val mtype_domain_equal : mtype -> mtype -> bool

module MLens : sig 
  type t

  val info : t -> Info.t
  val format_t : t -> unit
  val string_of_t : t -> string

  val astype : t -> Barx.t
  val avtype : t -> Barx.t
  val stype : t -> Brx.t
  val vtype : t -> Brx.t
  val ktype : t -> ktype
  val mtype : t -> mtype
  val srep : t -> Bstring.t -> string
  val vrep : t -> Bstring.t -> string
  val sequiv_identity : t -> bool
  val vequiv_identity : t -> bool
  val bij : t -> bool
  val rget : t -> Bstring.t -> string
  val rput : t -> Bstring.t -> Bstring.t -> string
  val rcreate : t -> Bstring.t -> string
  val canonizer_of_t : Info.t -> t -> Canonizer.t
  val invert : Info.t -> t -> t
  val copy : Info.t -> Brx.t -> t
  val weight : Info.t -> bool -> Bannot.Weight.t -> t -> t
  val clobber : Info.t -> Brx.t -> string -> (string -> string) -> t
  val concat : Info.t -> t -> t -> t
  val union : Info.t -> t -> t -> t
  val star : Info.t -> t -> t
  val iter : Info.t -> t -> int -> int -> t
  val permute : Info.t -> int list -> t list -> t
  val compose : Info.t -> t -> t -> t
  val align : Info.t -> t -> t
  val default : Info.t -> t -> Bstring.t -> t
  val mmatch : Info.t -> Btag.t -> t -> t
  val partition : Info.t -> Brx.t list -> t
  val merge : Info.t -> Brx.t -> t
  val fiat : Info.t -> t -> t
  val left_quot : Info.t -> Canonizer.t -> t -> t
  val right_quot : Info.t -> t -> Canonizer.t -> t
  val dup1 : Info.t -> t -> (string -> string) -> Brx.t -> t
  val dup2 : Info.t -> (string -> string) -> Brx.t -> t -> t
end
