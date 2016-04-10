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
(* /src/bheap.ml                                                              *)
(* Boomerang identifiers interface                                            *)
(* $Id: bident.mli 4624 2009-08-12 16:13:36Z cretin $ *)
(******************************************************************************)

(** {2 Plain Identifiers } *)
module Id : sig
  type t = Info.t * string
  (** the type of identifiers: parsing info and a string *)
  
  val mk : Info.t -> string -> t
  (** [mk i s] returns an identifier for [s] with parsing info [i]. *)

  val info_of_t : t -> Info.t
  (** [info_of_t x] returns the parsing info from [x]. *)

  val string_of_t : t -> string 
  (** [string_of_t x] returns the string that [x] represents. *)

  val prime : t -> t 
  (** [prime x] returns [x']. *)
 
  val compare : t -> t -> int
  (** [compare x y] compares [x] and [y], ignoring parsing info. *)

  val equal : t -> t -> bool
  (** [equal x y] returns [true] iff [x] and [y] represent the same
      string. *)

  val wild : t
  (** [wild] is a constant representing the "don't care" string "_" *)

  module Set : Set.S with type elt = t
  (** Sets with Id.ts as elements *)

  module Map : Map.S with type key = t
  (** Maps with Id.ts as keys *)
end

(** {2 Qualified Identifiers } *)
module Qid : sig 
  type t = Id.t list * Id.t
  (** the type of identifiers: a list of qualifiers and a
      base identifier *)

  val mk : Id.t list -> Id.t -> t
  (** [mk qs x] returns the qualified identifier with qualifiers [qs]
      and base identifier [x]. *)

  val t_of_id : Id.t -> t
  (** [t_of_id q] returns a qualified identifier with base identifier
      [x] and no qualifiers. *)

  val info_of_t : t -> Info.t
  (** [info_of_t q] returns the parsing info associated with [q]. *)

  val qs_of_t : t -> Id.t list
  (** [qs_of_t q] returns the qualifiers associated with [q]. *)

  val id_of_t : t -> Id.t
  (** [id_of_t q] returns the base identifier associated with [q]. *)

  val string_of_t : t -> string 
  (** [string_of_t q] formats prints [q] as a string. *)

  val prime : t -> t 
  (** [prime x] returns [x']. *)
 
  val compare : t -> t -> int
  (** [compare q1 q2] comparse [q1] and [q2] using a dictionary
      ordering on the underlying list of identifiers. *)

  val equal : t -> t -> bool
  (** [equal q1 q2] returns [true] iff [q1] and [q2] represent the same
      qualified identifier. *)

  val id_dot : Id.t -> t -> t
  (** [id_dot x1 q1] returns the qualified identifier representing [x1.q1]. *)

  val t_dot_id : t -> Id.t -> t
  (** [t_dot_id q x] returns the qualified identifier representing
      [q.x]. *)

  val parent_t : t -> t
  (** [parent_t q.x] returns the [q] *)

  val t_dot_t : t -> t -> t
  (** [t_dot_t q1 q2] returns the qualified identifier representing
      [q1.q2]. *)

  val id_prefix : t -> Id.t list -> bool
  (** [id_prefix q xl] returns [true] iff [q] is a prefix of [xl]. *)

  val mk_mod_t : Info.t -> string list -> string -> t
  (** [mk_mod_t ss s] constructs the qualified identifier representing
      [ss] with dummy parsing info. *)

  val mk_native_prelude_t : Info.t -> string -> t
  (** [mk_native_prelude_t s] constructs the qualified identifier representing
      [Native.Prelude.s] with dummy parsing info. *)

  val mk_prelude_t : Info.t -> string -> t
  (** [mk_prelude_t s] constructs the qualified identifier representing
      [Prelude.s] with dummy parsing info. *)

  val mk_core_t : Info.t -> string -> t
  (** [mk_prelude_core_t s] constructs the qualified identifier representing
      [Prelude.Core.s] with dummy parsing info. *)

  val mk_list_t : Info.t -> string -> t
  (** [mk_list_t s] constructs the qualified identifier representing
      [List.s] with dummy parsing info. *) 

  module Env : Env.S with type key = t
  (** Environments with Qid.ts as keys *)

  module Set : Set.S with type elt = t
  (** Sets with Qid.ts as elements *)

  module Map : Map.S with type key = t
  (** Maps with Qid.ts as keys *)
end
