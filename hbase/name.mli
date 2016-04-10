(***************************************************)
(* The Harmony Project                             *)
(* harmony@lists.seas.upenn.edu                    *)
(*                                                 *)
(* name.mli - interface for names                  *)
(***************************************************)
(* $Id: name.mli 3437 2007-12-09 22:41:31Z jnfoster $ *)

(** Names (which label edges in trees) *)

type t = string
(** A name is just a string *)

module Set : Set.S with type elt = t
  (** Sets of names *)

module Map : Mapplus.SMap with type key_t = t 
                          and type key_set = Set.t
  (** Finite maps with names as keys *)

module Hash : Hashtbl.S with type key = t
  (** Hashtables with names as keys *)

module SetHash : Hashtbl.S with type key = Set.t
  (** Hashtables with sets of names as keys *)
