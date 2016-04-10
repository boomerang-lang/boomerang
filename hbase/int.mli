(***************************************************)
(* The Harmony Project                             *)
(* harmony@lists.seas.upenn.edu                    *)
(*                                                 *)
(* int.mli - interface for ints                    *)
(***************************************************)
(* $Id$ *)

(** Structures of Integers *)
module type SET = sig include Set.S end
  
module Set : Set.S with type elt = int
  (** Sets of ints *)

module Map : Mapplus.SMap with type key_t = int 
                          and type key_set = Set.t
  (** Finite maps with ints as keys *)
  
module Hash : Hashtbl.S with type key = int

module SetSet : SET with type elt = Set.t
  (** Sets of sets of int *)



