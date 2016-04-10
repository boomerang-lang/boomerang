(***********************************************)
(* The Harmony Project                         *)
(* harmony@lists.seas.upenn.edu                *)
(*                                             *)
(* env.mli - interface for Focal environments  *)
(***********************************************)
(* $Id: env.mli 3581 2008-03-03 12:19:20Z jnfoster $ *)

(** Environments for resolving Focal qualified identifiers *)

module type S = sig 
    type key
    type 'a t

    (** [empty ()] yields a fresh environment. *)
    val empty : unit -> 'a t 

    (** [update ev q r] extends [ev] with a binding for [qid] and [rv]. 
        Returns a new environment *)
    val update : 'a t -> key -> 'a -> 'a t

    (** [fold f e b] computes [(f kN aN ... (f k1 a1 b)...)] *)
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    (** [lookup ev q] returns an option representing the binding for [q]
        in [ev]. *)
    val lookup : 'a t -> key -> 'a option

    (** [format_t format_r ev] pretty prints [ev] using format_r. *)
    val format_t : 'a t -> ('a -> unit) -> unit

    (** [iter f ev] iterates f over every element of [ev] *)
    val iter : (key -> 'a -> unit) -> 'a t -> unit
  end

module type PrintableOrderedType = sig 
  include Set.OrderedType
  val to_string : t -> string
end

module Make(Key:PrintableOrderedType)  : S
  with type key = Key.t
