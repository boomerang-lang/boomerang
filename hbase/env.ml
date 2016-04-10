(***************************************)
(* The Harmony Project                 *)
(* harmony@lists.seas.upenn.edu        *)
(*                                     *)
(* env.ml - Focal environments         *)
(***************************************)
(* $Id $ *)


module type S = 
  sig 
    type key

    type 'a t
    (** Abstract type of environments *)

    val empty : unit -> 'a t 
    (** [empty ()] yields a fresh environment. *)

    val update : 'a t -> key -> 'a -> 'a t
    (** [update ev q r] extends [ev] with a binding for [qid] and
        [rv]. Returns a new environment *)

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold f e b] computes [(f kN aN ... (f k1 a1 b)...)] *)

    val lookup : 'a t -> key -> 'a option
    (** [lookup ev q] returns an option representing the binding for [q]
        in [ev]. *)

    val format_t : 'a t -> ('a -> unit) -> unit
    (** [format_t format_r ev] pretty prints [ev] using format_r. *)

    val iter : (key -> 'a -> unit) -> 'a t -> unit
    (** [iter f ev] iterates f over every element of [ev] *)
  end

module type PrintableOrderedType = sig 
  include Set.OrderedType
  val to_string : t -> string 
end

module Make(Key:PrintableOrderedType) = struct
  module EMap = Mapplus.Make(Key)
  module KeyMap = EMap.Map
  module KeySet = EMap.KeySet

  type key = Key.t
  type 'a t = 'a KeyMap.t

  (* the empty environment *)
  let empty () : 'a t = KeyMap.empty

  (* produce env[q->v]; yields a NEW env *)
  let update ev k v = KeyMap.add k v ev

  let fold = KeyMap.fold

  let lookup ev k = 
    try 
      Some (KeyMap.find k ev)
    with Not_found -> None

  let format_t ev format_r = 
    Util.format "{@[";  
    let _ = 
      KeyMap.fold (fun k v acco -> 
                     Util.format "@[%s=@[" (Key.to_string k);
                     format_r v;
                     Util.format "@]";
                     if acco <> None then Util.format ",";
                     Util.format "@] ";
                     Some ())
        ev
        None in            
      Util.format "@]}"

  let iter f = KeyMap.iter (fun k v -> f k v)
end
