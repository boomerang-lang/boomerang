open Stdlib
open Star_semiring_tree

module type BaseAlignment =
sig
  type t
  val show : t shower
  val pp : t pper
  val compare : t comparer
  val hash : t hasher
  val hash_fold_t : t hash_folder
  val cost : t -> float
end

module type BaseData =
sig
  type t
  module Alignment : BaseAlignment
  val show : t shower
  val pp : t pper
  val compare : t comparer
  val hash : t hasher
  val hash_fold_t : t hash_folder
  val get_alignment : t -> t -> Alignment.t option
end

type position = int * int
[@@deriving ord, show, hash]

module PlusTimesStarTreeAlignmentOf
    (PD : Data)
    (TD : Data)
    (SD : Data)
    (BD : BaseData) =
struct
  module NormalizedTree = NormalizedPlusTimesStarTreeOf(BD)(PD)(TD)(SD)

  module type NonemptyPlusStarTreeAlignmentType =
  sig
    (* Data *)
    type t
    val show : t shower
    val pp : t pper
    val compare : t comparer
    val hash : t hasher
    val hash_fold_t : t hash_folder
    val cost : t -> float

    val mk_plus :
      PD.t ->
      ((position) * (position) * t) list ->
      ((position) * (position)) list ->
      ((position) * (position)) list ->
      t

    val mk_times :
      TD.t ->
      Permutation.t ->
      position list ->
      position list ->
      t list ->
      t

    val mk_star :
      SD.t ->
      t ->
      t

    val mk_base :
      BD.Alignment.t -> t
  end

  module rec NonemptyPlusStarTreeAlignment : NonemptyPlusStarTreeAlignmentType =
  struct
    module IntPair = PairOf(IntModule)(IntModule)

    module MappingDict =
      DictOf
        (PairOf(IntPair)(IntPair))
        (NonemptyPlusStarTreeAlignment)

    module CreateDict =
      DictOf
        (IntPair)
        (IntPair)

    type t =
      | Plus of PD.t * MappingDict.t * CreateDict.t * CreateDict.t
      | Times of TD.t * Permutation.t * position list * position list * t list
      | Star of SD.t * t
      | Base of BD.Alignment.t
    [@@deriving ord, show, hash]

   let mk_plus
       (pd:PD.t)
       (mls:((position) * (position) * t) list)
       (left_creates:((position) * (position)) list)
       (right_creates:((position) * (position)) list)
     : t =
     let md =
       MappingDict.from_kvp_list
         (List.map ~f:(fun (p1,p2,v) -> ((p1,p2),v)) mls)
     in
     let cld =
       CreateDict.from_kvp_list
         left_creates
     in
     let crd =
       CreateDict.from_kvp_list
         right_creates
     in
     Plus 
       (pd,md,cld,crd)

   let mk_times
       (td:TD.t)
       (p:Permutation.t)
       (p1:position list)
       (p2:position list)
       (als:t list)
     : t =
      Times (td,p,p1,p2,als)

   let mk_star (sd:SD.t) (nt:t) : t =
     Star (sd,nt)

   let mk_base (b:BD.Alignment.t) : t =
     Base b

   let rec cost
        (nt:t)
      : float =
      begin match nt with
        | Plus (_,md,cdl,cdr) ->
          let left_size  = Float.of_int (CreateDict.size cdl) in
          let right_size = Float.of_int (CreateDict.size cdr) in
          let bij_size = min left_size right_size in
          let max_size = left_size *. right_size in
          let mapped_count = Float.of_int (MappingDict.size md) in
          let priority_multiplier =
            if max_size = bij_size then
              1.
            else
              1. -.
              ((mapped_count -. bij_size) /. max_size)
          in
          let recursive_cost =
            List.fold_left
              ~f:(fun acc a' -> acc +. (cost a'))
              ~init:0.
              (MappingDict.value_list md)
          in
          let recursive_priority = 1. -. recursive_cost in
          let final_priority = recursive_priority *. priority_multiplier in
          1. -. final_priority
        | Times (_,_,pleft,pright,al) ->
          let mapped_count = List.length al in
          let unmapped_left_count = List.length pleft in
          let unmapped_right_count = List.length pright in
          let total_size =
            Float.of_int
              (mapped_count
               + unmapped_left_count
               + unmapped_right_count
               + 1)
          in
          let unnormalized_unmapped_cost =
            Float.of_int (unmapped_left_count + unmapped_right_count)
          in
          let unnormalized_recursive_cost =
            List.fold_left
              ~f:(fun acc a' -> acc +. (cost a'))
              ~init:0.
              al
          in
          let unnormalized_cost =
            unnormalized_unmapped_cost +.
            unnormalized_recursive_cost
          in
          (unnormalized_cost /. total_size)
        | Star (_,a) ->
          cost a
        | Base (a) ->
          BD.Alignment.cost a
      end

   module Tree = PlusTimesStarTreeOf(PD)(TD)(SD)(BD)
   module NonemptyTree = Tree

   let rec get_alignment_distance
       (t1:NormalizedTree.Nonempty.t)
       (t2:NormalizedTree.Nonempty.t)
     : float =
     failwith "ah"
  end

  type t =
    | Empty
    | NonemptyTree of NonemptyPlusStarTreeAlignment.t
  [@@deriving ord, show, hash]

  let cost
      (ao:t option)
    : float =
    begin match ao with
      | None -> 0.
      | Some Empty -> 1.
      | Some NonemptyTree nt ->
        NonemptyPlusStarTreeAlignment.cost nt
    end
end
