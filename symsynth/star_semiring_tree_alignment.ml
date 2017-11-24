open Stdlib
open Star_semiring_tree


module PlusTimesStarTreeAlignmentOf
    (PD : Data)
    (TD : Data)
    (SD : Data)
    (BD : Data) =
struct
  module type NonemptyPlusStarTreeAlignmentType =
  sig
    (* Data *)
    type t
    val show : t shower
    val pp : t pper
    val compare : t comparer
    val hash : t hasher
    val hash_fold_t : t hash_folder
    (*val cost : t -> float*)
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
        (IntModule)
        (IntModule)

    type t =
      | Plus of PD.t * MappingDict.t * CreateDict.t * CreateDict.t
      | Times of TD.t * Permutation.t * int list * int list * t list
      | Star of SD.t * t
      | Base of BD.t
    [@@deriving ord, show, hash]

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
          let priority_multiplier = 1. -. ((mapped_count -. bij_size) /. max_size) in
          failwith "ah"
      end
  end

   type t =
      | Empty
      | NonemptyTree of NonemptyPlusStarTreeAlignment.t
    [@@deriving ord, show, hash]

      (*let mapped_count = List.length al in
        let unmapped_left_count = List.length an.pleft in
        let unmapped_right_count = List.length an.pright in
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
          ~f:(fun acc a' -> acc +. (nonempty_cost a'))
          ~init:0.0
          al
        in
        let unnormalized_cost =
        unnormalized_unmapped_cost +.
        unnormalized_recursive_cost
        in
        (unnormalized_cost /. total_size)*)
end
