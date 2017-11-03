open Stdlib
open Star_semiring_tree

module type PlusStarTreeAlignment =
sig
  (* Data *)
  type t
  val show : t shower
  val pp : t pper
  val compare : t comparer
  val hash : t hasher
  val hash_fold_t : t hash_folder
end

module PlusTimesStarTreeAlignmentOf
    (PD : Data)
    (TD : Data)
    (SD : Data)
    (BD : Data) =
struct
  type nonempty_t =
    | Plus of PD.t * (int * nonempty_t) list list * (int * nonempty_t) list list
    | Times of TD.t * Permutation.t * int list * int list * nonempty_t list
    | Star of SD.t * nonempty_t
    | Base of BD.t

  type t =
    | Empty
    | NonemptyTree of nonempty_t

  let rec nonempty_cost
      (nt:nonempty_t)
    : float =
    begin match nt with
      | Plus (l,) -> 
    end
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
