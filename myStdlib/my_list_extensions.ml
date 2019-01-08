open Core
open Util
open Algebra

let sorting
    ~cmp:(cmp:'a comparer)
    (l:'a list)
  : (Permutation.t) =
  let l_with_is =
    List.mapi
      ~f:(fun i x -> (x,i))
      l
  in
  let sorted_l_with_is =
    List.sort
      ~compare:(fun (x1,_) (x2,_) -> cmp x1 x2)
      l_with_is
  in
  Permutation.create
    (List.map ~f:snd sorted_l_with_is)

let sorting_and_sort
    ~cmp:(cmp:'a comparer)
    (l:'a list)
  : (Permutation.t * 'a list) =
  let l_with_is =
    List.mapi
      ~f:(fun i x -> (x,i))
      l
  in
  let sorted_l_with_is =
    List.sort
      ~compare:(fun (x1,_) (x2,_) -> cmp x1 x2)
      l_with_is
  in
  (Permutation.create
     (List.map ~f:snd sorted_l_with_is)
  ,List.map ~f:fst sorted_l_with_is)
