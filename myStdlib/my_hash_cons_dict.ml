(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  Anders Miltner edited it a bit, and is very confused by licenses...   *)
(*  Ryan Beckett also edited it a bit, and might be confused by licenses  *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Core
open Util
open My_hash_cons

module HCDictOf(K:UIDData)(V:Data) = struct
  type key = K.t
  [@@deriving ord, show, hash]

  type value = V.t
  [@@deriving ord, show, hash]

  module CompareDict = My_dict.DictOf(K)(V)

  type t =
    | Empty
    | Leaf of key * value
    | Branch of int * int * t * t
  [@@deriving ord, show, hash]

  let empty = Empty

  let mk_leaf k v = Leaf (k,v)

  let mk_branch
      (i:int)
      (j:int)
      (l:t)
      (r:t)
    : t =
    begin match (l,r) with
      | (Empty,t) -> r
      | (t,Empty) -> l
      | (_,_)   -> Branch (i,j,l,r)
    end

  let zero_bit k m = phys_equal (k land m) 0

  let rec contains_key d k =
    begin match d with
    | Empty -> false
    | Leaf (j,_) -> phys_equal (K.uid k) (K.uid j)
    | Branch (_, m, l, r) ->
      contains_key (if zero_bit (K.uid k) m then l else r) k
    end

  let rec lookup d k =
    begin match d with
    | Empty -> None
    | Leaf (j,x) ->
      if K.uid k = K.uid j then Some x else None
    | Branch (_, m, l, r) -> lookup (if zero_bit (K.uid k) m then l else r) k
    end

  let lowest_bit x = x land (-x)

  let branching_bit p0 p1 = lowest_bit (p0 lxor p1)

  let mask p m = p land (m-1)

  let unsigned_lt n m = n >= 0 && (m < 0 || n < m)

  let join (p0,t0,p1,t1) =
    let m = branching_bit p0 p1 in
    if zero_bit p0 m then
      mk_branch (mask p0 m) m t0 t1
    else
      mk_branch (mask p0 m) m t1 t0

  let match_prefix k p m = phys_equal (mask k m) p

  let insert_or_combine
      ~combiner:(combiner:value -> value -> value)
      t
      k
      x =
    let rec ins d =
      begin match d with
        | Empty -> mk_leaf k x
        | Leaf (j,y) ->
	        if phys_equal (K.uid j) (K.uid k) then
	          mk_leaf k (combiner y x)
	        else
	          join (K.uid k, (mk_leaf k x), K.uid j, d)
        | Branch (p,m,t0,t1) ->
	        if match_prefix (K.uid k) p m then
	          if zero_bit (K.uid k) m then
	            mk_branch p m (ins t0) t1
	          else
	            mk_branch p m t0 (ins t1)
	        else
	          join (K.uid k, mk_leaf k x, p, d)
      end
    in
    ins t

  let insert = insert_or_combine ~combiner:(fun _ y -> y)

  let remove k t =
    let rec rmv d =
      begin match d with
      | Empty -> empty
      | Leaf (j,_) -> if phys_equal (K.uid k) (K.uid j) then empty else d
      | Branch (p,m,t0,t1) ->
	      if match_prefix (K.uid k) p m then
	        if zero_bit (K.uid k) m then
	          mk_branch p m (rmv t0) t1
	        else
	          mk_branch p m t0 (rmv t1)
	      else
	       d
      end
    in
    rmv t

  let as_kvp_list s =
    let rec as_kvp_list_internal acc t =
      match t with
      | Empty -> acc
      | Leaf (k,v) -> (k,v) :: acc
      | Branch (_,_,l,r) -> as_kvp_list_internal (as_kvp_list_internal acc r) l
    in
    as_kvp_list_internal [] s

  let rec merge_to_dict
      ~combiner:(combiner:value -> value -> value)
      (s:t)
      (t:t)
      : t =
    match (s,t) with
    | Empty, _  -> t
    | _, Empty  -> s
    | Leaf (k,v), _ -> insert_or_combine ~combiner:combiner t k v
    | _, Leaf (k,v) -> insert_or_combine ~combiner:combiner s k v
    | Branch (p,m,s0,s1), Branch (q,n,t0,t1) ->
        if phys_equal m n && match_prefix q p m then
          (* The trees have the same prefix. Merge the subtrees. *)
          mk_branch
            p
            m
            (merge_to_dict ~combiner:combiner s0 t0)
            (merge_to_dict ~combiner:combiner s1 t1)
        else if unsigned_lt m n && match_prefix q p m then
          (* [q] contains [p]. Merge [t] with a subtree of [s]. *)
          if zero_bit q m then
            mk_branch
              p
              m
              (merge_to_dict ~combiner:combiner s0 t)
              s1
          else
            mk_branch
              p
              m
              s0
              (merge_to_dict ~combiner:combiner s1 t)
        else if unsigned_lt n m && match_prefix p q n then
          (* [p] contains [q]. Merge [s] with a subtree of [t]. *)
          if zero_bit p n then
            mk_branch 
              q
              n
              (merge_to_dict ~combiner:combiner s t0)
              t1
          else
            mk_branch
              q
              n
              t0
              (merge_to_dict ~combiner:combiner s t1)
        else
          (* The prefixes disagree. *)
          join (p, s, q, t)

  let merge
      ~combiner:(combiner:value -> value -> 'a)
      ~only_d1_fn:(only_d1_fn:value -> 'a)
      ~only_d2_fn:(only_d2_fn:value -> 'a)
      (d1:t)
      (d2:t)
    : (key * 'a) list =
    let rec merge_ordered_lists
        (l1:(key * value) list)
        (l2:(key * value) list)
      : (key * 'a) list =
      begin match (l1,l2) with
        | (_,[]) ->
          List.map
            ~f:(fun (k,v) -> (k, only_d1_fn v))
            l1
        | ([],_) ->
          List.map
            ~f:(fun (k,v) -> (k,only_d2_fn v))
            l2
        | ((k1,v1)::t1,(k2,v2)::t2) ->
          begin match make_matchable (K.compare k1 k2) with
            | EQ -> (k1,combiner v1 v2)::(merge_ordered_lists t1 t2)
            | LT -> (k1,only_d1_fn v1)::(merge_ordered_lists t1 l2)
            | GT -> (k2,only_d2_fn v2)::(merge_ordered_lists l1 t2)
          end
      end
    in
    let ordered_d1_kvp_list =
      List.sort
        ~compare:(fun (k1,_) (k2,_) -> K.compare k1 k2)
        (as_kvp_list d1)
    in
    let ordered_d2_kvp_list =
      List.sort
        ~compare:(fun (k1,_) (k2,_) -> K.compare k1 k2)
        (as_kvp_list d2)
    in
    (merge_ordered_lists ordered_d1_kvp_list ordered_d2_kvp_list)

  let rec iter ~f:f d =
    begin match d with
    | Empty -> ()
    | Leaf (k,x) -> f k x
    | Branch (_,_,t0,t1) -> iter f t0; iter f t1
    end

  let rec map ~f:f d =
    begin match d with
      | Empty -> empty
      | Leaf (k,x) -> mk_leaf k (f x)
      | Branch (p,m,t0,t1) -> mk_branch p m (map f t0) (map f t1)
    end

  let rec mapi ~f:f d =
    begin match d with
      | Empty -> empty
      | Leaf (k,x) -> mk_leaf k (f k x)
      | Branch (p,m,t0,t1) -> mk_branch p m (mapi f t0) (mapi f t1)
    end

  let rec fold f s accu = match s with
    | Empty -> accu
    | Leaf (k,x) -> f k x accu
    | Branch (_,_,t0,t1) -> fold f t0 (fold f t1 accu)
end
