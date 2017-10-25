open Core
open Util
open Algebra
open My_list_extensions

type 'a nonempty_tree = Node of ('a * ('a nonempty_tree list))
[@@deriving ord, show, hash]


type 'a tree =
  | NonemptyTree of 'a nonempty_tree
  | EmptyTree
[@@deriving ord, show, hash]


type 'a nonempty_normalized_tree =
    NNode of ('a * ('a nonempty_normalized_tree list) * Permutation.t)
[@@deriving ord, show, hash]

type 'a normalized_tree =
  | NonemptyNTree of 'a nonempty_normalized_tree
  | EmptyNTree
[@@deriving ord, show, hash]

module TreeOf
    (D:Data)
  : Data with type t = D.t tree =
struct
  type t = D.t tree
  [@@deriving ord, show, hash]
end


module UnorderedNonemptyTreeOf
    (D:Data)
  : Data with type t = D.t nonempty_tree =
struct
  type t = D.t nonempty_tree
  [@@deriving show, hash]

  let rec compare
      (Node n1:t)
      (Node n2:t)
    : int =
    pair_compare
      D.compare
      (ordered_partition_order compare)
      n1
      n2
end


module UnorderedTreeOf
    (D:Data) =
struct
  type t = D.t tree
  [@@deriving show, hash]

  type nonempty_t = D.t nonempty_tree

  let compare
      (t1:t)
      (t2:t)
    : int =
    let module NET = UnorderedNonemptyTreeOf(D) in
    begin match (t1,t2) with
      | (EmptyTree        , EmptyTree        ) -> 0
      | (EmptyTree        , NonemptyTree _   ) -> -1
      | (NonemptyTree _   , EmptyTree        ) -> 1
      | (NonemptyTree net1, NonemptyTree net2) ->
        NET.compare net1 net2
    end
end

module NonemptyNormalizedTreeOf
    (D:Data) =
struct
  type t = D.t nonempty_normalized_tree
  [@@deriving show, hash]

  let rec compare
      (NNode (l1,t1s,_):t)
      (NNode (l2,t2s,_):t)
    : int =
    pair_compare
      D.compare
      (compare_list ~cmp:compare)
      (l1,t1s)
      (l2,t2s)

  let rec from_nonempty_tree
      (Node (l,ts):UnorderedNonemptyTreeOf(D).t)
    : t =
    let normalized_ts = List.map ~f:from_nonempty_tree ts in
    let (p,sorted_normalized_ts) =
      sorting_and_sort
        ~cmp:compare
        normalized_ts
    in
    NNode (l,sorted_normalized_ts,p)
end

module NormalizedTreeOf
    (D:Data) =
struct
  type t = D.t normalized_tree
  [@@deriving show, hash]

  type nonempty_t = D.t nonempty_tree

  let compare
      (t1:t)
      (t2:t)
    : int =
    let module NENT = NonemptyNormalizedTreeOf(D) in
    begin match (t1,t2) with
      | (EmptyNTree         , EmptyNTree         ) -> 0
      | (EmptyNTree         , NonemptyNTree _    ) -> -1
      | (NonemptyNTree _    , EmptyNTree         ) -> 1
      | (NonemptyNTree nent1, NonemptyNTree nent2) ->
        NENT.compare nent1 nent2
    end

  let from_tree
      (t:UnorderedTreeOf(D).t)
    : t =
    let module NENT = NonemptyNormalizedTreeOf(D) in
    begin match t with
      | EmptyTree -> EmptyNTree
      | NonemptyTree n -> NonemptyNTree (NENT.from_nonempty_tree n)
    end
end
