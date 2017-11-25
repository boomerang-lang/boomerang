open Core
open Util

module DictOf (K:Data) (V:Data) =
struct
  type key = K.t
  [@@deriving ord, show, hash]

  type value = V.t
  [@@deriving ord, show, hash]

  type pair = key * value
  [@@deriving ord, show, hash]

  type t = 
    | Leaf
    | Two of t * pair * t
    | Three of t * pair * t * pair * t
  [@@deriving ord, show, hash]

  (* A dictionary entry is a (key,value) pair. We compare two (key,value)
   * pairs with the provided key-comparison function D.compare. For example,
   * we may choose to keep a dictionary mapping links to their ranks. In this
   * case, our (key,value) pairs will be (link,rank) pairs, and we compare
   * links using string comparison. *)

  (* Type definition for dictionary, which we choose to represent as a 2-3 Tree.
   * This is almost the same as the binary search tree definition from pset4 and
   * lecture, except we add one more case: a Three-node. 
   *
   * A Three-node contains two pairs and three subtrees: left, middle, and 
   * right, represented by the 3 dicts in the definition below. *)

  (* INVARIANTS: 
   * 2-node: Two(left,(k1,v1),right) 
   * (1) Every key k appearing in subtree left must be k < k1.
   * (2) Every key k appearing in subtree right must be k > k1. 
   * (3) The length of the path from the 2-node to
   *     every leaf in its two subtrees must be the same.  
   * 
   * 3-node: Three(left,(k1,v1),middle,(k2,v2),right) 
   * (1) k1 < k2.
   * (2) Every key k appearing in subtree left must be k < k1. 
   * (3) Every key k appearing in subtree right must be k > k2. 
   * (4) Every key k appearing in subtree middle must be k1 < k < k2.
   * (5) The length of the path from the 3-node to every leaf in its three 
   *     subtrees must be the same. 
   *)

  (* FOR INSERTION:
   * A kicked configuration returned by going downwards on insertion.
   * We can only kick up Two nodes, hence Up takes a dict * pair * dict *)
  type kicked =
    | Up of t * pair * t
    | Done of t

  (* FOR REMOVAL:
   * A hole configuration returned by going downwards on removal. We
   * include a pair option whenever we remove the minimum of the right
   * subtree of the current pair in order the current pair *)
  type hole =
    | Hole of pair option * t
    | Absorbed of pair option * t

  (* FOR REMOVAL:
   * A direction will distinguish which configuration we came from in the
   * removal cases. We use direction2 for cases (1-2) on the handout, and
   * we use direction3 for cases (3-4) on the handout. *)
  type direction2 =
    | Left2
    | Right2

  type direction3 =
    | Left3
    | Mid3
    | Right3
        
  (* TODO:
   * How do we represent an empty dictionary with 2-3 trees? *)
  let empty : t = Leaf

  let rec fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: t) : 'a =
    match d with
      | Leaf -> u
      | Two(left,(k,v),right) ->
        let left_acc = fold f u left in
        let mid_acc = f k v left_acc in
        let right_acc = fold f mid_acc right in
        right_acc
      | Three(left,(k1,v1),middle,(k2,v2),right) ->
        let left_acc = fold f u left in
        let leftmid_acc = f k1 v1 left_acc in
        let mid_acc = fold f leftmid_acc middle in
        let rightmid_acc = f k2 v2 mid_acc in
        let right_acc = fold f rightmid_acc right in
        right_acc

  let rec map_values
      ~f:(f:value -> value)
      (d:t)
    : 'a =
    begin match d with
      | Leaf -> Leaf
      | Two(left,(k,v),right) ->
        Two(
          map_values ~f:f left,
          (k,f v),
          map_values ~f:f right)
      | Three(left,(k1,v1),middle,(k2,v2),right) ->
        Three(
          map_values ~f:f left,
          (k1,f v1),
          map_values ~f:f middle,
          (k2,f v2),
          map_values ~f:f right)
    end

  (* Upward phase for w where its parent is a Two node whose (key,value) is x.
   * One of x's children is w, and the other child is x_other. This function
   * should return a kicked-up configuration containing the new tree as a
   * result of performing the upward phase on w. *)
  let insert_upward_two (w: pair) (w_left: t) (w_right: t) 
      (x: pair) (x_other: t) : kicked = 
    let (w_key,_) = w in
    let (x_key,_) = x in
    let cmp = compare_key w_key x_key in
    if (is_equal cmp) then
      Done(Two(w_left,w,w_right))
    else if (is_lt cmp) then
      Done(Three(w_left,w,w_right,x,x_other))
    else
      Done(Three(x_other,x,w_left,w,w_right))

  (* Upward phase for w where its parent is a Three node whose (key,value) is x.
   * One of x's children is w, and of the two remaining children, 
   * other_left is the subtree more to the left and other_right is the 
   * subtree more to the right. 
   *
   * E.g. From our handout, for the first case where w's parent is a Three-tree,
   * other_left would be c and other_right would be d. For the second case,
   * other_left would be a and other_right would be d. For the third case,
   * other_left would be a and other_right would be b. 
   *
   * This function should return a kicked-up configuration containing the 
   * new tree as a result of performing the upward phase on w. *)
  let insert_upward_three (w: pair) (w_left: t) (w_right: t)
      (x: pair) (y: pair) (other_left: t) (other_right: t) : kicked =
    let (w_key,_) = w in
    let (x_key,_) = x in
    let (y_key,_) = y in
    match
      make_matchable (compare_key w_key x_key),
      make_matchable (compare_key w_key y_key) with
      | EQ, _ -> Done(Three(w_left,x,other_left,y,other_right))
      | _, EQ -> Done(Three(w_left,x,other_left,y,other_right))
      | LT, _ -> 
        let left = Two(w_left,w,w_right) in
        let right = Two(other_left,y,other_right) in
        Up(left,x,right)
      | _, GT -> 
        let left = Two(other_left,x,other_right) in
        let right = Two(w_left,w,w_right) in
        Up(left,y,right)
      | GT, LT -> 
        let left = Two(other_left,x,w_left) in
        let right = Two(w_right,y,other_right) in
        Up(left,w,right)

  (* Downward phase for inserting (k,v) into our dictionary d. 
   * The downward phase returns a "kicked" up configuration, where
   * 
   * type kicked =
   *      | Up of dict * pair * dict
   *      | Done of dict
   * 
   * A kicked up configuration can only be a Two node, hence the Up
   * constructor takes the same parameters as the Two constructor. We return
   * Up(left,(k,v),right) if the Two-node represented by this Up needs to
   * be further kicked up in the upward phase (this is represented by an up
   * arrow on the 2-3 Tree handout). We return Done(d) if we have finished
   * our upward phase on the tree represented by d. 
   *
   * The functions insert_downward, insert_downward_two, and 
   * insert_downward_three are __mutually recursive__, hence the 
   * "let rec" and the "and" keywords. Here, we use three mutually recursive
   * functions to simplify our code into smaller pieces.
   *
   * Two functions f and g are __mutually recursive__ if in f's definition, 
   * f calls g, and in g's definition, g calls f. This definition of
   * mutually recursive definitions can be extended to more than two functions,
   * as follows: 
   * 
   * Functions f1, f2, f3, ..., fn are mutually recursive if for each of
   * these functions f, all of the other f_i's can be called on some execution 
   * of f. *)

  (* insert_downward should handle the base case when inserting into a Leaf,
   * and if our dictionary d is a Two-node or a Three-node, we call the 
   * corresponding functions insert_downward_two or insert_downward_three
   * with the appropriate arguments. *)
  let rec insert_downward
      (merge:value -> value -> value)
      (d: t)
      (k: key)
      (v: value)
    : kicked =
    match d with
      | Leaf -> Up(Leaf,(k,v),Leaf)
      | Two(left,n,right) -> 
        insert_downward_two merge (k,v) n left right
      | Three(left,n1,middle,n2,right) -> 
        insert_downward_three merge (k,v) n1 n2 left middle right

  (* Downward phase on a Two node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) is the (key,value) of the current Two node, and left and right
   * are the two subtrees of the current Two node. *)
  and insert_downward_two
      (merge:value -> value -> value)
      ((k,v): pair)
      ((k1,v1): pair)
      (left: t)
      (right: t)
    : kicked =
    let cmp = compare_key k k1 in
    if is_equal cmp then
      Done(Two(left,(k1,merge v1 v),right))
    else if is_lt cmp then
      begin match insert_downward merge left k v with
        | Up(l_kick,w,r_kick) -> 
          insert_upward_two w l_kick r_kick (k1,v1) right
        | Done new_left -> Done(Two(new_left,(k1,v1),right))
      end
    else
      begin match insert_downward merge right k v with
        | Up(l_kick,w,r_kick) -> 
          insert_upward_two w l_kick r_kick (k1,v1) left
        | Done new_right -> Done(Two(left,(k1,v1),new_right))
      end


  (* Downward phase on a Three node. (k,v) is the (key,value) we are inserting,
   * (k1,v1) and (k2,v2) are the two (key,value) pairs in our Three node, and
   * left, middle, and right are the three subtrees of our current Three node *)
  and insert_downward_three
      (merge:value -> value -> value)
      ((k,v): pair)
      ((k1,v1): pair)
      ((k2,v2): pair)
      (left: t)
      (middle: t)
      (right: t)
    : kicked =
    match
      make_matchable (compare_key k k1),
      make_matchable (compare_key k k2) with
      | EQ, _ -> Done(Three(left,(k1,merge v1 v),middle,(k2,v2),right))
      | _, EQ -> Done(Three(left,(k1,v1),middle,(k2,merge v2 v),right))
      | LT, _ -> 
        (match insert_downward merge left k v with
          | Up(l_kick,w,r_kick) ->
            insert_upward_three w l_kick r_kick (k1,v1) (k2,v2) middle right
          | Done new_left -> Done(Three(new_left,(k1,v1),middle,(k2,v2),right))
        )
      | _, GT ->
        (match insert_downward merge right k v with
          | Up(l_kick,w,r_kick) ->
            insert_upward_three w l_kick r_kick (k1,v1) (k2,v2) left middle
          | Done new_right -> Done(Three(left,(k1,v1),middle,(k2,v2),new_right))
        )
      | GT, LT ->
        (match insert_downward merge middle k v with
          | Up(l_kick,w,r_kick) ->
            insert_upward_three w l_kick r_kick (k1,v1) (k2,v2) left right
          | Done new_mid -> Done(Three(left,(k1,v1),new_mid,(k2,v2),right))
        )   

  (* We insert (k,v) into our dict using insert_downward, which gives us
   * "kicked" up configuration. We return the tree contained in the "kicked"
   * configuration. *)
  let insert_or_merge
      ~merge:(merge: value -> value -> value)
      (d: t)
      (k: key)
      (v: value)
    : t =
    match insert_downward merge d k v with
      | Up(l,(k1,v1),r) -> Two(l,(k1,v1),r)
      | Done x -> x

  let insert : t -> key -> value -> t =
    insert_or_merge ~merge:(fun _ n -> n)

  (* Upward phase for removal where the parent of the hole is a Two node. 
   * See cases (1-2) on the handout. n is the (key,value) pair contained in
   * the parent node; left and right are the subtrees of the parent node (our
   * hole is one of these subtrees); and dir indicates which subtree was
   * contained by the hole. *)
  let remove_upward_two (n: pair) (rem: pair option) 
      (left: t) (right: t) (dir: direction2) : hole =
    match dir,n,left,right with
      | Left2,x,l,Two(m,y,r) -> Hole(rem,Three(l,x,m,y,r))
      | Right2,y,Two(l,x,m),r -> Hole(rem,Three(l,x,m,y,r))
      | Left2,x,a,Three(b,y,c,z,d) -> Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Right2,z,Three(a,x,b,y,c),d -> Absorbed(rem,Two(Two(a,x,b),y,Two(c,z,d)))
      | Left2,_,_,_ | Right2,_,_,_ -> Absorbed(rem,Two(Leaf,n,Leaf))

  (* Upward phase for removal where the parent of the hole is a Three node.
   * See cases (3-4) on the handout. n1 and n2 are the (key,value) pairs
   * contained in the parent node; left, middle, and right are the subtrees
   * of the parent node (our hole is one of these subtrees); and dir indicates
   * which subtree was the tree contained by the hole. *)
  let remove_upward_three (n1: pair) (n2: pair) (rem: pair option)
      (left: t) (middle: t) (right: t) (dir: direction3) : hole =
    match dir,n1,n2,left,middle,right with
      | Left3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,y,z,Two(a,x,b),c,d -> Absorbed(rem,Two(Three(a,x,b,y,c),z,d))
      | Mid3,x,y,a,b,Two(c,z,d) -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Right3,x,z,a,Two(b,y,c),d -> Absorbed(rem,Two(a,x,Three(b,y,c,z,d)))
      | Left3,w,z,a,Three(b,x,c,y,d),e -> 
        Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,y,z,Three(a,w,b,x,c),d,e -> 
        Absorbed(rem,Three(Two(a,w,b),x,Two(c,y,d),z,e))
      | Mid3,w,x,a,b,Three(c,y,d,z,e) ->
        Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Right3,w,z,a,Three(b,x,c,y,d),e ->
        Absorbed(rem,Three(a,w,Two(b,x,c),y,Two(d,z,e)))
      | Left3,_,_,_,_,_ | Mid3,_,_,_,_,_ | Right3,_,_,_,_,_ ->
        Absorbed(rem,Three(Leaf,n1,Leaf,n2,Leaf))

  (* DO NOT EDIT THIS *)
  let rec remove_downward (updater:value -> value option) (d: t) (k: key) : hole =
    match d with
      | Leaf -> Absorbed(None,d)
      | Two(Leaf,(k1,v1),Leaf) ->
        if is_equal (compare_key k k1) then
          let rem = Some (k1,v1) in
          begin match updater v1 with
            | None -> Hole(rem,Leaf)
            | Some v1 -> Absorbed(rem,Two(Leaf,(k1,v1),Leaf))
          end
        else
          Absorbed(None,d)
      | Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf) ->
        (begin match
            make_matchable (compare_key k k1),
            make_matchable (compare_key k k2) with
        | EQ, _ ->
          let rem = Some (k1,v1) in
          begin match updater v1 with
            | None -> Absorbed(rem,Two(Leaf,(k2,v2),Leaf))
            | Some v1 -> Absorbed(rem,Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf))
          end
        | _, EQ ->
          let rem = Some (k2,v2) in
          begin match updater v2 with
            | None -> Absorbed(rem,Two(Leaf,(k1,v1),Leaf))
            | Some v2 -> Absorbed(rem,Three(Leaf,(k1,v1),Leaf,(k2,v2),Leaf))
          end
        | _, _ -> Absorbed(None,d)
        end
        )
      | Two(l,n,r) -> remove_downward_two updater k n l r
      | Three(l,n1,m,n2,r) -> remove_downward_three updater k n1 n2 l m r

  (* DO NOT EDIT THIS *)
  and remove_downward_two
      (updater:value -> value option)
      (k: key)
      ((k1,v1): pair)
      (left: t)
      (right: t)
    : hole =
    let cmp = compare_key k k1 in
    if is_equal cmp then
      let rem = Some (k1,v1) in
      begin match updater v1 with
        | None ->
          begin match remove_min right with
            | Hole(None,_) ->
              Hole(rem,left)
            | Hole(Some n,new_right) -> 
              remove_upward_two n rem left new_right Right2
            | Absorbed(None,_) ->
              Hole(rem,left)
            | Absorbed(Some n,new_right) ->
              Absorbed(rem,Two(left,n,new_right))
          end
        | Some v1 ->
          Absorbed(None,Two(left,(k1,v1),right))
      end
    else if is_lt cmp then
      begin match remove_downward updater left k with
        | Hole(rem,t) ->
          remove_upward_two (k1,v1) rem t right Left2
        | Absorbed(rem,t) ->
          Absorbed(rem,Two(t,(k1,v1),right))
      end
    else
        begin match remove_downward updater right k with
          | Hole(rem,t) ->
            remove_upward_two (k1,v1) rem left t Right2
          | Absorbed(rem,t) ->
            Absorbed(rem,Two(left,(k1,v1),t))
        end

  (* DO NOT EDIT THIS *)
  and remove_downward_three (updater:value -> value option) (k: key) ((k1,v1): pair) ((k2,v2): pair)
      (left: t) (middle: t) (right: t) : hole =
    match
      make_matchable (compare_key k k1),
      make_matchable (compare_key k k2) with
    | EQ, _ ->
      let rem = Some (k1,v2) in
      begin match updater v1 with
        | None ->
          begin match remove_min middle with
            | Hole(None,_) ->
              Hole(rem,Two(left,(k2,v2),right))
            | Hole(Some n,new_middle) -> 
              remove_upward_three n (k2,v2) rem left new_middle right Mid3
            | Absorbed(None,_) ->
              Absorbed(rem,Two(left,(k1,v1),right))
            | Absorbed(Some n,new_middle) -> 
              Absorbed(rem,Three(left,n,new_middle,(k2,v2),right))
          end
        | Some v1 ->
          Absorbed(None,Three(left,(k1,v1),middle,(k2,v2),right))
      end
    | _ , EQ ->
      let rem = Some (k2,v2) in
      begin match updater v2 with
        | None ->
          begin match remove_min right with
            | Hole(None,_) -> Hole(rem,Two(left,(k1,v1),middle))
            | Hole(Some n,new_right) -> 
              remove_upward_three (k1,v1) n rem left middle new_right Right3
            | Absorbed(None,_) -> Absorbed(rem,Two(left,(k1,v1),middle))
            | Absorbed(Some n,new_right) -> 
              Absorbed(None,Three(left,(k1,v1),middle,n,new_right))
          end
        | Some v2' ->
          Absorbed(rem,Three(left,(k1,v1),middle,(k2,v2'),right))
      end
    | LT, _ ->
      begin match remove_downward updater left k with
        | Hole(rem,t) -> 
          remove_upward_three (k1,v1) (k2,v2) rem t middle right Left3
        | Absorbed(rem,t) -> 
          Absorbed(rem,Three(t,(k1,v1),middle,(k2,v2),right))
      end
    | _, GT ->
      begin match remove_downward updater right k with
        | Hole(rem,t) -> 
          remove_upward_three (k1,v1) (k2,v2) rem left middle t Right3
        | Absorbed(rem,t) -> 
          Absorbed(rem,Three(left,(k1,v1),middle,(k2,v2),t))
      end
    | GT, LT ->
      begin match remove_downward updater middle k with
        | Hole(rem,t) -> 
          remove_upward_three (k1,v1) (k2,v2) rem left t right Mid3
        | Absorbed(rem,t) -> 
          Absorbed(rem,Three(left,(k1,v1),t,(k2,v2),right))
      end

  (* DO NOT EDIT THIS *)
  and remove_min (d: t) : hole =
    match d with
    | Leaf -> Hole(None,Leaf)
    | Two(Leaf,n,_) -> Hole(Some n,Leaf)
    | Three(Leaf,n1,middle,n2,right) -> Absorbed(Some n1,Two(middle,n2,right))
    | Two(left,n,right) -> 
      (match remove_min left with
       | Hole(rem,t) -> remove_upward_two n rem t right Left2
       | Absorbed(rem,t) -> Absorbed(rem,Two(t,n,right))
      )
    | Three(left,n1,middle,n2,right) ->
        (match remove_min left with
          | Hole(rem,t) -> remove_upward_three n1 n2 rem t middle right Left3
          | Absorbed(rem,t) -> Absorbed(rem,Three(t,n1,middle,n2,right))
        )

  let remove_or_update_and_retrieve_old
      ~updater:(updater:value -> value option)
      (d: t)
      (k: key)
    : (value option * t) =
    begin match remove_downward updater d k with
      | Hole(kvo,d') -> (Option.map ~f:snd kvo,d')
      | Absorbed(kvo,d') -> (Option.map ~f:snd kvo,d')
    end

  let remove_or_update_and_retrieve_old_exn
      ~updater:(updater:value -> value option)
      (d: t)
      (k: key)
    : (value * t) =
    let (vo,t) = remove_or_update_and_retrieve_old ~updater:updater d k in
    (Option.value_exn vo, t)

  let remove_or_update ~updater:(updater:value -> value option) (d: t) (k: key) : t =
    snd (remove_or_update_and_retrieve_old ~updater:updater d k)

  (* DO NOT EDIT THIS *)
  let remove : t -> key -> t =
    remove_or_update ~updater:(fun _ -> None)

  (* TODO:
   * Write a lookup function that returns the value of the given key
   * in our dictionary and returns it as an option, or return None
   * if the key is not in our dictionary. *)
  let rec lookup (d: t) (k: key) : value option =
    match d with
      | Leaf -> None
      | Two(left,(k1,v1),right) ->
        let cmp = compare_key k k1 in
        if is_equal cmp then
          Some v1
        else if is_lt cmp then
          lookup left k
        else
          lookup right k
      | Three(left,(k1,v1),middle,(k2,v2),right) ->
        (match
           make_matchable (compare_key k k1),
           make_matchable (compare_key k k2) with
          | EQ, _ -> Some v1
          | _, EQ -> Some v2
          | LT, _ -> lookup left k
          | _, GT -> lookup right k
          | GT, LT -> lookup middle k)

  let lookup_default ~default:(default:value) (d:t) (k:key) : value =
    begin match lookup d k with
      | None -> default
      | Some v -> v
    end


  let lookup_exn (d:t) (k:key) : value =
    begin match lookup d k with
      | Some v -> v
      | None -> failwith ("lookup_exn: " ^ (show_key k) ^ " not found")
    end

  (* TODO:
   * Write a function to test if a given key is in our dictionary *)
  let member (d: t) (k: key) : bool =
    lookup d k <> None

  (* TODO:
   * Write a function that removes any (key,value) pair from our 
   * dictionary (your choice on which one to remove), and returns
   * as an option this (key,value) pair along with the new dictionary. 
   * If our dictionary is empty, this should return None. *)
  let choose (d: t) : (key * value * t) option =
    match d with
      | Leaf -> None
      | Two(_,(k,v),_) -> Some (k,v,remove d k)
      | Three(_,(k,v),_,_,_) -> Some (k,v,remove d k)

  let from_kvp_list
      (l:(key * value) list)
    : t =
    List.fold_left
      ~f:(fun d (k,v) ->
          insert d k v)
      ~init:empty
      l

  let singleton
      (k:key)
      (v:value)
    : t =
    insert
      empty
      k
      v

  let is_empty
      (d:t)
    : bool =
    begin match d with
      | Leaf -> true
      | _ -> false
    end

  let as_kvp_list
      (d:t)
    : (key * value) list =
    fold
      (fun k v l -> (k,v)::l)
      []
      d

  let key_list
      (d:t)
    : key list =
    List.map
      ~f:fst
      (as_kvp_list d)

  let value_list
      (d:t)
    : value list =
    List.map
      ~f:snd
      (as_kvp_list d)

  let compare
      (d1:t)
      (d2:t)
    : comparison =
    let comparer = (pair_compare compare_key V.compare) in
    compare_list
      ~cmp:comparer
      (List.sort ~cmp:comparer (as_kvp_list d1))
      (List.sort ~cmp:comparer (as_kvp_list d2))

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
          begin match make_matchable (compare_key k1 k2) with
            | EQ -> (k1,combiner v1 v2)::(merge_ordered_lists t1 t2)
            | LT -> (k1,only_d1_fn v1)::(merge_ordered_lists t1 l2)
            | GT -> (k2,only_d2_fn v2)::(merge_ordered_lists l1 t2)
          end
      end
    in
    let ordered_d1_kvp_list = List.rev (as_kvp_list d1) in
    let ordered_d2_kvp_list = List.rev (as_kvp_list d2) in
    (merge_ordered_lists ordered_d1_kvp_list ordered_d2_kvp_list)

  let merge_to_dict
      ?only_d1_fn:(only_d1_fn:value -> value = ident)
      ?only_d2_fn:(only_d2_fn:value -> value = ident)
      ~combiner:(combiner:value -> value -> value)
      (d1:t)
      (d2:t)
    : t =
    from_kvp_list
      (merge
         ~combiner:combiner
         ~only_d1_fn:only_d1_fn
         ~only_d2_fn:only_d2_fn
         d1
         d2)

  let rec max_key
      (d:t)
    : key option =
    begin match d with
      | Leaf -> None
      | Two (_,(k,_),Leaf) -> Some k
      | Two (_,_,right) -> max_key right
      | Three (_,_,_,(k,_),Leaf) -> Some k
      | Three (_,_,_,_,right) -> max_key right
    end

  let max_key_exn
      (d:t)
    : key =
    Option.value_exn (max_key d)

  let rec size
    : t -> int =
    fold
      (fun _ _ acc -> acc+1)
      0
end


