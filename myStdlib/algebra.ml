open Core
open Util
open My_dict
open My_set
open String_utilities

module Probability =
struct
  type t = Float.t
  [@@deriving ord, show, hash]

  (* not transitive *)
  (* http://floating-point-gui.de/errors/comparison/ *)
  let equal
      (f1:t)
      (f2:t)
    : bool =
    let abs1 = Float.abs f1 in
    let abs2 = Float.abs f2 in
    let diff = Float.abs (f1 -. f2) in
    let min_normal = 0.00000000000000000000001 in
    let epsilon = 0.000001 in
    if Float.equal f1 f2 then
      true
    else if (Float.equal f1 0.)
         || (Float.equal f1 0.)
         || diff <. min_normal then
      diff <. (epsilon *. min_normal)
    else
      diff /. (Float.min (abs1 +. abs2) Float.max_finite_value) <. epsilon

  (* not transitive *)
  let compare
      (f1:t)
      (f2:t)
    : int =
    if equal f1 f2 then
      0
    else
      compare f1 f2

  let not
      (p:t)
    : t =
    1. -. p

  let information_content
      (p:t)
    : Float.t =
    Float.neg (Math.log2 p)
end

module Semiring =
struct 
  module type Sig =
  sig
    type t
    val apply_at_every_level : (t -> t) -> t -> t
    val applies_for_every_applicable_level : (t -> t option) -> t -> t list
    val zero : t
    val one : t
    val separate_plus : t -> (t * t) option
    val separate_times : t -> (t * t) option
    val make_plus : t -> t -> t
    val make_times : t -> t -> t
  end

  let maximally_factor_element
      (type t)
      (module S : Sig with type t = t)
      ~is_eq:(is_eq:t -> t -> bool)
    : S.t -> S.t =
    let rec separate_into_sum
        (r:S.t)
      : S.t list =
      begin match S.separate_plus r with
        | None -> [r]
        | Some (r1,r2) -> (separate_into_sum r1) @ (separate_into_sum r2)
      end
    in
    let rec separate_into_product
        (r:S.t)
      : S.t list =
      begin match S.separate_times r with
        | None -> [r]
        | Some (r1,r2) -> (separate_into_product r1) @ (separate_into_product r2)
      end
    in
    let combine_nonempty_list_exn
        (combiner:S.t -> S.t -> S.t)
        (rl:S.t list)
      : S.t =
      let (rlf,rll) = split_by_last_exn rl in
      List.fold_right
        ~f:(fun r acc ->
            combiner r acc)
        ~init:rll
        rlf
    in
    let combine_list
        (combiner:S.t -> S.t -> S.t)
        (rl:S.t list)
      : S.t option =
      begin match rl with
        | [] -> None
        | _ -> Some (combine_nonempty_list_exn combiner rl)
      end
    in
    let maximally_factor_current_level
        (product_splitter:S.t list -> (S.t * S.t list))
        (product_combiner:S.t -> S.t -> S.t)
        (he:S.t)
      : S.t =
      let sum_list = separate_into_sum he in
      let sum_product_list_list =
        List.map
          ~f:separate_into_product
          sum_list
      in
      let product_keyed_sum_list =
        List.map
          ~f:product_splitter
          sum_product_list_list
      in
      let grouped_assoc_list =
        group_by_keys
          ~is_eq:is_eq
          product_keyed_sum_list
      in
      let keyed_sum_list =
        List.map
          ~f:(fun (k,all) ->
              let producted_elements =
                List.map
                  ~f:(fun pl ->
                      begin match combine_list S.make_times pl with
                        | None -> S.one
                        | Some he -> he
                      end)
                  all
              in
              (k,producted_elements))
          grouped_assoc_list
      in
      let ringed_list =
        List.map
          ~f:(fun (k,al) ->
              let factored_side = combine_nonempty_list_exn S.make_plus al in
              if factored_side = S.one then
                k
              else
                product_combiner
                  k
                  factored_side)
          keyed_sum_list
      in
      combine_nonempty_list_exn S.make_plus ringed_list
    in
    Fn.compose
      (fold_until_fixpoint
        ~is_eq:is_eq
         (S.apply_at_every_level
            (maximally_factor_current_level
               (Fn.compose swap_double split_by_last_exn)
               (Fn.flip S.make_times))))
      (fold_until_fixpoint
        ~is_eq:is_eq
         (S.apply_at_every_level
            (maximally_factor_current_level
               split_by_first_exn
               S.make_times)))
end

module StochasticStarSemiring =
struct
  module type Sig =
  sig
    type t
    val apply_at_every_level : (t -> t) -> t -> t
    val applies_for_every_applicable_level : (t -> t option) -> t -> t list
    val zero : t
    val one : t
    val separate_star : t -> (t * Probability.t) option
    val make_plus : t -> t -> Probability.t -> t
    val make_times : t -> t -> t
    val make_star : t -> Probability.t -> t
  end

  let unfold_left
      (type t)
      (module S : Sig with type t = t)
      (r:S.t)
      (p:Probability.t)
    : S.t =
    S.make_plus
      S.one
      (S.make_times
         r
         (S.make_star r p))
      p

  let unfold_right
      (type t)
      (module S : Sig with type t = t)
      (r:S.t)
      (p:Probability.t)
    : S.t =
    S.make_plus
      S.one
      (S.make_times
         (S.make_star r p)
         r)
      p

  let unfold_left_if_star
      (type t)
      (module S : Sig with type t = t)
      (v:S.t)
    : S.t option =
    Option.map
      ~f:(uncurry (unfold_left (module S : Sig with type t = t)))
      (S.separate_star v)

  let unfold_right_if_star
      (type t)
      (module S : Sig with type t = t)
      (v:S.t)
    : S.t option =
    Option.map
      ~f:(uncurry (unfold_right (module S : Sig with type t = t)))
      (S.separate_star v)

  let left_unfold_all_stars
      (type t)
      (module S : Sig with type t = t)
      (v:S.t)
    : S.t list =
    let ssr = (module S : Sig with type t = t) in
    S.applies_for_every_applicable_level
      (unfold_left_if_star ssr)
      v

  let right_unfold_all_stars
      (type t)
      (module S : Sig with type t = t)
      (v:S.t)
    : S.t list =
    let ssr = (module S : Sig with type t = t) in
    S.applies_for_every_applicable_level
      (unfold_right_if_star ssr)
      v
end



module Permutation = struct
  module IntIntDict = DictOf(IntModule)(IntModule)

  type t =
    {
      forward : IntIntDict.t ;
      reverse : IntIntDict.t ;
    }
  [@@deriving show, hash, ord]

  let create_dict_of_pairs
      (len:int)
      (mapping:(int * int) list)
    : IntIntDict.t =
    List.fold_left
      ~f:(fun acc (x,y) ->
          if ((IntIntDict.member acc x) || (x >= len) || (x < 0)) then
            failwith ("Not Bijection "
                      ^ (string_of_list
                           (string_of_pair
                              string_of_int
                              string_of_int)
                           mapping))
          else
            IntIntDict.insert acc x y)
      ~init:IntIntDict.empty
      mapping

  let create_from_pairs
      (mapping:(int * int) list)
    : t =
    let len = List.length mapping in
    let reverse_mapping = List.map ~f:(fun (x,y) -> (y,x)) mapping in
    {
      forward = create_dict_of_pairs len mapping;
      reverse = create_dict_of_pairs len reverse_mapping;
    }

  let create
      (mapping:int list)
    : t =
    let pair_mapping = List.mapi ~f:(fun i x -> (i,x)) mapping in
    create_from_pairs pair_mapping

  let inverse
      (p:t)
    : t =
    {
      forward = p.reverse ;
      reverse = p.forward ;
    }

  let identity
      (n:int)
    : t =
    create (range 0 n)

  let apply_exn
      (permutation:t)
      (n:int)
    : int =
    IntIntDict.lookup_exn permutation.forward n

  let apply_inverse_exn
      (permutation:t)
      (n:int)
    : int =
    IntIntDict.lookup_exn permutation.reverse n

  let apply_to_list_exn (permutation:t) (l:'a list) : 'a list =
    let perm_pos_l = List.mapi ~f:(fun i x -> (apply_exn permutation i,x)) l in
    List.map
      ~f:snd
      (List.sort
         ~compare:(fun (i1,_) (i2,_) -> Int.compare i1 i2)
         perm_pos_l)

  let apply_inverse_to_list_exn (permutation:t) (l:'a list) : 'a list =
    let perm_pos_l =
      List.mapi
        ~f:(fun i x -> (apply_inverse_exn permutation i,x))
        l
    in
    List.map
      ~f:snd
      (List.sort
         ~compare:(fun (i1,_) (i2,_) -> Int.compare i1 i2)
         perm_pos_l)

  let size
      (p:t)
    : int =
    IntIntDict.size p.forward

  type swap_concat_compose_tree =
    | SCCTSwap of swap_concat_compose_tree * swap_concat_compose_tree
    | SCCTConcat of swap_concat_compose_tree * swap_concat_compose_tree
    | SCCTCompose of swap_concat_compose_tree * swap_concat_compose_tree
    | SCCTLeaf
  [@@deriving ord, show, hash]

  let rec size_scct (scct:swap_concat_compose_tree) : int =
    begin match scct with
      | SCCTSwap (s1,s2) -> (size_scct s1) + (size_scct s2)
      | SCCTConcat (s1,s2) -> (size_scct s1) + (size_scct s2)
      | SCCTCompose (s1,s2) -> max (size_scct s1) (size_scct s2)
      | SCCTLeaf -> 1
    end

  let rec has_compose (scct:swap_concat_compose_tree) : bool =
    begin match scct with
      | SCCTSwap (s1,s2) -> (has_compose s1) || (has_compose s2)
      | SCCTConcat (s1,s2) -> (has_compose s1) || (has_compose s2)
      | SCCTCompose _ -> true
      | SCCTLeaf -> false
    end

  let rec pp_swap_concat_compose_tree (scct:swap_concat_compose_tree)
    : string =
    begin match scct with
      | SCCTSwap (scct1,scct2) -> "swap ("
                                  ^ (pp_swap_concat_compose_tree scct1)
                                  ^ ","
                                  ^ (pp_swap_concat_compose_tree scct2)
                                  ^ ")"
      | SCCTConcat (scct1,scct2) -> "concat ("
                                    ^ (pp_swap_concat_compose_tree scct1)
                                    ^ ","
                                    ^ (pp_swap_concat_compose_tree scct2)
                                    ^ ")"
      | SCCTCompose (scct1,scct2) -> "compose ("
                                     ^ (pp_swap_concat_compose_tree scct1)
                                     ^ ","
                                     ^ (pp_swap_concat_compose_tree scct2)
                                     ^ ")"
      | SCCTLeaf -> "."
    end

  let as_int_list
      (perm:t)
    : int list =
    List.map
      ~f:snd
      (List.sort
         ~compare:(fun (i1,_) (i2,_) -> Int.compare i1 i2)
         (IntIntDict.as_kvp_list perm.reverse))

  let to_swap_concat_compose_tree
      (p:t)
    : swap_concat_compose_tree =
    let rec to_swap_concat_compose_tree_internal (l:int list) : swap_concat_compose_tree =
      let stupidconcat (l:int list) : swap_concat_compose_tree =
        let (_,t) = split_by_first_exn l in
        List.fold_left
          ~f:(fun acc _ ->
              SCCTConcat (acc,SCCTLeaf))
          ~init:SCCTLeaf
          t
      in
      if List.length l = 0 then
        failwith "bad input"
      else if List.length l = 1 then
        SCCTLeaf
      else
        let valid_split =
          List.fold_left
            ~f:(fun acc i ->
                begin match acc with
                  | None ->
                    let (l1,l2) = split_at_index_exn l i in
                    if pairwise_maintain_invariant (<) l1 l2 then
                      Some (i,true)
                    else if pairwise_maintain_invariant (>) l1 l2 then
                      Some (i,false)
                    else
                      None
                  | _ -> acc
                end)
            ~init:None
            (range 1 (List.length l)) in
        begin match valid_split with
          | None ->
            let (_,i) =
              List.foldi
                ~f:(fun i' (acc,i) x ->
                    if acc > x then
                      (acc,i)
                    else
                      (x,i'))
                ~init:(-1,-1)
                l in
            let (l1,l2) = split_at_index_exn l i in
            let (h,t) = split_by_first_exn l2 in
            SCCTCompose
              (to_swap_concat_compose_tree_internal (l1@t@[h])
              ,SCCTConcat
                  (stupidconcat l1
                  ,SCCTSwap
                      (SCCTLeaf
                      ,stupidconcat t)))
          | Some (i,b) ->
            let (l1,l2) = split_at_index_exn l i in
            if b then
              SCCTConcat
                (to_swap_concat_compose_tree_internal l1
                ,to_swap_concat_compose_tree_internal l2)
            else
              SCCTSwap
                (to_swap_concat_compose_tree_internal l2
                ,to_swap_concat_compose_tree_internal l1)
        end
    in
    to_swap_concat_compose_tree_internal (as_int_list p)
end


module CountedPermutation =
struct
  type element =
    {
      old_index : int       ;
      new_index : int * int ;
    }
  [@@deriving ord, show, hash, make]

  type t = element list
  [@@deriving ord, show, hash]

  let apply_exn
      (p:t)
      (i:int)
    : int * int =
    let e =
      List.find_exn
        ~f:(fun e -> e.old_index = i)
        p
    in
    e.new_index

  let apply_inverse_exn
      (p:t)
      (ij:int * int)
    : int =
    let e_option =
      List.find
        ~f:(fun e -> e.new_index = ij)
        p
    in
    begin match e_option with
      | None -> failwith
                  (string_of_int (fst ij)
                   ^ ","
                   ^ string_of_int (snd ij)
                   ^ "\n"
                   ^ show p)
      | Some e -> e.old_index
    end

  let sorting
      ~cmp:(cmp:'a comparer)
      (l:'a list)
    : t * 'a list list =
    let indexed_l =
      List.mapi
        ~f:(fun i x -> (x,i))
        l
    in
    let sorted_partitioned_indexed_l =
      sort_and_partition
        ~cmp:(fun (x1,_) (x2,_) -> cmp x1 x2)
        indexed_l
    in
    let (unflattened_p,sorted_partitioned_l) =
      List.unzip
        (List.mapi
           ~f:(fun i xl ->
               List.unzip
                 (List.mapi
                    ~f:(fun j (x,old_index) ->
                        (make_element
                           ~old_index:old_index
                           ~new_index:(i,j)
                        ,x)
                      )
                    xl))
           sorted_partitioned_indexed_l)
    in
    (List.concat unflattened_p,sorted_partitioned_l)
end

module StarSemiring =
struct
  module type Sig =
  sig
    type t
    val apply_at_every_level : (t -> t) -> t -> t
    val applies_for_every_applicable_level : (t -> t option) -> t -> t list
    val zero : t
    val one : t
    val separate_star : t -> t option
    val make_plus : t -> t -> t
    val make_times : t -> t -> t
    val make_star : t -> t
  end

  let unfold_left_if_star
      (type t)
      (module S : Sig with type t = t)
      (v:S.t)
    : S.t option =
    Option.map
      ~f:(fun r' ->
          S.make_plus
            S.one
            (S.make_times
               r'
               (S.make_star r')))
      (S.separate_star v)

  let unfold_right_if_star
      (type t)
      (module S : Sig with type t = t)
      (v:S.t)
    : S.t option =
    Option.map
      ~f:(fun r' ->
          S.make_plus
            S.one
            (S.make_times
               (S.make_star r')
               r'))
      (S.separate_star v)

  let left_unfold_all_stars
      (type t)
      (module S : Sig with type t = t)
      (v:S.t)
    : S.t list =
    let ssr = (module S : Sig with type t = t) in
    S.applies_for_every_applicable_level
      (unfold_left_if_star ssr)
      v

  let right_unfold_all_stars
      (type t)
      (module S : Sig with type t = t)
      (v:S.t)
    : S.t list =
    let ssr = (module S : Sig with type t = t) in
    S.applies_for_every_applicable_level
      (unfold_right_if_star ssr)
      v
end


