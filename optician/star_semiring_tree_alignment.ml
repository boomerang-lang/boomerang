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

module Position = PairOf(IntModule)(IntModule)

type position = Position.t
[@@deriving ord, show, hash]

module PositionPermutation =
struct
  module UnderlyingDict = DictOf(Position)(Position)
  type t = UnderlyingDict.t
  [@@deriving ord, show, hash]

  let create_from_doubles
      (ps:(Position.t * Position.t) list)
    : t =
    UnderlyingDict.from_kvp_list ps
end

module PlusTimesStarTreeAlignmentOf
    (PD : Data)
    (TD : Data)
    (SD : Data)
    (BD : BaseData) =
struct
  module NormalizedTree = NormalizedPlusTimesStarTreeOf(PD)(TD)(SD)(BD)

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
      PositionPermutation.t ->
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

    val get_minimal_alignment :
      NormalizedTree.Nonempty.t -> NormalizedTree.Nonempty.t -> t option

    val get_alignment_distance :
      NormalizedTree.Nonempty.t -> NormalizedTree.Nonempty.t -> float
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
      | Times of TD.t
                 * PositionPermutation.t
                 * position list
                 * position list
                 * t list
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
       (p:PositionPermutation.t)
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

   (* Anders apologizes to whoever reads this code *)
   module PrioritiedDataTreePairs =
   struct
     include TripleOf
         (NormalizedTree.Nonempty)
         (NormalizedTree.Nonempty)
         (FloatModule)
     module Priority = FloatModule
     let priority ((_,_,p):t) = p
   end
   module ProcessedTreeInfo = TripleOf(IntModule)(IntModule)(IntModule)
   module DataTreeProcessedInfoDict =
     DictOf(NormalizedTree.Nonempty)(ProcessedTreeInfo)
   module DataTreeDataTreePriorityPQueue =
     PriorityQueueOf(PrioritiedDataTreePairs)
   let rec get_alignment_distance
       (t1:NormalizedTree.Nonempty.t)
       (t2:NormalizedTree.Nonempty.t)
     : float =
     begin match (get_minimal_alignment t1 t2) with
       | None -> 1.
       | Some al -> cost al
     end

   and get_minimal_alignment_times
       (tl1:TD.t)
       (tl2:TD.t)
       (tts1:NormalizedTree.Nonempty.l list)
       (tts2:NormalizedTree.Nonempty.l list)
     : t option =
     (*| Times of TD.t * Permutation.t * position list * position list
      * t list*)
     if not (is_equal (TD.compare tl1 tl2)) then
       None
     else
       let list_to_dict
           (ts:NormalizedTree.Nonempty.l list)
         : DataTreeProcessedInfoDict.t =
         List.foldi
           ~f:(fun i d (t,c) ->
               DataTreeProcessedInfoDict.insert_or_merge
                 ~merge:(fun _ _ -> failwith "shouldnt merge")
                 d
                 t
                 (i,0,c))
           ~init:DataTreeProcessedInfoDict.empty
           ts
       in
       let d1 = list_to_dict tts1 in
       let d2 = list_to_dict tts2 in
       let t1_keys = DataTreeProcessedInfoDict.key_list d1 in
       let t2_keys = DataTreeProcessedInfoDict.key_list d2 in
       let pq =
         DataTreeDataTreePriorityPQueue.from_list
           (cartesian_map
              ~f:(fun t1 t2 -> (t1,t2,get_alignment_distance t1 t2))
              t1_keys
              t2_keys)
       in
       let (indices_and_alignments,pleft,pright) =
         fold_until_completion
           ~f:(fun (pq,d1,d2,indices_and_alignments) ->
               begin match DataTreeDataTreePriorityPQueue.pop pq with
                 | None ->
                   let leftover_left =
                     List.concat_map
                       ~f:(fun (i1,p,c) ->
                           List.map
                             ~f:(fun i2 -> (i1,i2))
                             (range p c))
                       (DataTreeProcessedInfoDict.value_list d1)
                   in
                   let leftover_right =
                     List.concat_map
                       ~f:(fun (i1,p,c) ->
                           List.map
                             ~f:(fun i2 -> (i1,i2))
                             (range p c))
                       (DataTreeProcessedInfoDict.value_list d2)
                   in
                   Right(indices_and_alignments,leftover_left,leftover_right)
                 | Some ((t1,t2,_),_,pq) ->
                   let l1o = DataTreeProcessedInfoDict.lookup d1 t1 in
                   let l2o = DataTreeProcessedInfoDict.lookup d2 t2 in
                   begin match (l1o,l2o) with
                     | (None   ,_       ) ->
                       Left (pq,d1,d2,indices_and_alignments)
                     | (_      ,    None) ->
                       Left (pq,d1,d2,indices_and_alignments)
                     | (Some (i1,p1,c1), Some (i2,p2,c2)) ->
                       let alignment_option =
                         get_minimal_alignment
                           t1
                           t2
                       in
                       begin match alignment_option with
                         | None -> 
                           Left
                             (DataTreeDataTreePriorityPQueue.empty
                             ,d1
                             ,d2
                             ,indices_and_alignments)
                         | Some alignment ->
                           let index_to = max (c1-p1) (c2-p2) in
                           let p1_new = p1 + index_to in
                           let p2_new = p2 + index_to in
                           let new_indices_and_alignments =
                             List.map
                               ~f:(fun i -> ((i1,p1+i),(i2,p2+i),alignment))
                               (range 0 index_to)
                           in
                           let indices_and_alignments =
                             new_indices_and_alignments@indices_and_alignments
                           in
                           let updater_with_replacement
                               ((index,current,total):int*int*int)
                               (_:int*int*int)
                             : (int * int * int) option =
                             if (current = total) then
                               None
                             else
                               Some (index,current,total)
                           in
                           let d1 =
                             DataTreeProcessedInfoDict.remove_or_update
                               ~updater:(updater_with_replacement (i1,p1_new,c1))
                               d1
                               t1
                           in
                           let d2 =
                             DataTreeProcessedInfoDict.remove_or_update
                               ~updater:(updater_with_replacement (i2,p2_new,c2))
                               d2
                               t2
                           in
                           Left (pq,d1,d2,indices_and_alignments)
                       end
                   end
               end)
           (pq,d1,d2,[])
       in
       let (perm_doubles,alignments) =
         List.unzip
           (List.map
              ~f:(fun (x,y,z) -> ((x,y),z))
              indices_and_alignments)
       in
       Some
         (mk_times
            tl1
            (PositionPermutation.create_from_doubles perm_doubles)
            pleft
            pright
            alignments)

   and get_minimal_alignment
       (t1:NormalizedTree.Nonempty.t)
       (t2:NormalizedTree.Nonempty.t)
     : t option =
     begin match (t1,t2) with
       | (NormalizedTree.Nonempty.Plus (pl1,pts1),
          NormalizedTree.Nonempty.Plus (pl2,pts2)) ->
         failwith "ah1"
       | (NormalizedTree.Nonempty.Times (td1,tts1),
          NormalizedTree.Nonempty.Times (td2,tts2)) ->
         get_minimal_alignment_times td1 td2 tts1 tts2
       | (NormalizedTree.Nonempty.Star (sl1,sts1),
          NormalizedTree.Nonempty.Star (sl2,sts2)) ->
         failwith "ah"
       | (NormalizedTree.Nonempty.Base bd1, NormalizedTree.Nonempty.Base bd2) ->
         failwith "ah"
       | _ -> None
     end
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

  module Tree = PlusTimesStarTreeOf(PD)(TD)(SD)(BD)

  let get_minimal_alignment
      (t1:Tree.t)
      (t2:Tree.t)
    : t option =
    let (normalized_t1,script1) = NormalizedTree.from_tree t1 in
    let (normalized_t2,script2) = NormalizedTree.from_tree t2 in
    begin match (normalized_t1,normalized_t2) with
      | (NormalizedTree.Empty,NormalizedTree.Empty) -> Some Empty
      | (NormalizedTree.Nonempty nt1,NormalizedTree.Nonempty nt2) ->
        let nonempty_alignment =
          NonemptyPlusStarTreeAlignment.get_minimal_alignment
            nt1
            nt2
        in
        Option.map
          ~f:(fun na -> NonemptyTree na)
          nonempty_alignment
      | _ -> None
    end

  let get_alignment_distance
      (t1:Tree.t)
      (t2:Tree.t)
    : float =
    cost (get_minimal_alignment t1 t2)
end
