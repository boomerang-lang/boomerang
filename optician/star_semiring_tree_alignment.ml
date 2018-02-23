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

module type PlusData =
sig
  type t
  val show : t shower
  val pp : t pper
  val compare : t comparer
  val hash : t hasher
  val hash_fold_t : t hash_folder
  val are_compatible : t -> t -> bool
end

module type TimesData =
sig
  type t
  val show : t shower
  val pp : t pper
  val compare : t comparer
  val hash : t hasher
  val hash_fold_t : t hash_folder
  val are_compatible : t -> t -> bool
end

module type StarData =
sig
  type t
  val show : t shower
  val pp : t pper
  val compare : t comparer
  val hash : t hasher
  val hash_fold_t : t hash_folder
  val are_compatible : t -> t -> bool
end

module Position = PairOf(IntModule)(IntModule)
module IntIntDict = DictOf(IntModule)(IntModule)

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
    (PD : PlusData)
    (TD : TimesData)
    (SD : StarData)
    (BD : BaseData) =
struct
  module NormalizedTree = NormalizedPlusTimesStarTreeOf(PD)(TD)(SD)(BD)

  module Nonempty =
  struct
    type t =
      | Plus of PD.t * PD.t
                * (int * int * t) list
                * int list
                * int list
      | Times of TD.t * TD.t
                  * (int * int * t) list
                  * int list
                  * int list
      | Star of SD.t * SD.t * t
      | Base of BD.Alignment.t
    [@@deriving ord, show, hash]
  end

  module type NonemptyNormalizedPlusStarTreeAlignmentType =
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
      PD.t ->
      ((position) * (position) * t) list ->
      ((position) * (position)) list ->
      ((position) * (position)) list ->
      t

    val mk_times :
      TD.t ->
      TD.t ->
      (position * position * t) list ->
      position list ->
      position list ->
      t

    val mk_star :
      SD.t ->
      SD.t ->
      t ->
      t

    val mk_base :
      BD.Alignment.t -> t

    val get_minimal_alignment :
      NormalizedTree.Nonempty.t -> NormalizedTree.Nonempty.t -> t option

    val get_alignment_distance :
      NormalizedTree.Nonempty.t -> NormalizedTree.Nonempty.t -> float option

    val to_nonempty :
      t ->
      NormalizedTree.NormalizationScript.nonempty_t ->
      NormalizedTree.NormalizationScript.nonempty_t ->
      Nonempty.t
  end

  module rec NonemptyNormalizedPlusStarTreeAlignment : NonemptyNormalizedPlusStarTreeAlignmentType =
  struct
    module IntPair = PairOf(IntModule)(IntModule)


    module MappingDict =
    struct
      module DirectAlignmentMapping =
        DictOf
          (PairOf(Position)(Position))
          (NonemptyNormalizedPlusStarTreeAlignment)

      module SingleAlignmentMapping =
        DictOf
          (Position)
          (NonemptyNormalizedPlusStarTreeAlignment)

      module IndirectAlignmentMapping =
        DictOf
          (Position)
          (SingleAlignmentMapping)

      type t =
        {
          direct_mapping : DirectAlignmentMapping.t;
          indirect_mapping_left : IndirectAlignmentMapping.t;
          indirect_mapping_right : IndirectAlignmentMapping.t;
        }
      [@@deriving ord, show, hash, make]

      let direct_mapping
          (m:t)
        : DirectAlignmentMapping.t =
        m.direct_mapping

      let indirect_mapping_left
          (m:t)
        : IndirectAlignmentMapping.t =
        m.indirect_mapping_left

      let indirect_mapping_right
          (m:t)
        : IndirectAlignmentMapping.t =
        m.indirect_mapping_right

      let edges
          (iam:IndirectAlignmentMapping.t)
          (p:Position.t)
        : SingleAlignmentMapping.t =
        begin match IndirectAlignmentMapping.lookup iam p with
          | None -> SingleAlignmentMapping.empty
          | Some d -> d
        end

      let edges_left
          (m:t)
          (p:Position.t)
        : SingleAlignmentMapping.t =
        edges m.indirect_mapping_left p

      let edges_right
          (m:t)
          (p:Position.t)
        : SingleAlignmentMapping.t =
        edges m.indirect_mapping_right p

      let edge_count_left
          (m:t)
          (p:Position.t)
        : int =
        SingleAlignmentMapping.size (edges_left m p)

      let edge_count_right
          (m:t)
          (p:Position.t)
        : int =
        SingleAlignmentMapping.size (edges_right m p)

      let add_alignment
          (m:t)
          ((p1,p2,a):position * position * NonemptyNormalizedPlusStarTreeAlignment.t)
        : t =
        let make_indirect_mapping
            (d:IndirectAlignmentMapping.t)
            (p1:Position.t)
            (p2:Position.t)
            (a:NonemptyNormalizedPlusStarTreeAlignment.t)
          : IndirectAlignmentMapping.t =
          IndirectAlignmentMapping.insert_or_merge
            ~merge:(fun d1 d2 ->
                SingleAlignmentMapping.merge_to_dict
                  ~combiner:(fun _ _ -> failwith "bad kvps")
                  d1
                  d2)
            d
            p1
            (SingleAlignmentMapping.singleton p2 a)
        in
        let dm =
          DirectAlignmentMapping.insert
            m.direct_mapping
            (p1,p2)
            a
        in
        let iml =
          make_indirect_mapping
            m.indirect_mapping_left
            p1
            p2
            a
        in
        let imr =
          make_indirect_mapping
            m.indirect_mapping_right
            p2
            p1
            a
        in
        make
          ~direct_mapping:dm
          ~indirect_mapping_left:iml
          ~indirect_mapping_right:imr

      let empty : t =
        make
          ~direct_mapping:DirectAlignmentMapping.empty
          ~indirect_mapping_left:IndirectAlignmentMapping.empty
          ~indirect_mapping_right:IndirectAlignmentMapping.empty

      let insert_kvp_list
          (d:t)
        : (Position.t * Position.t * NonemptyNormalizedPlusStarTreeAlignment.t) list
          -> t =
        List.fold_left
          ~f:add_alignment
          ~init:d

      let from_kvp_list
        : (Position.t * Position.t * NonemptyNormalizedPlusStarTreeAlignment.t) list
          -> t =
        insert_kvp_list empty

      let size
          (m:t)
        : int =
        DirectAlignmentMapping.size (m.direct_mapping)

      let all_alignments
          (m:t)
        : (Position.t * Position.t * NonemptyNormalizedPlusStarTreeAlignment.t) list =
        List.map
          ~f:(fun ((p1,p2),a) -> (p1,p2,a))
          (DirectAlignmentMapping.as_kvp_list (m.direct_mapping))

      let non_normalized_cost
          (md:t)
        : float =
        let all_matches = all_alignments md in
        let associated_edge_count
            (pleft:position)
            (pright:position)
          : float =
          let left_count = edge_count_left md pleft in
          let right_count = edge_count_right md pright in
          Float.of_int (left_count + right_count - 1)
        in
        let cost_of_edge
            ((p1,p2,a):position * position * NonemptyNormalizedPlusStarTreeAlignment.t)
          : float =
          let plain_cost = NonemptyNormalizedPlusStarTreeAlignment.cost a in
          let associated_edges = associated_edge_count p1 p2 in
          1. -. ((1. -. plain_cost) /. associated_edges)
        in
        let edge_cost =
          List.fold_left
            ~f:(+.)
            ~init:0.
            (List.map ~f:(cost_of_edge) all_matches)
        in
        edge_cost /. ((Float.of_int (List.length all_matches) +. 1.))

      let cost
          (md:t)
        : float =
        let edge_cost = non_normalized_cost md in
        edge_cost /. (Float.of_int (size md) +. 1.)

      let left_positions
          (m:t)
        : position list =
        IndirectAlignmentMapping.key_list (m.indirect_mapping_left)

      let right_positions
          (m:t)
        : position list =
        IndirectAlignmentMapping.key_list (m.indirect_mapping_right)
    end

    module CreateDict =
      DictOf
        (IntPair)
        (IntPair)

    type t =
      | Plus of PD.t * PD.t * MappingDict.t * CreateDict.t * CreateDict.t
      | Times of TD.t * TD.t
                 * (position * position * t) list
                 * position list
                 * position list
      | Star of SD.t * SD.t * t
      | Base of BD.Alignment.t
    [@@deriving ord, show, hash]

    let mk_plus
        (pd1:PD.t)
        (pd2:PD.t)
        (mls:((position) * (position) * t) list)
        (left_creates:((position) * (position)) list)
        (right_creates:((position) * (position)) list)
      : t =
      let md =
        MappingDict.from_kvp_list
          mls
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
        (pd1,pd2,md,cld,crd)

    let mk_times
        (td1:TD.t)
        (td2:TD.t)
        (ms:(position * position * t) list)
        (p1:position list)
        (p2:position list)
      : t =
      Times (td1,td2,ms,p1,p2)

    let mk_star (sd1:SD.t) (sd2:SD.t) (nt:t) : t =
      Star (sd1,sd2,nt)

    let mk_base (b:BD.Alignment.t) : t =
      Base b

    let rec cost
        (nt:t)
      : float =
      begin match nt with
        | Plus (_,_,md,cdl,cdr) ->
          let all_matches = MappingDict.all_alignments md in
          let associated_edge_count
              (pleft:position)
              (pright:position)
            : float =
            let left_count = MappingDict.edge_count_left md pleft in
            let right_count = MappingDict.edge_count_right md pright in
            Float.of_int (left_count + right_count - 1)
          in
          let cost_of_edge
              ((p1,p2,a):position * position * t)
            : float =
            let plain_cost = cost a in
            let associated_edges = associated_edge_count p1 p2 in
            1. -. ((1. -. plain_cost) /. associated_edges)
          in
          let edge_cost =
            List.fold_left
              ~f:(+.)
              ~init:0.
              (List.map ~f:(cost_of_edge) all_matches)
          in
          edge_cost /. ((Float.of_int (List.length all_matches) +. 1.))
        | Times (_,_,als,pleft,pright) ->
          let mapped_count = List.length als in
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
              ~f:(fun acc (_,_,a') -> acc +. (cost a'))
              ~init:0.
              als
          in
          let unnormalized_cost =
            unnormalized_unmapped_cost +.
            unnormalized_recursive_cost
          in
          (unnormalized_cost /. total_size)
        | Star (_,_,a) ->
          cost a
        | Base (a) ->
          BD.Alignment.cost a
      end

    (* Anders apologizes to whoever reads this code *)
    module ProcessedTreeInfo = TripleOf(IntModule)(IntModule)(IntModule)
    module ProcessedPlusTreeInfo =
      QuadrupleOf(IntModule)(IntModule)(IntModule)(IntModule)
    module DataTreeProcessedInfoDict =
      DictOf(NormalizedTree.Nonempty)(ProcessedTreeInfo)
    module PlusDataTreeProcessedInfoDict =
      DictOf(NormalizedTree.Nonempty)(ProcessedPlusTreeInfo)
    module DataTreeMappingCostDict =
      DictOf
        (PairOf(NormalizedTree.Nonempty)(NormalizedTree.Nonempty))
        (FloatModule)
    module PrioritiedRemainingElements =
    struct
      include QuintupleOf
          (ListOf(PairOf(NormalizedTree.Nonempty)(NormalizedTree.Nonempty)))
          (ListOf(TripleOf(Position)(Position)(NonemptyNormalizedPlusStarTreeAlignment)))
          (DataTreeProcessedInfoDict)
          (DataTreeProcessedInfoDict)
          (FloatModule)
      module Priority = FloatModule
      let priority ((_,_,_,_,p):t) = p
    end
    module PositionCostDict = DictOf(Position)(FloatModule)
    module PrioritiedRemainingPlusElements =
    struct
      type nonempty_tree = NormalizedTree.Nonempty.t
      [@@deriving ord, show, hash]

      type t =
        {
          to_process : (nonempty_tree * nonempty_tree) list;
          positions : MappingDict.t;
          left_costs : PositionCostDict.t;
          right_costs : PositionCostDict.t;
          processed_left : PlusDataTreeProcessedInfoDict.t;
          processed_right : PlusDataTreeProcessedInfoDict.t;
          priority : float;
        }
      [@@deriving ord, show, hash, make]

      module Priority = FloatModule

      let priority (x:t) = x.priority
    end
    module RemainingElementsPQueue =
      PriorityQueueOf(PrioritiedRemainingElements)
    module RemainingPlusElementsPQueue =
      PriorityQueueOf(PrioritiedRemainingPlusElements)
    let rec get_alignment_distance
        (t1:NormalizedTree.Nonempty.t)
        (t2:NormalizedTree.Nonempty.t)
      : float option =
      Option.map
        ~f:(fun al -> cost al)
        (get_minimal_alignment t1 t2)

    and get_minimal_alignment_plus
        (pl1:PD.t)
        (pl2:PD.t)
        (tts1:NormalizedTree.Nonempty.l list)
        (tts2:NormalizedTree.Nonempty.l list)
      : t option =
      if not (PD.are_compatible pl1 pl2) then
        None
      else
        let list_to_dict
            (ts:NormalizedTree.Nonempty.l list)
          : PlusDataTreeProcessedInfoDict.t =
          List.foldi
            ~f:(fun i d (t,c) ->
                PlusDataTreeProcessedInfoDict.insert_or_merge
                  ~merge:(fun _ _ -> failwith "shouldnt merge")
                  d
                  t
                  (i,0,c,0))
            ~init:PlusDataTreeProcessedInfoDict.empty
            ts
        in
        let d1 = list_to_dict tts1 in
        let d2 = list_to_dict tts2 in
        let t1_keys = PlusDataTreeProcessedInfoDict.key_list d1 in
        let t2_keys = PlusDataTreeProcessedInfoDict.key_list d2 in
        let cd1 = CreateDict.empty in
        let cd2 = CreateDict.empty in
        (*TODO: required creates*)
        (*TODO: required puts*)
        let tree_priorities =
          DataTreeMappingCostDict.from_kvp_list
            (cartesian_filter_map
               ~f:(fun t1 t2 ->
                   Option.map
                     ~f:(fun d -> ((t1,t2),d))
                     (get_alignment_distance t1 t2))
               t1_keys
               t2_keys)
        in
        let pq =
          RemainingPlusElementsPQueue.singleton
            ({
              to_process = (DataTreeMappingCostDict.key_list tree_priorities);
              positions = MappingDict.empty;
              left_costs = PositionCostDict.empty;
              right_costs = PositionCostDict.empty;
              processed_left = d1;
              processed_right = d2;
              priority = 0.;
            })
        in
        let alignment_info_option =
          fold_until_completion
            ~f:(fun pq ->
                begin match RemainingPlusElementsPQueue.pop pq with
                  | None -> Right None
                  | Some
                      ({
                        to_process = [];
                        positions = aligns;
                        processed_left = d1;
                        processed_right = d2;
                        _
                      },f,_) ->
                    let any_leftover_left =
                      List.exists
                        ~f:(fun (_,_,_,n) -> n = 0)
                        (PlusDataTreeProcessedInfoDict.value_list d1)
                    in
                    let any_leftover_right =
                      List.exists
                        ~f:(fun (_,_,_,n) -> n = 0)
                        (PlusDataTreeProcessedInfoDict.value_list d2)
                    in
                    if any_leftover_left || any_leftover_right then
                      Right None
                    else
                      Right (Some aligns)
                  | Some (qe,f,pq) ->
                    let t1t2s = qe.to_process in
                    let aligns = qe.positions in
                    let cd1 = qe.left_costs in
                    let cd2 = qe.right_costs in
                    let d1 = qe.processed_left in
                    let d2 = qe.processed_right in
                    let to_add = remove_all_elements t1t2s in
                    let queue_elements =
                      List.map
                        ~f:(fun ((t1,t2),t1t2s) ->
                            let l1o =
                              PlusDataTreeProcessedInfoDict.lookup
                                d1
                                t1
                            in
                            let l2o =
                              PlusDataTreeProcessedInfoDict.lookup
                                d2
                                t2
                            in
                            begin match (l1o,l2o) with
                              | (None   ,_       ) ->
                                PrioritiedRemainingPlusElements.make
                                  ~to_process:t1t2s
                                  ~positions:aligns
                                  ~left_costs:cd1
                                  ~right_costs:cd2
                                  ~processed_left:d1
                                  ~processed_right:d2
                                  ~priority:(failwith "this shouldnt hit")
                                  ()
                              | (_      ,    None) ->
                                PrioritiedRemainingPlusElements.make
                                  ~to_process:t1t2s
                                  ~positions:aligns
                                  ~left_costs:cd1
                                  ~right_costs:cd2
                                  ~processed_left:d1
                                  ~processed_right:d2
                                  ~priority:(failwith "this shouldnt hit")
                                  ()
                              | (Some (i1,p1,c1,u1), Some (i2,p2,c2,u2)) ->
                                if u1 > 0 && u2 > 0 then
                                  PrioritiedRemainingPlusElements.make
                                    ~to_process:t1t2s
                                    ~positions:aligns
                                    ~left_costs:cd1
                                    ~right_costs:cd2
                                    ~processed_left:d1
                                    ~processed_right:d2
                                    ~priority:f
                                    ()
                                else
                                  let alignment =
                                    Option.value_exn
                                      (get_minimal_alignment
                                         t1
                                         t2)
                                  in
                                  let fully_processed = (c1-p1) = (c2-p2) in
                                  let t1t2s =
                                    if fully_processed then
                                      t1t2s
                                    else
                                      (t1,t2)::t1t2s
                                  in
                                  let index_to = max (c1-p1) (c2-p2) in
                                  let p1_new = p1 + index_to in
                                  let p2_new = p2 + index_to in
                                  let ranges =
                                    List.zip_exn
                                      (range p1 p1_new)
                                      (range p2 p2_new)
                                  in
                                  let (u1,p1_new) =
                                    if p1_new = c1 then
                                      (u1+1,0)
                                    else
                                      (u1,p1_new)
                                  in
                                  let (u2,p2_new) =
                                    if p2_new = c2 then
                                      (u2+1,0)
                                    else
                                      (u2,p2_new)
                                  in
                                  let d1 =
                                    PlusDataTreeProcessedInfoDict.insert
                                      d1
                                      t1
                                      (i1,p1_new,c1,u1)
                                  in
                                  let d2 =
                                    PlusDataTreeProcessedInfoDict.insert
                                      d2
                                      t2
                                      (i2,p2_new,c2,u2)
                                  in
                                  let new_aligns =
                                    List.map
                                      ~f:(fun (p1,p2) ->
                                          ((i1,p1),(i2,p2),alignment))
                                      ranges
                                  in
                                  let md =
                                    MappingDict.insert_kvp_list
                                      aligns
                                      new_aligns
                                  in
                                  PrioritiedRemainingPlusElements.make
                                    ~to_process:t1t2s
                                    ~positions:md
                                    ~left_costs:cd1
                                    ~right_costs:cd2
                                    ~processed_left:d1
                                    ~processed_right:d2
                                    ~priority:(MappingDict.non_normalized_cost md)
                                    ()
                            end)
                        to_add
                    in
                    Left (RemainingPlusElementsPQueue.push_all pq queue_elements)
                end)
            pq
        in
        let update_map
            (cd:CreateDict.t)
            (indirect_mapping:MappingDict.IndirectAlignmentMapping.t)
          : CreateDict.t =
          MappingDict.IndirectAlignmentMapping.fold
            (fun p1 edge_map cd ->
               if CreateDict.member cd p1 then
                 cd
               else
                 let p2 =
                   (fst
                      (MappingDict.SingleAlignmentMapping.fold
                         (fun p2 a (min_p,min_cost) ->
                            let cost_a = cost a in
                            if cost_a < min_cost then
                              (p2,cost_a)
                            else
                              (min_p,min_cost))
                         ((-100,-100),Float.max_value)
                         edge_map))
                 in
                 CreateDict.insert cd p1 p2)
            cd
            indirect_mapping
        in
        Option.map
          ~f:(fun aligns ->
              let cd1 =
                update_map
                  cd1
                  (MappingDict.indirect_mapping_left aligns)
              in
              let cd2 =
                update_map
                  cd2
                  (MappingDict.indirect_mapping_right aligns)
              in
              Plus (pl1,pl2,aligns,cd1,cd2))
          alignment_info_option

    and get_minimal_alignment_times
        (tl1:TD.t)
        (tl2:TD.t)
        (tts1:NormalizedTree.Nonempty.l list)
        (tts2:NormalizedTree.Nonempty.l list)
      : t option =
      if not (TD.are_compatible tl1 tl2) then
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
        let tree_priorities =
          DataTreeMappingCostDict.from_kvp_list
            (cartesian_filter_map
               ~f:(fun t1 t2 ->
                   Option.map
                     ~f:(fun d -> ((t1,t2),d))
                     (get_alignment_distance t1 t2))
               t1_keys
               t2_keys)
        in
        let pq =
          RemainingElementsPQueue.singleton
            ((DataTreeMappingCostDict.key_list tree_priorities)
            ,[]
            ,d1
            ,d2
            ,0.)
        in
        let alignment_info_option =
          fold_until_completion
            ~f:(fun pq ->
                begin match RemainingElementsPQueue.pop pq with
                  | None -> Right None
                  | Some (([],aligns,d1,d2,_),f,_) ->
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
                    Right (Some (aligns,leftover_left,leftover_right))
                  | Some ((t1t2s,aligns,d1,d2,_),f,pq) ->
                    let to_add = remove_all_elements t1t2s in
                    let queue_elements =
                      List.map
                        ~f:(fun ((t1,t2),t1t2s) ->
                            let l1o = DataTreeProcessedInfoDict.lookup d1 t1 in
                            let l2o = DataTreeProcessedInfoDict.lookup d2 t2 in
                            begin match (l1o,l2o) with
                              | (None   ,_       ) ->
                                (t1t2s,aligns,d1,d2,f)
                              | (_      ,    None) ->
                                (t1t2s,aligns,d1,d2,f)
                              | (Some (i1,p1,c1), Some (i2,p2,c2)) ->
                                let alignment_cost =
                                  (DataTreeMappingCostDict.lookup_exn
                                     tree_priorities
                                     (t1,t2))
                                in
                                let alignment =
                                  Option.value_exn
                                    (get_minimal_alignment
                                       t1
                                       t2)
                                in
                                let index_to = max (c1-p1) (c2-p2) in
                                let p1_new = p1 + index_to in
                                let p2_new = p2 + index_to in
                                let ranges =
                                  List.zip_exn
                                    (range p1 p1_new)
                                    (range p2 p2_new)
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
                                let new_aligns =
                                  List.map
                                    ~f:(fun (p1,p2) -> ((i1,p1),(i2,p2),alignment))
                                    ranges
                                in
                                (t1t2s
                                ,new_aligns@aligns
                                ,d1
                                ,d2
                                ,f +. alignment_cost)
                            end)
                        to_add
                    in
                    Left (RemainingElementsPQueue.push_all pq queue_elements)
                end)
            pq
        in
        Option.map
          ~f:(fun (aligns,pleft,pright) ->
              mk_times
                tl1
                tl2
                aligns
                pleft
                pright)
          alignment_info_option

    and get_minimal_alignment
        (t1:NormalizedTree.Nonempty.t)
        (t2:NormalizedTree.Nonempty.t)
      : t option =
      begin match (t1,t2) with
        | (NormalizedTree.Nonempty.Plus (pl1,pts1),
           NormalizedTree.Nonempty.Plus (pl2,pts2)) ->
          get_minimal_alignment_plus pl1 pl2 pts1 pts2
        | (NormalizedTree.Nonempty.Times (tl1,tts1),
           NormalizedTree.Nonempty.Times (tl2,tts2)) ->
          get_minimal_alignment_times tl1 tl2 tts1 tts2
        | (NormalizedTree.Nonempty.Star (sl1,(sts1,_)),
           NormalizedTree.Nonempty.Star (sl2,(sts2,_))) ->
          if not (SD.are_compatible sl1 sl2) then
            None
          else
            Option.map
              ~f:(fun a -> Star (sl1,sl2,a))
              (get_minimal_alignment sts1 sts2)
        | (NormalizedTree.Nonempty.Base bd1, NormalizedTree.Nonempty.Base bd2) ->
          Option.map ~f:(fun a -> Base a) (BD.get_alignment bd1 bd2)
        | _ -> None
      end

    let rec to_nonempty
        (nta:NonemptyNormalizedPlusStarTreeAlignment.t)
        (ns1:NormalizedTree.NormalizationScript.nonempty_t)
        (ns2:NormalizedTree.NormalizationScript.nonempty_t)
      : Nonempty.t =
      begin match (nta,ns1,ns2) with
        | (Plus (pl1,pl2,md,cd1,cd2),Plus(pnl1,nsl1),Plus(pnl2,nsl2)) ->
          let createdict_to_createlist
              (cd:CreateDict.t)
              (left_perm:CountedPermutation.t)
              (right_perm:CountedPermutation.t)
            : int list =
            let unsorted_clp =
              List.map
                ~f:(fun (s,t) ->
                    (CountedPermutation.apply_inverse_exn left_perm s
                    ,CountedPermutation.apply_inverse_exn right_perm t))
                (CreateDict.as_kvp_list cd1)
            in
            let sorted_clp =
              List.sort
                ~cmp:(fun (s1,_) (s2,_) -> Int.compare s1 s2)
                unsorted_clp
            in
            List.map
              ~f:snd
              sorted_clp
          in
          let pl1' =
            NormalizedTree.NormalizationScript.PD_NormalizationLabel.get_label
              pnl1
          in
          let pl2' =
            NormalizedTree.NormalizationScript.PD_NormalizationLabel.get_label
              pnl2
          in
          let perm1 =
            NormalizedTree.NormalizationScript.PD_NormalizationLabel.get_perm
              pnl1
          in
          let perm2 =
            NormalizedTree.NormalizationScript.PD_NormalizationLabel.get_perm
              pnl2
          in
          if pl1 <> pl1' || pl2 <> pl2' then
            failwith "bad application of normalization script"
          else
            let cl1 = createdict_to_createlist cd1 perm1 perm2 in
            let cl2 = createdict_to_createlist cd2 perm2 perm1 in
            let all_aligns =
              List.map
                ~f:(fun (p1,p2,nta) ->
                    let i1 = CountedPermutation.apply_inverse_exn perm1 p1 in
                    let i2 = CountedPermutation.apply_inverse_exn perm1 p1 in
                    let ns1 =
                      List.nth_exn
                        nsl1
                        i1
                    in
                    let ns2 =
                      List.nth_exn
                        nsl2
                        i2
                    in
                    (i1,i2,to_nonempty nta ns1 ns2))
                (MappingDict.all_alignments md)
            in
            Plus (pl1,pl2,all_aligns,cl1,cl2)
        | (Times(tl1,tl2,als,pl1,pl2)
          ,Times(tnl1,nsl1)
          ,Times(tnl2,nsl2)) ->
          let transform_projection_list
              (perm:CountedPermutation.t)
              (plist:position list)
            : int list =
            List.map
              ~f:(CountedPermutation.apply_inverse_exn perm)
              plist
          in
          let tl1' =
            NormalizedTree.NormalizationScript.TD_NormalizationLabel.get_label
              tnl1
          in
          let tl2' =
            NormalizedTree.NormalizationScript.TD_NormalizationLabel.get_label
              tnl2
          in
          let perm1 =
            NormalizedTree.NormalizationScript.TD_NormalizationLabel.get_perm
              tnl1
          in
          let perm2 =
            NormalizedTree.NormalizationScript.TD_NormalizationLabel.get_perm
              tnl2
          in
          if tl1 <> tl1' || tl2 <> tl2' then
            failwith "bad application of normalization script"
          else
            let projs1 = transform_projection_list perm1 pl1 in
            let projs2 = transform_projection_list perm2 pl2 in
            let aligns =
              List.map
                ~f:(fun (p1,p2,al) ->
                    let i1 = CountedPermutation.apply_inverse_exn perm1 p1 in
                    let i2 = CountedPermutation.apply_inverse_exn perm2 p2 in
                    let ns1 =
                      List.nth_exn
                        nsl1
                        i1
                    in
                    let ns2 =
                      List.nth_exn
                        nsl2
                        i2
                    in
                    (i1,i2,to_nonempty al ns1 ns2))
                als
            in
            let ordered_aligns =
              List.sort
                ~cmp:(fun (p1,_,_) (p2,_,_) -> Int.compare p1 p2)
                aligns
            in
            let fsts_and_pos =
              List.mapi
                ~f:(fun i (p1,_,_) -> (p1,i))
                ordered_aligns
            in
            let snds_and_pos =
              List.mapi
                ~f:(fun i p2 -> (p2,i))
                (List.sort
                   ~cmp:Int.compare
                   (List.map ~f:snd_trip aligns))
            in
            let fst_map = IntIntDict.from_kvp_list fsts_and_pos in
            let snd_map = IntIntDict.from_kvp_list snds_and_pos in
            let perm =
              Permutation.create_from_pairs
                (List.map
                   ~f:(fun (p1,p2) ->
                       (IntIntDict.lookup_exn fst_map p1
                       ,IntIntDict.lookup_exn snd_map p2))
                   (List.map ~f:(fun (p1,p2,_) -> (p1,p2)) ordered_aligns))
            in
            let alignments =
              List.map
                ~f:trd_trip
                ordered_aligns
            in
            Times (tl1,tl2,aligns,projs1,projs2)
        | (Star (sl1,sl2,ns'),Star (sl1',ns1'), Star (sl2',ns2')) ->
          if (sl1 <> sl1') || (sl2 <> sl2') then
            failwith "bad application of normalization script"
          else
            Star (sl1,sl2,to_nonempty ns' ns1' ns2')
        | (Base ba,Base bl,Base bl') ->
          Base ba
        | _ -> failwith "bad application of normalization script"
      end
  end

  type t =
    | Empty
    | NonemptyTree of Nonempty.t
  [@@deriving ord, show, hash]

  let cost
      (ao:t option)
    : float =
    begin match ao with
      | None -> 0.
      | Some Empty -> 1.
      | Some NonemptyTree nt ->
        failwith "TODO"
        (*NonemptyNormalizedPlusStarTreeAlignment.cost nt*)
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
        let (script1,script2) =
          begin match (script1,script2) with
            | (Nonempty script1,Nonempty script2) -> (script1,script2)
            | _ -> failwith "bad normalization"
          end
        in
        let nonempty_normalized_alignment =
          NonemptyNormalizedPlusStarTreeAlignment.get_minimal_alignment
            nt1
            nt2
        in
        Option.map
          ~f:(fun nna ->
              let na =
                NonemptyNormalizedPlusStarTreeAlignment.to_nonempty
                  nna
                  script1
                  script2
              in
              NonemptyTree na)
          nonempty_normalized_alignment
      | _ -> None
    end

  let get_alignment_distance
      (t1:Tree.t)
      (t2:Tree.t)
    : float =
    cost (get_minimal_alignment t1 t2)
end
