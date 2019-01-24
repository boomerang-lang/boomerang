open MyStdlib
open Star_semiring_tree
open Lenscontext
open Consts

let lc : LensContext.t ref = ref LensContext.empty

module type DefaultData = Data

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
  val get_alignment : LensContext.t -> t -> t -> Alignment.t option
  val requires_mapping : t -> bool
  module Default : DefaultData
  val extract_default : t -> Default.t option
  val information_content : t -> float
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
  val requires_mapping : t -> bool
  module Default : DefaultData
  val extract_default : t -> Default.t option
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
  val requires_mapping : t -> bool
  module Default : DefaultData
  val extract_default : t -> Default.t option
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
  val requires_mapping : t -> bool
  module Default : DefaultData
  val extract_default : t -> Default.t option
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
  let rec requires_mapping
      (nt:NormalizedTree.Nonempty.t)
      (requires_if_nonzero:bool)
    : bool =
    (begin match nt.node with
      | Plus (pd,nts) ->
        PD.requires_mapping pd
        || (List.length
              (List.dedup_and_sort
                 ~compare:Int.compare
                 (List.map ~f:(fun (_,_,i) -> i) nts))
            <> List.length nts)
        || (List.exists ~f:(fun ((nt,_),_,i) -> requires_mapping nt false) nts)
      | Times (td,nts) ->
        TD.requires_mapping td
        || (List.exists ~f:(fun ((nt,_),b) -> requires_mapping nt b) nts)
      | Star (sd,(nt,_),_) ->
        SD.requires_mapping sd
        || requires_mapping nt false
      | Base bd ->
        BD.requires_mapping bd
     end
    ||
    requires_if_nonzero)
    && (NormalizedTree.Nonempty.information_content nt) <> 0.

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
      (position * position * t) list ->
      (position * position) list ->
      (position * position) list ->
      t

    val mk_times :
      TD.t ->
      TD.t ->
      (position * position * t) list ->
      (position * float) list ->
      (position * float) list ->
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

    val to_nonempty_and_cost :
      t ->
      NormalizedTree.NormalizationScript.nonempty_t ->
      NormalizedTree.NormalizationScript.nonempty_t ->
      Nonempty.t * float

    val clear_dict : unit -> unit
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
          IndirectAlignmentMapping.insert_or_combine
            ~combiner:(fun d1 d2 ->
                SingleAlignmentMapping.merge_to_dict
                  ~combiner:(fun v1 v2 -> failwith "bad kvps")
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
          (kvps:(Position.t
                 * Position.t
                 * NonemptyNormalizedPlusStarTreeAlignment.t) list)
        : t =
        List.fold_left
          ~f:add_alignment
          ~init:d
          kvps

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
      {
        node                    : t_node                      ;
        mutable left_right_cost : float option [@hash.ignore] ;
        mutable right_left_cost : float option [@hash.ignore] ;
      }
    and t_node =
      | Plus of PD.t * PD.t * MappingDict.t * CreateDict.t * CreateDict.t
      | Times of TD.t * TD.t
                 * (position * position * t) list
                 * (position * float) list
                 * (position * float) list
      | Star of SD.t * SD.t * t
      | Base of BD.Alignment.t
    [@@deriving ord, show, hash]

    type normalized_alignment = t

    let mk_t
        (tn:t_node)
      : t =
      {
        node            = tn   ;
        left_right_cost = None ;
        right_left_cost = None ;
      }

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
      mk_t
        (Plus
           (pd1
           ,pd2
           ,md
           ,cld
           ,crd))

    let mk_times
        (td1:TD.t)
        (td2:TD.t)
        (ms:(position * position * t) list)
        (p1:(position * float) list)
        (p2:(position * float) list)
      : t =
      mk_t
        (Times
           (td1
           ,td2
           ,ms
           ,p1
           ,p2))

    let mk_star (sd1:SD.t) (sd2:SD.t) (nt:t) : t =
      mk_t
        (Star (sd1,sd2,nt))

    let mk_base (b:BD.Alignment.t) : t =
      mk_t (Base b)

    let rec right_left_cost
        (nt:t)
      : float =
      if !no_intelligent_cost then
        0.
      else if !constants_cost then
        begin match nt.node with
          | Plus (_,_,md,_,_) ->
            let position_cost
                (iam:MappingDict.IndirectAlignmentMapping.t)
                (p:position)
              : float =
              let associated_edges =
                MappingDict.SingleAlignmentMapping.value_list
                  (MappingDict.IndirectAlignmentMapping.lookup_exn
                     iam
                     p)
              in
              let edge_costs = List.map ~f:right_left_cost associated_edges in
              let edge_cost = FloatList.sum edge_costs in
              let edge_count = List.length associated_edges in
              let choice_cost = (Float.of_int edge_count) -. 1. in
              edge_cost +. choice_cost
            in
            let side_cost
                (iam:MappingDict.IndirectAlignmentMapping.t)
              : float =
              let positions =
                MappingDict.IndirectAlignmentMapping.key_list
                  iam
              in
              let positions_cost =
                List.map
                  ~f:(position_cost iam)
                  positions
              in
              if List.is_empty positions_cost then
                0.
              else
                FloatList.sum positions_cost
            in
            let right_cost =
              side_cost
                (MappingDict.indirect_mapping_right md)
            in
            right_cost
          | Times (_,_,als,_,pright) ->
            let recursive_costs =
              List.map
                ~f:(fun (_,_,a) -> right_left_cost a +. 1.)
                als
            in
            let projected_costs = List.map ~f:snd pright in
            let merge_costs = List.fold_left ~f:(+.) ~init:0. in
            (merge_costs recursive_costs)
            +. (merge_costs projected_costs)
          | Star (_,_,a) ->
            right_left_cost a
          | Base (a) ->
            BD.Alignment.cost a
        end
      else
        begin match nt.right_left_cost with
          | Some c -> c
          | None ->
            let c =
              begin match nt.node with
                | Plus (_,_,md,_,_) ->
                  let position_cost
                      (iam:MappingDict.IndirectAlignmentMapping.t)
                      (p:position)
                    : float =
                    let associated_edges =
                      MappingDict.SingleAlignmentMapping.value_list
                        (MappingDict.IndirectAlignmentMapping.lookup_exn
                           iam
                           p)
                    in
                    let edge_costs = List.map ~f:right_left_cost associated_edges in
                    let average_edge_cost = FloatList.average edge_costs in
                    let edge_count = List.length associated_edges in
                    let choice_cost = Math.log2 (Float.of_int edge_count) in
                    average_edge_cost +. choice_cost
                  in
                  let side_cost
                      (iam:MappingDict.IndirectAlignmentMapping.t)
                    : float =
                    let positions =
                      MappingDict.IndirectAlignmentMapping.key_list
                        iam
                    in
                    let positions_cost =
                      List.map
                        ~f:(position_cost iam)
                        positions
                    in
                    if List.is_empty positions_cost then
                      0.
                    else
                      FloatList.average positions_cost
                  in
                  let right_cost =
                    side_cost
                      (MappingDict.indirect_mapping_right md)
                  in
                  right_cost
                | Times (_,_,als,_,pright) ->
                  let recursive_costs =
                    List.map
                      ~f:(fun (_,_,a) -> right_left_cost a)
                      als
                  in
                  let projected_costs = List.map ~f:snd pright in
                  let merge_costs = List.fold_left ~f:(+.) ~init:0. in
                  (merge_costs recursive_costs)
                  +. (merge_costs projected_costs)
                | Star (_,_,a) ->
                  right_left_cost a
                | Base (a) ->
                  BD.Alignment.cost a
              end
            in
            nt.right_left_cost <- Some c;
            c
        end

    let rec left_right_cost
        (nt:t)
      : float =
      if !no_intelligent_cost then
        0.
      else if !constants_cost then
        begin match nt.node with
          | Plus (_,_,md,_,_) ->
            let position_cost
                (iam:MappingDict.IndirectAlignmentMapping.t)
                (p:position)
              : float =
              let associated_edges =
                MappingDict.SingleAlignmentMapping.value_list
                  (MappingDict.IndirectAlignmentMapping.lookup_exn
                     iam
                     p)
              in
              let edge_costs = List.map ~f:left_right_cost associated_edges in
              let edge_cost = FloatList.sum edge_costs in
              let edge_count = List.length associated_edges in
              let choice_cost = (Float.of_int edge_count) -. 1. in
              edge_cost +. choice_cost
            in
            let side_cost
                (iam:MappingDict.IndirectAlignmentMapping.t)
              : float =
              let positions =
                MappingDict.IndirectAlignmentMapping.key_list
                  iam
              in
              let positions_cost =
                List.map
                  ~f:(position_cost iam)
                  positions
              in
              if List.is_empty positions_cost then
                0.
              else
                FloatList.sum positions_cost
            in
            let left_cost =
              side_cost
                (MappingDict.indirect_mapping_left md)
            in
            left_cost
          | Times (_,_,als,pleft,_) ->
            let recursive_costs =
              List.map
                ~f:(fun (_,_,a) -> left_right_cost a +. 1.)
                als
            in
            let projected_costs = List.map ~f:snd pleft in
            let merge_costs = List.fold_left ~f:(+.) ~init:0. in
            (merge_costs recursive_costs)
            +. (merge_costs projected_costs)
          | Star (_,_,a) ->
            left_right_cost a
          | Base (a) ->
            BD.Alignment.cost a
        end
      else
        begin match nt.left_right_cost with
          | Some c -> c
          | None ->
            let c =
              begin match nt.node with
                | Plus (_,_,md,_,_) ->
                  let position_cost
                      (iam:MappingDict.IndirectAlignmentMapping.t)
                      (p:position)
                    : float =
                    let associated_edges =
                      MappingDict.SingleAlignmentMapping.value_list
                        (MappingDict.IndirectAlignmentMapping.lookup_exn
                           iam
                           p)
                    in
                    let edge_costs = List.map ~f:left_right_cost associated_edges in
                    let edge_count = List.length associated_edges in
                    if edge_count = 0 then
                      0.
                    else
                      let average_edge_cost = FloatList.average edge_costs in
                      let choice_cost = Math.log2 (Float.of_int edge_count) in
                      average_edge_cost +. choice_cost
                  in
                  let side_cost
                      (iam:MappingDict.IndirectAlignmentMapping.t)
                    : float =
                    let positions =
                      MappingDict.IndirectAlignmentMapping.key_list
                        iam
                    in
                    let positions_cost =
                      List.map
                        ~f:(position_cost iam)
                        positions
                    in
                    if List.is_empty positions_cost then
                      0.
                    else
                      FloatList.average positions_cost
                  in
                  let left_cost =
                    side_cost
                      (MappingDict.indirect_mapping_left md)
                  in
                  left_cost
                | Times (_,_,als,pleft,_) ->
                  let recursive_costs =
                    List.map
                      ~f:(fun (_,_,a) -> left_right_cost a)
                      als
                  in
                  let projected_costs = List.map ~f:snd pleft in
                  let merge_costs = List.fold_left ~f:(+.) ~init:0. in
                  (merge_costs recursive_costs)
                  +. (merge_costs projected_costs)
                | Star (_,_,a) ->
                  left_right_cost a
                | Base (a) ->
                  BD.Alignment.cost a
              end
            in
            nt.left_right_cost <- Some c;
            c
        end

    let rec cost
        (nt:t)
      : float =
      left_right_cost nt +. right_left_cost nt

    (* Anders apologizes to whoever reads this code *)
    module GetMinimalAlignmentArg =
    struct
      include HashConsOf(PairOf(NormalizedTree.Nonempty)(NormalizedTree.Nonempty))
      let create t1 t2 = hashcons (t1,t2)
    end

    module PlusMappingState =
    struct
      module TreeTrip = TripleOf(NormalizedTree.Nonempty)(Probability)(IntModule)

      module Mappings =
      struct
        include DictOf
            (PairOf(TreeTrip)(TreeTrip))
            (PairOf(NonemptyNormalizedPlusStarTreeAlignment)(IntModule))

        let insert
            (m:t)
            (nt1:TreeTrip.t)
            (nt2:TreeTrip.t)
            (al:normalized_alignment)
            (count:int)
          : t =
          insert_or_combine
            ~combiner:(fun (al,count1) (_,count2) -> (al,count1+count2))
            m
            (nt1,nt2)
            (al,count)

        let remove
            (m:t)
            (nt1:TreeTrip.t)
            (nt2:TreeTrip.t)
            (count:int)
          : t =
          update
            ~updater:(fun (al,old_count) ->
                let remaining = old_count - count in
                (al,remaining))
            m
            (nt1,nt2)

        let lookup_count
            (m:t)
            (nt1:TreeTrip.t)
            (nt2:TreeTrip.t)
          : int =
          begin match lookup m (nt1,nt2) with
            | Some (_,count) -> count
            | None -> 0
          end
      end

      module ProcessedTreeInfo =
      struct
        module MGDict = DictOf(IntModule)(IntModule)

        type t =
          {
            tree_trip       : TreeTrip.t ;
            index           : int        ;
            processed_count : int        ;
            total_count     : int        ;
            mg_dict         : MGDict.t   ;
          }
        [@@deriving ord, show, hash, make]

        let init
            ~tree_trip:(tree_trip:TreeTrip.t)
            ~index:(index:int)
            ~total_count:(total_count:int)
          : t =
          make
            ~index:index
            ~tree_trip:tree_trip
            ~processed_count:0
            ~total_count:total_count
            ~mg_dict:(MGDict.empty)

        let add_alignments
            (info:t)
            (count:int)
            (group:int)
          : t =
          let processed_count = info.processed_count + count in
          let old_count = MGDict.lookup_default ~default:0 info.mg_dict group in
          let mg_dict = MGDict.insert info.mg_dict group (old_count + count) in
          { info with
            processed_count = processed_count ;
            mg_dict = mg_dict ;
          }

        let can_take
            (info:t)
            (group:int)
          : bool =
          let count = MGDict.lookup_default ~default:0 info.mg_dict group in
          count < info.total_count

        let mapped_loop
            (info:t)
          : int =
          info.processed_count / info.total_count
      end

      module TreeInfoDict =
      struct
        include DictOf(TreeTrip)(ProcessedTreeInfo)

        let multimap_penalty
            (d:t)
            (t:TreeTrip.t)
          : float =
          let info = lookup_exn d t in
          if ProcessedTreeInfo.can_take info (trd3 t) then
            Math.log (Float.of_int (ProcessedTreeInfo.mapped_loop info + 1))
          else
            Float.infinity

        let add_alignments
            (d:t)
            (t:TreeTrip.t)
            (count:int)
            (group:int)
          : t =
          update
            ~updater:(fun info ->
                ProcessedTreeInfo.add_alignments info count group)
            d
            t

        let add_productive
            (d:t)
            (t:TreeTrip.t)
          : bool =
          let info = lookup_exn d t in
          ProcessedTreeInfo.mapped_loop info = 0

        let available_count
            (d:t)
            (t:TreeTrip.t)
            (group:int)
          : int =
          let info = lookup_exn d t in
          let loop_availability =
            info.total_count
            - (info.processed_count mod info.total_count)
          in
          let group_availability =
            info.total_count -
            (ProcessedTreeInfo.MGDict.lookup_default
               ~default:0
               info.mg_dict
               group)
          in
          min loop_availability group_availability

        let all_count
            (d:t)
          : int =
          List.fold_left
            ~f:(fun acc (_,info) -> acc + info.total_count)
            ~init:0
            (as_kvp_list d)
      end

      module CreateDict =
      struct
        include DictOf(IntPair)(IntPair)
      end

      module PrioritiedRemainingElements =
      struct
        include QuadrupleOf
            (TreeTrip)
            (TreeTrip)
            (NonemptyNormalizedPlusStarTreeAlignment)
            (FloatModule)
        module Priority = FloatModule
        let priority ((_,_,_,p):t) = p
      end

      module RemainingElementsPQueue =
        PriorityQueueOf(PrioritiedRemainingElements)

      type t =
        {
          info_dict_l   : TreeInfoDict.t            ;
          info_dict_r   : TreeInfoDict.t            ;
          create_dict_l : CreateDict.t              ;
          create_dict_r : CreateDict.t              ;
          mappings      : Mappings.t                ;
          remaining     : RemainingElementsPQueue.t ;
        }
      [@@deriving show]

      let add_productive
          (s:t)
          (t_l:TreeTrip.t)
          (t_r:TreeTrip.t)
        : bool =
        let productive_l = TreeInfoDict.add_productive s.info_dict_l t_l in
        let productive_r = TreeInfoDict.add_productive s.info_dict_r t_r in
        productive_l || productive_r

      let available_count_left
          (s:t)
          (t:TreeTrip.t)
          (group:int)
        : int =
        TreeInfoDict.available_count s.info_dict_l t group

      let available_count_right
          (s:t)
          (t:TreeTrip.t)
          (group:int)
        : int =
        TreeInfoDict.available_count s.info_dict_r t group

      let add_alignments
          (s:t)
          (t_l:TreeTrip.t)
          (t_r:TreeTrip.t)
          (al:normalized_alignment)
          (count:int)
        : t =
        let info_dict_l = TreeInfoDict.add_alignments s.info_dict_l t_l count (trd3 t_r) in
        let info_dict_r = TreeInfoDict.add_alignments s.info_dict_r t_r count (trd3 t_l) in
        let mappings = Mappings.insert s.mappings t_l t_r al count in
        { s with
          info_dict_l = info_dict_l ;
          info_dict_r = info_dict_r ;
          mappings = mappings       ; }

      let pop
          (s:t)
        : (TreeTrip.t
           * TreeTrip.t
           * normalized_alignment
           * float
           * t) option =
        begin match RemainingElementsPQueue.pop s.remaining with
          | None -> None
          | Some ((nt1,nt2,al,pri),_,remaining) ->
            Some
              (nt1
              ,nt2
              ,al
              ,pri
              ,{ s with remaining = remaining; })
        end

      let push
          (s:t)
          (t1:TreeTrip.t)
          (t2:TreeTrip.t)
          (al:normalized_alignment)
          (c:float)
        : t =
        { s with
          remaining = RemainingElementsPQueue.push s.remaining (t1,t2,al,c)
        }

      let pure_cost = cost

      let cost
          (s:t)
          (t_l:TreeTrip.t)
          (t_r:TreeTrip.t)
          (na:normalized_alignment)
        : float =
        let multimap_penalty_l =
          TreeInfoDict.multimap_penalty
            s.info_dict_l
            t_l
        in
        let multimap_penalty_r =
          TreeInfoDict.multimap_penalty
            s.info_dict_r
            t_r
        in
        multimap_penalty_l +. multimap_penalty_r +. cost na

      let init
          (recursive_f:GetMinimalAlignmentArg.t -> normalized_alignment option)
          ~subtrees_l:(subtrees_l:(NormalizedTree.Nonempty.l * Probability.t * int) list)
          ~subtrees_r:(subtrees_r:(NormalizedTree.Nonempty.l * Probability.t * int) list)
        : t =
        (*print_endline @$ (string_of_list Int.to_string (List.map ~f:(fun ((_,c),_,_) -> c) subtrees_l));
          print_endline @$ (string_of_list Int.to_string (List.map ~f:(fun ((_,c),_,_) -> c) subtrees_r));*)
        let list_to_dict
            (ts:(NormalizedTree.Nonempty.l * Probability.t * int) list)
          : TreeInfoDict.t =
          List.foldi
            ~f:(fun i d ((t,c),p,num) ->
                TreeInfoDict.insert_or_combine
                  ~combiner:(fun _ _ -> failwith "shouldnt merge")
                  d
                  (t,p,num)
                  (ProcessedTreeInfo.init ~tree_trip:(t,p,num) ~index:i ~total_count:c))
            ~init:TreeInfoDict.empty
            ts
        in
        let info_dict_l = list_to_dict subtrees_l in
        let info_dict_r = list_to_dict subtrees_r in
        let t1_keys = TreeInfoDict.key_list info_dict_l in
        let t2_keys = TreeInfoDict.key_list info_dict_r in
        let relevant_trees =
          (cartesian_filter_map
             ~f:(fun (t1,p1,i1) (t2,p2,i2) ->
                 let ans_o = recursive_f (GetMinimalAlignmentArg.create t1 t2) in
                 Option.map ~f:(fun ans -> ((t1,p1,i1),(t2,p2,i2),ans,pure_cost ans)) ans_o)
             t1_keys
             t2_keys)
        in
        let remaining = RemainingElementsPQueue.from_list relevant_trees in
        {
          info_dict_l    = info_dict_l      ;
          info_dict_r    = info_dict_r      ;
          create_dict_l  = CreateDict.empty ;
          create_dict_r  = CreateDict.empty ;
          remaining      = remaining        ;
          mappings       = Mappings.empty   ;
        }

      module PositionNumbers =
      struct
        module PositionData =
        struct
          type t =
            {
              index            : int ;
              current_position : int ;
              max_position     : int ;
            }
          [@@deriving ord, show, hash, make]

          let init
              ~index:(index:int)
              ~max_position:(max_position:int)
            : t =
            make
              ~index:index
              ~current_position:0
              ~max_position:max_position

          let increase_and_retrieve
              (data:t)
              (count:int)
            : (position list) * t =
            let i = data.index in
            let current_position = data.current_position in
            let max_position = data.max_position in
            let js =
              List.map
                ~f:(fun offset -> (current_position + offset) mod max_position)
                (range 0 count)
            in
            let data =
              { data with
                current_position = data.current_position + count;
              }
            in
            let ijs =
              List.map
                ~f:(fun j -> (i,j))
                js
            in
            (ijs,data)
        end
        include DictOf(TreeTrip)(PositionData)

        let pop_positions
            (d:t)
            (t:TreeTrip.t)
            (count:int)
          : (position list) * t =
          let data = lookup_exn d t in
          let (poss,data) = PositionData.increase_and_retrieve data count in
          let d = insert d t data in
          (poss,d)

        let init
            (info_dict:TreeInfoDict.t)
          : t =
          let info_dict_kvp = TreeInfoDict.as_kvp_list info_dict in
          let tree_data_kvp =
            List.map
              ~f:(fun (t,info) ->
                  (t
                  ,PositionData.init
                      ~index:info.index
                      ~max_position:info.total_count))
              info_dict_kvp
          in
          from_kvp_list
            tree_data_kvp
      end
      module BestAlignments =
      struct
        include DictOf(Position)(PairOf(Position)(FloatModule))

        let update_bests
            (d:t)
            (ps:(position * position) list)
            (al:normalized_alignment)
          : t =
          let c = pure_cost al in
          List.fold_left
            ~f:(fun d (p1,p2) ->
                insert_or_combine
                  ~combiner:(fun (previous_best_p,previous_best_c) (p2,c) ->
                      if previous_best_c < c then
                        (previous_best_p,previous_best_c)
                      else
                        (p2,c))
                  d
                  p1
                  (p2,c))
            ~init:d
            ps
      end
      let to_aligns_and_creates
          (s:t)
        : ((position * position * normalized_alignment) list
           * (position * position) list
           * (position * position) list) option =
        let pnl = PositionNumbers.init s.info_dict_l in
        let pnr = PositionNumbers.init s.info_dict_r in
        let bal = BestAlignments.empty in
        let bar = BestAlignments.empty in
        let mappings_kvp = Mappings.as_kvp_list s.mappings in
        let (_,_,bal,bar,aligns) =
          List.fold_left
            ~f:(fun (pnl,pnr,bal,bar,aligns) ((tl,tr),(al,count)) ->
                let (possl,pnl) = PositionNumbers.pop_positions pnl tl count in
                let (possr,pnr) = PositionNumbers.pop_positions pnr tr count in
                let posps_lr = List.zip_exn possl possr in
                let posps_rl = List.zip_exn possr possl in

                let bal = BestAlignments.update_bests bal posps_lr al in
                let bar = BestAlignments.update_bests bar posps_rl al in
                let new_aligns =
                  List.map
                    ~f:(fun (pl,pr) -> (pl,pr,al))
                    posps_lr
                in
                (pnl,pnr,bal,bar,new_aligns@aligns))
            ~init:(pnl,pnr,bal,bar,[])
            mappings_kvp
        in
        let cdl =
          List.fold_left
            ~f:(fun cdl (p1,(p2,_)) ->
                CreateDict.insert_or_combine
                  ~combiner:(fun v_old _ -> v_old)
                  cdl
                  p1
                  p2)
            ~init:s.create_dict_l
            (BestAlignments.as_kvp_list bal)
        in
        let cdr =
          List.fold_left
            ~f:(fun cdr (p1,(p2,_)) ->
                CreateDict.insert_or_combine
                  ~combiner:(fun v_old _ -> v_old)
                  cdr
                  p1
                  p2)
            ~init:s.create_dict_r
            (BestAlignments.as_kvp_list bar)
        in
        let createsl = CreateDict.as_kvp_list cdl in
        let createsr = CreateDict.as_kvp_list cdr in
        if List.length createsl <> TreeInfoDict.all_count s.info_dict_l
         || List.length createsr <> TreeInfoDict.all_count s.info_dict_r
        then
          None
        else
          Some (aligns,createsl,createsr)
    end

    module TimesMappingState =
    struct
      module TreePair = PairOf(NormalizedTree.Nonempty)(BoolModule)

      let net_loss_pure
          (nt:normalized_alignment)
          ((tl,bl):TreePair.t)
          ((tr,br):TreePair.t)
        : float =
        if (cost nt) = 0. && requires_mapping tl bl && requires_mapping tr br then
          Float.neg_infinity
        else
          cost nt
          -. NormalizedTree.Nonempty.information_content tl
          -. NormalizedTree.Nonempty.information_content tr

      module Mappings =
      struct
        include DictOf
            (PairOf(TreePair)(TreePair))
          (PairOf(NonemptyNormalizedPlusStarTreeAlignment)(IntModule))

        let insert
            (m:t)
            (nt1:TreePair.t)
            (nt2:TreePair.t)
            (al:normalized_alignment)
            (count:int)
          : t =
          insert_or_combine
            ~combiner:(fun (al,count1) (_,count2) -> (al,count1+count2))
            m
            (nt1,nt2)
            (al,count)

        let remove
            (m:t)
            (nt1:TreePair.t)
            (nt2:TreePair.t)
            (count:int)
          : t =
          update
            ~updater:(fun (al,old_count) ->
                let remaining = old_count - count in
                (al,remaining))
            m
            (nt1,nt2)

        let lookup_count
            (m:t)
            (nt1:TreePair.t)
            (nt2:TreePair.t)
          : int =
          begin match lookup m (nt1,nt2) with
            | Some (_,count) -> count
            | None -> 0
          end

        module TreeLocationDict =
          DictOf(TreePair)(IntModule)
        let to_aligns
            (m:t)
            (left_indices:TreePair.t -> int)
            (right_indices:TreePair.t -> int)
          : (position * position * normalized_alignment) list =
          let left_p = TreeLocationDict.empty in
          let right_p = TreeLocationDict.empty in
          let tree_tree_align_count = as_kvp_list m in
          let (aligns,_,_) =
            List.fold_left
              ~f:(fun (acc_als,left_p,right_p) ((t1,t2),(al,n)) ->
                  let old_l =
                    TreeLocationDict.lookup_default
                      ~default:0
                      left_p
                      t1
                  in
                  let old_r =
                    TreeLocationDict.lookup_default
                      ~default:0
                      right_p
                      t2
                  in
                  let ind_l = left_indices t1 in
                  let ind_r = right_indices t2 in
                  let new_aligns =
                    List.map
                      ~f:(fun i -> ((ind_l,old_l+i),(ind_r,old_r+i),al))
                      (range 0 n)
                  in
                  let left_p =
                    TreeLocationDict.insert_or_combine
                      ~combiner:(+)
                      left_p
                      t1
                      n
                  in
                  let right_p =
                    TreeLocationDict.insert_or_combine
                      ~combiner:(+)
                      right_p
                      t2
                      n
                  in
                  (new_aligns@acc_als,left_p,right_p))
              ~init:([],left_p,right_p)
              tree_tree_align_count
          in
          aligns
      end

      module ProcessedTreeInfo =
      struct
        module MappedInfoPQueueElement =
        struct
          include TripleOf
              (TreePair)
              (NonemptyNormalizedPlusStarTreeAlignment)
              (FloatModule)
          module Priority = FloatModule
          let priority ((_,_,p):t) = (-1. *. p)
        end

        module MappedInfoPQueue = PriorityQueueOf(MappedInfoPQueueElement)

        type t =
          {
            tree            : TreePair.t         ;
            index           : int                ;
            processed_count : int                ;
            total_count     : int                ;
            mapped_values   : MappedInfoPQueue.t ;
          }
        [@@deriving ord, show, hash, make]

        let init
            ~tree:(tree:TreePair.t)
            ~index:(index:int)
            ~total_count:(total_count:int)
          : t =
          make
            ~index:index
            ~tree:tree
            ~processed_count:0
            ~total_count:total_count
            ~mapped_values:MappedInfoPQueue.empty

        let add_alignments
            (info:t)
            (target:NormalizedTree.Nonempty.t * bool)
            (al:normalized_alignment)
            (count:int)
            (removable:bool)
          : t =
          let mapped_values =
            if removable then
              MappedInfoPQueue.push
                info.mapped_values
                (target
                ,al
                ,net_loss_pure al (info.tree) target)
            else
              info.mapped_values
          in
          { info with
            processed_count = info.processed_count + count;
            mapped_values = mapped_values;
          }

        let free_up_to
            (mapping_counts:TreePair.t -> int)
            (info:t)
            (max:int)
          : t * (TreePair.t
                 * float
                 * int) option =
          let (pq,popped_num_opt) =
            fold_until_completion
              ~f:(fun pq ->
                  begin match MappedInfoPQueue.pop info.mapped_values with
                    | None -> Right (pq,None)
                    | Some (popped,cost,pq) ->
                      let (nt2,al,cost) = popped in
                      let count = mapping_counts nt2 in
                      if count = 0 then
                        Left pq
                      else
                        Right (pq,Some (nt2,al,cost,count))
                  end)
              info.mapped_values
          in
          begin match popped_num_opt with
            | None ->
              let info =
                { info with
                  mapped_values = pq;
                }
              in
              (info,None)
            | Some (nt2,al,cost,count) ->
              let (count,pq) =
                if count <= max then
                  (count,pq)
                else
                  (max
                  ,MappedInfoPQueue.push
                      pq
                      (nt2,al,cost))
              in
              let info =
                { info with
                  processed_count = info.processed_count - count ;
                  mapped_values   = pq  ;
                }
              in
              (info,Some (nt2,cost,count))
          end

        let increase_available
            (info:t)
            (num:int)
          : t =
          { info with
              processed_count = info.processed_count - num
          }
      end

      module TreeInfoDict =
      struct
        include DictOf(TreePair)(ProcessedTreeInfo)

        let remaining_count_exn
            (d:t)
            (nt:TreePair.t)
          : int =
          let info = lookup_exn d nt in
          info.total_count - info.processed_count

        let add_alignments
            (d:t)
            ~key:(k:TreePair.t)
            ~target:(target:TreePair.t)
            ~alignment:(alignment:normalized_alignment)
            ~count:(count:int)
            ~removable:(removable:bool)
          : t =
          update
            ~updater:(fun info ->
                ProcessedTreeInfo.add_alignments
                  info
                  target
                  alignment
                  count
                  removable)
            d
            k

        let free_up_to
            (mapping_counts:TreePair.t -> int)
            (d:t)
            (t:TreePair.t)
            (num:int)
          : t * (TreePair.t * float * int) option =
          let info =
            lookup_exn
              d
              t
          in
          let (info,data_option) =
            ProcessedTreeInfo.free_up_to
              mapping_counts
              info
              num
          in
          let d =
            insert
              d
              t
              info
          in
          (d,data_option)

        let increase_available
            (d:t)
            (t:TreePair.t)
            (num:int)
          : t =
          let info =
            lookup_exn
              d
              t
          in
          let info =
            ProcessedTreeInfo.increase_available
              info
              num
          in
          insert
            d
            t
            info

        let index_exn
            (d:t)
            (t:TreePair.t)
          : int =
          let info = lookup_exn d t in
          info.index

        let get_projections
            (d:t)
          : ((position * float) list) option =
          let projection_opts =
            List.map
              ~f:(fun v ->
                  if v.processed_count <> v.total_count
                  && requires_mapping (fst v.tree) (snd v.tree) then
                    None
                  else
                    let cost =
                      NormalizedTree.Nonempty.information_content
                        (fst v.tree)
                    in
                    (*print_endline @$ Bool.to_string @$ requires_mapping @$ fst @$ v.tree;*)
                    Some (List.map
                            ~f:(fun j -> ((v.index,j),cost))
                            (range v.processed_count v.total_count)))
              (value_list d)
          in
          let projections_opt = distribute_option projection_opts in
          Option.map ~f:List.concat projections_opt

        let total_count
            (d:t)
          : int =
          List.fold_left
            ~f:(fun acc v -> acc + v.total_count)
            ~init:0
            (value_list d)

        let all_positions
            (d:t)
          : position list =
          List.concat_map
            ~f:(fun v ->
                let i = v.index in
                List.map
                  ~f:(fun j -> (i,j))
                  (range 0 v.total_count))
            (value_list d)
      end

      module PrioritiedRemainingElements =
      struct
        include QuadrupleOf
            (TreePair)
            (TreePair)
            (NonemptyNormalizedPlusStarTreeAlignment)
            (FloatModule)
        module Priority = FloatModule
        let priority ((_,_,_,p):t) = p
      end

      module RemainingElementsPQueue =
        PriorityQueueOf(PrioritiedRemainingElements)

      type t =
        {
          t1_dict   : TreeInfoDict.t            ;
          t2_dict   : TreeInfoDict.t            ;
          mappings  : Mappings.t                ;
          remaining : RemainingElementsPQueue.t ;
        }
      [@@deriving show]

        let index
            (s:t)
            (tree_l:TreePair.t)
            (tree_r:TreePair.t)
          : int * int =
          (TreeInfoDict.index_exn s.t1_dict tree_l
          ,TreeInfoDict.index_exn s.t2_dict tree_r)

      let remaining_count
          (s:t)
        : int =
        RemainingElementsPQueue.length s.remaining

      let init
          (recursive_f:GetMinimalAlignmentArg.t -> normalized_alignment option)
          ~left_subtrees:(left_subtrees:(NormalizedTree.Nonempty.l * bool) list)
          ~right_subtrees:(right_subtrees:(NormalizedTree.Nonempty.l * bool) list)
        : t =
        let list_to_dict
            (ts:(NormalizedTree.Nonempty.l * bool) list)
          : TreeInfoDict.t =
          List.foldi
            ~f:(fun i d ((t,c),b) ->
                TreeInfoDict.insert_or_combine
                  ~combiner:(fun _ _ -> failwith "shouldnt merge")
                  d
                  (t,b)
                  (ProcessedTreeInfo.init (t,b) i c))
            ~init:TreeInfoDict.empty
            ts
        in
        let t1_dict = list_to_dict left_subtrees in
        let t2_dict = list_to_dict right_subtrees in
        let t1_keys = TreeInfoDict.key_list t1_dict in
        let t2_keys = TreeInfoDict.key_list t2_dict in
        let relevant_trees =
          (cartesian_filter_map
             ~f:(fun t1 t2 ->
                 let ans_o = recursive_f (GetMinimalAlignmentArg.create (fst t1) (fst t2)) in
                 Option.map ~f:(fun ans -> (t1,t2,ans,net_loss_pure ans t1 t2)) ans_o)
             t1_keys
             t2_keys)
        in
        let remaining = RemainingElementsPQueue.from_list relevant_trees in
        {
          t1_dict   = t1_dict        ;
          t2_dict   = t2_dict        ;
          mappings  = Mappings.empty ;
          remaining = remaining      ;
        }

      let push
          (s:t)
          (t1:TreePair.t)
          (t2:TreePair.t)
          (al:normalized_alignment)
          (c:float)
        : t =
        { s with
          remaining = RemainingElementsPQueue.push s.remaining (t1,t2,al,c)
        }

      let pop
          (s:t)
        : (TreePair.t
           * TreePair.t
           * normalized_alignment
           * float
           * t) option =
        begin match RemainingElementsPQueue.pop s.remaining with
          | None -> None
          | Some ((nt1,nt2,al,pri),_,remaining) ->
            Some
              (nt1
              ,nt2
              ,al
              ,pri
              ,{ s with remaining = remaining; })
        end

      let add_alignments
          (s:t)
          (nt1:TreePair.t)
          (nt2:TreePair.t)
          (a:normalized_alignment)
          (count:int)
        : t =
        let mappings = Mappings.insert s.mappings nt1 nt2 a count in
        let removable =
          not @$ requires_mapping (fst nt1) (snd nt1)
          && not @$ requires_mapping (fst nt2) (snd nt2)
        in
        let t1_dict =
          TreeInfoDict.add_alignments
            s.t1_dict
            ~key:nt1
            ~target:nt2
            ~alignment:a
            ~count:count
            ~removable:removable
        in
        let t2_dict =
          TreeInfoDict.add_alignments
            s.t2_dict
            ~key:nt2
            ~target:nt1
            ~alignment:a
            ~count:count
            ~removable:removable
        in
        { s with
          mappings = mappings ;
          t1_dict  = t1_dict  ;
          t2_dict  = t2_dict  ;
        }

      let remaining_count_left
          (s:t)
          (nt:TreePair.t)
        : int =
        TreeInfoDict.remaining_count_exn s.t1_dict nt

      let remaining_count_right
          (s:t)
          (nt:TreePair.t)
        : int =
        TreeInfoDict.remaining_count_exn s.t2_dict nt

      let total_remaining_left
          (s:t)
        : int =
        List.fold_left
          ~f:(+)
          ~init:0
          (List.map
             ~f:(fun nt -> remaining_count_left s nt)
             (TreeInfoDict.key_list s.t1_dict))

      let total_remaining_right
          (s:t)
        : int =
        List.fold_left
          ~f:(+)
          ~init:0
          (List.map
             ~f:(fun nt -> remaining_count_right s nt)
             (TreeInfoDict.key_list s.t2_dict))

      let remove_left_up_to
          (s:t)
          (nt1:TreePair.t)
          (max_amount:int)
        : t * (int * float) option =
        let (t1_dict,free_left_info_opt) =
          TreeInfoDict.free_up_to
            (Mappings.lookup_count s.mappings nt1)
            s.t1_dict
            nt1
            max_amount
        in
        begin match free_left_info_opt with
          | None ->
            let s =
              { s with
                t1_dict = t1_dict
              }
            in
            (s,None)
          | Some (nt2,cost,removed) ->
            let mappings =
              Mappings.remove
                s.mappings
                nt1
                nt2
                removed
            in
            let t2_dict =
              TreeInfoDict.increase_available
                s.t2_dict
                nt2
                removed
            in
            let s =
              { s with
                mappings = mappings ;
                t1_dict  = t1_dict  ;
                t2_dict  = t2_dict  ;
              }
            in
            (s,Some (removed,cost))
        end

      let remove_right_up_to
          (s:t)
          (nt2:TreePair.t)
          (max_amount:int)
        : t * (int * float) option =
        let (t2_dict,free_right_info_opt) =
          TreeInfoDict.free_up_to
            (fun nt1 -> Mappings.lookup_count s.mappings nt1 nt2)
            s.t2_dict
            nt2
            max_amount
        in
        begin match free_right_info_opt with
          | None ->
            let s =
              { s with
                t2_dict = t2_dict
              }
            in
            (s,None)
          | Some (nt1,cost,removed) ->
            let mappings =
              Mappings.remove
                s.mappings
                nt1
                nt2
                removed
            in
            let t1_dict =
              TreeInfoDict.increase_available
                s.t1_dict
                nt1
                removed
            in
            let s =
              { s with
                mappings = mappings ;
                t1_dict  = t1_dict  ;
                t2_dict  = t2_dict  ;
              }
            in
            (s,Some (removed,cost))
        end

      let check_wellformedness
          (s:t)
        : unit =
        let aligns =
          Mappings.to_aligns
            s.mappings
            (TreeInfoDict.index_exn s.t1_dict)
            (TreeInfoDict.index_exn s.t2_dict)
        in
        let pleft_opt = TreeInfoDict.get_projections s.t1_dict in
        let pright_opt = TreeInfoDict.get_projections s.t2_dict in
        begin match (pleft_opt,pright_opt) with
          | (Some pleft,Some pright) ->
            let positions_l =
              List.sort
                ~compare:compare_position
                (TreeInfoDict.all_positions s.t1_dict)
            in
            let positions_r =
              List.sort
                ~compare:compare_position
                (TreeInfoDict.all_positions s.t2_dict)
            in
            let returned_positions_l =
              List.map ~f:fst3 aligns
              @ List.map ~f:fst pleft
            in
            let returned_positions_r =
              List.map ~f:snd3 aligns
              @ List.map ~f:fst pright
            in
            let symmetric_diff_l =
              symmetric_set_minus
                compare_position
                positions_l
                returned_positions_l
            in
            let symmetric_diff_r =
              symmetric_set_minus
                compare_position
                positions_r
                returned_positions_r
            in
            if not (List.is_empty symmetric_diff_l) then
              failwith
                (string_of_list
                   (string_of_either show_position show_position)
                   symmetric_diff_l);
            if not (List.is_empty symmetric_diff_r) then
              failwith
                (string_of_list
                   (string_of_either show_position show_position)
                   symmetric_diff_r);
          | _ ->
            ()
        end


      let to_alignment
          (tdl:TD.t)
          (tdr:TD.t)
          (s:t)
        : normalized_alignment option =
        check_wellformedness s;
        let aligns =
          Mappings.to_aligns
            s.mappings
            (TreeInfoDict.index_exn s.t1_dict)
            (TreeInfoDict.index_exn s.t2_dict)
        in
        let pleft_opt = TreeInfoDict.get_projections s.t1_dict in
        let pright_opt = TreeInfoDict.get_projections s.t2_dict in
        begin match (pleft_opt,pright_opt) with
          | (Some pleft,Some pright) ->
            Some
              (mk_times
                 tdl
                 tdr
                 aligns
                 pleft
                 pright)
          | _ ->
            None
        end
    end

    module ProcessedPlusTreeInfo =
      QuadrupleOf(IntModule)(IntModule)(IntModule)(IntModule)
    module PlusDataTreeProcessedInfoDict =
      DictOf(NormalizedTree.Nonempty)(ProcessedPlusTreeInfo)
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
    module RemainingPlusElementsPQueue =
      PriorityQueueOf(PrioritiedRemainingPlusElements)

    let get_minimal_alignment_plus
        (recursive_f:GetMinimalAlignmentArg.t -> t option)
        (pl1:PD.t)
        (pl2:PD.t)
        (tts1:(NormalizedTree.Nonempty.l * Probability.t * int) list)
        (tts2:(NormalizedTree.Nonempty.l * Probability.t * int) list)
      : t option =
      if not (PD.are_compatible pl1 pl2) then
        None
      else
        let s =
          PlusMappingState.init
            ~subtrees_l:tts1
            ~subtrees_r:tts2
            recursive_f
        in
        let s =
          fold_until_completion
            ~f:(fun s ->
                begin match PlusMappingState.pop s with
                  | None ->
                    (* no more elements, return *)
                    Right s
                  | Some (nt_l,nt_r,na,c,s) ->
                    let c_updated = PlusMappingState.cost s nt_l nt_r na in
                    if c_updated <> c then
                      (* cost updated since last push, push with updated cost *)
                      (Left
                        (PlusMappingState.push
                           s
                           nt_l
                           nt_r
                           na
                           c_updated))
                    else if
                      not (PlusMappingState.add_productive s nt_l nt_r) then
                      Left s
                    else
                      let count =
                        min
                          (PlusMappingState.available_count_left s nt_l (trd3 nt_r))
                          (PlusMappingState.available_count_right s nt_r (trd3 nt_l))
                      in
                      let s =
                        PlusMappingState.add_alignments
                          s
                          nt_l
                          nt_r
                          na
                          count
                      in
                      Left
                        (PlusMappingState.push
                           s
                           nt_l
                           nt_r
                           na
                           (PlusMappingState.cost s nt_l nt_r na))
                end)
            s
        in
        let aligns_createsl_createsr_opt =
          PlusMappingState.to_aligns_and_creates
            s
        in
        Option.map
          ~f:(fun (aligns,createsl,createsr) ->
              mk_plus
                pl1
                pl2
                aligns
                createsl
                createsr)
          aligns_createsl_createsr_opt

    let get_minimal_alignment_times
        (recursive_f:GetMinimalAlignmentArg.t -> t option)
        (tl1:TD.t)
        (tl2:TD.t)
        (tts1:(NormalizedTree.Nonempty.l * bool) list)
        (tts2:(NormalizedTree.Nonempty.l * bool) list)
      : t option =
      if not (TD.are_compatible tl1 tl2) then
        None
      else
        let s =
          TimesMappingState.init
            recursive_f
            tts1
            tts2
        in
        (*print_endline "REMAINING LEFT";
        print_endline @$ string_of_int @$ TimesMappingState.total_remaining_left s;
        print_endline "REMAINING RIGHT";
        print_endline @$ string_of_int @$ TimesMappingState.total_remaining_right s;
        print_endline "POSSIBLE MAPS";
        print_endline @$ string_of_int @$ TimesMappingState.remaining_count s;
        print_endline "FOLD BEGIN";
        if TimesMappingState.remaining_count s < TimesMappingState.total_remaining_left s then
             print_endline (string_of_list NormalizedTree.Nonempty.show_l tts1);
        if TimesMappingState.remaining_count s < TimesMappingState.total_remaining_right s then
             print_endline (string_of_list NormalizedTree.Nonempty.show_l tts2);*)
        let s =
          fold_until_completion
            ~f:(fun s ->
                begin match TimesMappingState.pop s with
                  | None -> Right (Some s)
                  | Some (nt1,nt2,na,net_loss,s) ->
                    let s_orig = s in
                    let requires_mapping =
                      requires_mapping (fst nt1) (snd nt1)
                      || requires_mapping (fst nt2) (snd nt2)
                    in
                    if net_loss >=. 0.
                    && not requires_mapping then
                      (* would (potentially) total cost to align,
                         this is where the greedy algorithm is potentially
                         suboptimal *)
                      Left s
                    else
                      let remaining_left =
                        TimesMappingState.remaining_count_left
                          s
                          nt1
                      in
                      let remaining_right =
                        TimesMappingState.remaining_count_right
                          s
                          nt2
                      in
                      if remaining_left = 0
                      && remaining_right = 0 then
                        (* nothing left to process for either *)
                        Left s
                      else
                        let (s,remaining_penalty_opt) =
                          if remaining_left = 0 then
                            let (s,remaining_penalty_l_opt) =
                              TimesMappingState.remove_left_up_to
                                s
                                nt1
                                remaining_right
                            in
                            (s
                            ,Option.map
                                ~f:(fun (remaining_left,penalty_left) ->
                                    (remaining_left
                                    ,remaining_right
                                    ,penalty_left
                                    ,0.))
                                remaining_penalty_l_opt)
                          else if remaining_right = 0 then
                            let (s,remaining_penalty_r_opt) =
                              TimesMappingState.remove_right_up_to
                                s
                                nt2
                                remaining_left
                            in
                            (s
                            ,Option.map
                                ~f:(fun (remaining_right,penalty_right) ->
                                    (remaining_left
                                    ,remaining_right
                                    ,0.
                                    ,penalty_right))
                                remaining_penalty_r_opt)
                          else
                            (s,Some (remaining_left,remaining_right,0.,0.))
                        in
                        (*let sp_rleft_pleft_opt =

                          if (remaining_left = 0) then
                            TimesMappingState.remove_left_up_to
                              s
                              nt1
                              remaining_right
                          else
                            Some (s,remaining_left,0.)
                        in*)
                        begin match remaining_penalty_opt with
                          | None ->
                            if requires_mapping then
                              Right None
                            else
                              Left s
                          | Some (remaining_left
                                 ,remaining_right
                                 ,penalty_left
                                 ,penalty_right) ->
                            let net_loss_pure = TimesMappingState.net_loss_pure na nt1 nt2 in
                            let new_net_loss = net_loss_pure
                                               -. penalty_left
                                               -. penalty_right
                            in
                            if net_loss = new_net_loss then
                              (* cost hasn't changed since pushing,
                                 add the alignments *)
                              let amount_processed =
                                min
                                  remaining_left
                                  remaining_right
                              in
                              let s =
                                TimesMappingState.add_alignments
                                  s
                                  nt1
                                  nt2
                                  na
                                  amount_processed
                              in
                              Left
                                (TimesMappingState.push
                                   s
                                   nt1
                                   nt2
                                   na
                                   net_loss_pure)
                            else
                              (* cost updated from when pushed:
                                 push with updated cost *)
                              Left
                                (TimesMappingState.push
                                   s_orig
                                   nt1
                                   nt2
                                   na
                                   new_net_loss)
                        end
                end)
            s
        in
        option_bind
          (TimesMappingState.to_alignment tl1 tl2)
          s

    module UnfixedGetMinimalAlignment =
    struct
      module Arg = GetMinimalAlignmentArg
      module Result = OptionOf(NonemptyNormalizedPlusStarTreeAlignment)
      let f
          (recursive_f:Arg.t -> Result.t)
          (arg:Arg.t)
        : Result.t =
        let (t1,t2) = arg.node in
        begin match (t1.node,t2.node) with
          | (NormalizedTree.Nonempty.Plus (pl1,pts1),
             NormalizedTree.Nonempty.Plus (pl2,pts2)) ->
            get_minimal_alignment_plus recursive_f pl1 pl2 pts1 pts2
          | (NormalizedTree.Nonempty.Times (tl1,tts1),
             NormalizedTree.Nonempty.Times (tl2,tts2)) ->
            get_minimal_alignment_times recursive_f tl1 tl2 tts1 tts2
          | (NormalizedTree.Nonempty.Star (sl1,(sts1,_),_),
             NormalizedTree.Nonempty.Star (sl2,(sts2,_),_)) ->
            if not (SD.are_compatible sl1 sl2) then
              None
            else
              Option.map
                ~f:(fun a -> mk_star sl1 sl2 a)
                (recursive_f (GetMinimalAlignmentArg.create sts1 sts2))
          | (NormalizedTree.Nonempty.Base bd1, NormalizedTree.Nonempty.Base bd2) ->
            Option.map ~f:(fun a -> mk_base a) (BD.get_alignment !lc bd1 bd2)
          | _ -> None
        end
    end

    module Memo = FixHCMemoizerOf(UnfixedGetMinimalAlignment)

    let rec get_minimal_alignment
        (sts1:NormalizedTree.Nonempty.t)
        (sts2:NormalizedTree.Nonempty.t)
      : t option =
      Memo.evaluate (GetMinimalAlignmentArg.create sts1 sts2)

    let clear_dict = Memo.clear

    let rec to_nonempty_and_cost
        (nta:NonemptyNormalizedPlusStarTreeAlignment.t)
        (ns1:NormalizedTree.NormalizationScript.nonempty_t)
        (ns2:NormalizedTree.NormalizationScript.nonempty_t)
      : Nonempty.t * float =
      let cost = cost nta in
      let al:Nonempty.t =
        begin match (nta.node,ns1,ns2) with
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
                  (CreateDict.as_kvp_list cd)
              in
              let sorted_clp =
                List.sort
                  ~compare:(fun (s1,_) (s2,_) -> Int.compare s1 s2)
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
                      let i2 = CountedPermutation.apply_inverse_exn perm2 p2 in
                      let ns1 =
                        fst3 @$
                        List.nth_exn
                          nsl1
                          i1
                      in
                      let ns2 =
                        fst3 @$
                        List.nth_exn
                          nsl2
                          i2
                      in
                      (i1,i2,fst (to_nonempty_and_cost nta ns1 ns2)))
                  (MappingDict.all_alignments md)
              in
              Plus (pl1',pl2',all_aligns,cl1,cl2)
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
            if not (is_equal (TD.compare tl1 tl1')) || not (is_equal (TD.compare tl2 tl2')) then
              failwith ("bad application of normalization script"
                ^ TD.show tl1 ^ TD.show tl1')
            else
              let pl1 = List.map ~f:fst pl1 in
              let pl2 = List.map ~f:fst pl2 in
              (*print_endline "startprojections";
              print_endline (string_of_list show_position pl1);
              print_endline (string_of_list show_position pl2);
              print_endline (string_of_list (fun (p1,p2,_) -> show_position p1 ^ "," ^ show_position p2) als);
              print_endline (CountedPermutation.show perm1);
              print_endline (CountedPermutation.show perm2);
              print_endline "endprojections\n\n\n";*)
              let projs1 = transform_projection_list perm1 pl1 in
              let projs2 = transform_projection_list perm2 pl2 in
              let aligns =
                List.map
                  ~f:(fun (p1,p2,al) ->
                      let i1 = CountedPermutation.apply_inverse_exn perm1 p1 in
                      let i2 = CountedPermutation.apply_inverse_exn perm2 p2 in
                      let ns1 =
                        fst (List.nth_exn
                               nsl1
                               i1)
                      in
                      let ns2 =
                        fst (List.nth_exn
                          nsl2
                          i2)
                      in
                      (i1,i2,fst (to_nonempty_and_cost al ns1 ns2)))
                  als
              in
              Times (tl1',tl2',aligns,projs1,projs2)
          | (Star (sl1,sl2,ns'),Star (sl1',_,ns1'), Star (sl2',_,ns2')) ->
            if (sl1 <> sl1') || (sl2 <> sl2') then
              failwith "bad application of normalization script"
            else
              Star (sl1,sl2,fst (to_nonempty_and_cost ns' ns1' ns2'))
          | (Base ba,Base bl,Base bl') ->
            Base ba
          | _ -> failwith "bad application of normalization script"
        end
      in
      (al,cost)
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
        failwith "not allowed"
    end

  module Tree = PlusTimesStarTreeOf(PD)(TD)(SD)(BD)

  let get_minimal_alignment_and_cost
      (t1:Tree.t)
      (t2:Tree.t)
    : t option * float =
    let (normalized_t1,script1) = NormalizedTree.from_tree t1 in
    let (normalized_t2,script2) = NormalizedTree.from_tree t2 in
    begin match (normalized_t1,normalized_t2) with
      | (NormalizedTree.Empty,NormalizedTree.Empty) -> (Some Empty,0.)
      | (NormalizedTree.Nonempty nt1,NormalizedTree.Nonempty nt2) ->
        (*print_endline (NormalizedTree.NormalizationScript.show script1);
        print_endline (NormalizedTree.NormalizationScript.show script2);*)
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
        begin match nonempty_normalized_alignment with
          | Some nna ->
            let (na,c) =
              NonemptyNormalizedPlusStarTreeAlignment.to_nonempty_and_cost
                nna
                script1
                script2
            in
            (*if c >. 0. then print_endline (Nonempty.show na);*)
            (Some (NonemptyTree na),c)
          | None -> (None,1.)
        end
      | _ -> (None,1.)
    end

  let get_alignment_distance
      (t1:Tree.t)
      (t2:Tree.t)
    : float =
    snd (get_minimal_alignment_and_cost t1 t2)
end
