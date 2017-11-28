open Stdlib

(** To be an ordered metric space, we require the space to have a distance
  * metric (a function distance : X -> X -> double) and a total ordering (a function
  * compare : X -> X -> int) we do not require any meaningful interaction
  * between the metric and the ordering **)
module TreeAlignmentOf(D : Data) =
struct
  type alignment_node =
    {
      perm   : Permutation.t  ;
      pleft  : int list       ;
      pright : int list       ;
    }
  [@@deriving ord, show, hash]

  let create_alignment_node
      ~perm:(perm:Permutation.t)
      ~pleft:(pleft:int list)
      ~pright:(pright:int list)
    : alignment_node =
    {
      perm   = perm   ;
      pleft  = pleft  ;
      pright = pright ;
    }


  type t = (alignment_node tree) option
  [@@deriving ord, show, hash]

  let rec nonempty_cost
      (Node(an,al):alignment_node nonempty_tree)
    : float =
    let mapped_count = List.length al in
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
    (unnormalized_cost /. total_size)

  let cost
      (a:t)
    : float = 
    begin match a with
      | None -> 1.0
      | Some EmptyTree -> 0.0
      | Some NonemptyTree nea -> nonempty_cost nea
    end

  module DataTree = UnorderedNonemptyTreeOf(D)
  module DataTreeIndexListDict = DictOf(DataTree)(ListOf(IntModule))
  module PrioritiedDataTreePairs =
  struct
    include TripleOf(DataTree)(DataTree)(FloatModule)
    let priority ((_,_,p):t) = p
  end
  module DataTreeDataTreePriorityPQueue = PriorityQueueOf(PrioritiedDataTreePairs)

  let get_minimal_alignment
      (t1:D.t tree)
      (t2:D.t tree)
    : t =
    let rec get_nonempty_alignment_distance
        (t1:D.t nonempty_tree)
        (t2:D.t nonempty_tree)
      : float =
      begin match get_minimal_alignment_nonempty t1 t2 with
        | None -> 1.0
        | Some nea -> nonempty_cost nea
      end

    and get_minimal_alignment_nonempty
        (Node(l1,t1s):D.t nonempty_tree)
        (Node(l2,t2s):D.t nonempty_tree)
      : alignment_node nonempty_tree option =
      if (not (is_equal (D.compare l1 l2))) then
        None
      else
        let list_to_dict
            (ts:D.t nonempty_tree list)
          : DataTreeIndexListDict.t =
          List.foldi
            ~f:(fun i d t ->
                DataTreeIndexListDict.insert_or_merge
                  ~merge:(fun il1 il2 -> il2@il1)
                  d
                  t
                  [i])
            ~init:DataTreeIndexListDict.empty
            ts
        in
        let d1 = list_to_dict t1s in
        let d2 = list_to_dict t2s in
        let t1_keys = DataTreeIndexListDict.key_list d1 in
        let t2_keys = DataTreeIndexListDict.key_list d2 in
        let pq =
          DataTreeDataTreePriorityPQueue.from_list
            (cartesian_map
               ~f:(fun t1 t2 -> (t1,t2,get_nonempty_alignment_distance t1 t2))
               t1_keys
               t2_keys)
        in
        let (indices_and_alignments,pleft,pright) =
          fold_until_completion
            ~f:(fun (pq,d1,d2,indices_and_alignments) ->
                begin match DataTreeDataTreePriorityPQueue.pop pq with
                  | None ->
                    let leftover_left =
                      List.concat
                        (DataTreeIndexListDict.value_list d1)
                    in
                    let leftover_right =
                      List.concat
                        (DataTreeIndexListDict.value_list d2)
                    in
                    Right(indices_and_alignments,leftover_left,leftover_right)
                  | Some ((t1,t2,_),_,pq) ->
                    let l1o = DataTreeIndexListDict.lookup d1 t1 in
                    let l2o = DataTreeIndexListDict.lookup d2 t2 in
                    begin match (l1o,l2o) with
                      | (None   ,_       ) ->
                        Left (pq,d1,d2,indices_and_alignments)
                      | (_      ,    None) ->
                        Left (pq,d1,d2,indices_and_alignments)
                      | (Some l1, Some l2) ->
                        let index_to = max (List.length l1) (List.length l2) in
                        let (l11,l12) = split_at_index_exn l1 index_to in
                        let (l21,l22) = split_at_index_exn l2 index_to in
                        let new_indices_and_alignment_options =
                          List.map
                            ~f:(fun (i,j) ->
                                let t1 = List.nth_exn t1s i in
                                let t2 = List.nth_exn t2s j in
                                let alignment_option =
                                  get_minimal_alignment_nonempty
                                    t1
                                    t2
                                in
                                Option.map
                                  ~f:(fun a -> (i,j,a))
                                  alignment_option)
                            (List.zip_exn l11 l21)
                        in
                        let new_indices_and_alignments_option =
                          distribute_option
                            new_indices_and_alignment_options
                        in
                        begin match new_indices_and_alignments_option with
                          | None -> 
                            Left (DataTreeDataTreePriorityPQueue.empty,d1,d2,indices_and_alignments)
                          | Some new_indices_and_alignments ->
                            let indices_and_alignments =
                              new_indices_and_alignments@indices_and_alignments
                            in
                            let updater_with_replacement
                                (to_replace:int list)
                                (_:int list)
                              : (int list) option =
                              if (List.is_empty to_replace) then
                                None
                              else
                                Some to_replace
                            in
                            let d1 =
                              DataTreeIndexListDict.remove_or_update
                                ~updater:(updater_with_replacement l12)
                                d1
                                t1
                            in
                            let d2 =
                              DataTreeIndexListDict.remove_or_update
                                ~updater:(updater_with_replacement l22)
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
          (Node
             (create_alignment_node
                ~perm:(Permutation.create_from_doubles perm_doubles)
                ~pleft:pleft
                ~pright:pright,
              alignments))
    in
    begin match (t1,t2) with
      | (EmptyTree, EmptyTree) -> Some EmptyTree
      | (EmptyTree, NonemptyTree _) -> None
      | (NonemptyTree _, EmptyTree) -> None
      | (NonemptyTree nt1, NonemptyTree nt2) ->
        Option.map
          ~f:(fun nea -> NonemptyTree nea)
          (get_minimal_alignment_nonempty nt1 nt2)
    end

  let get_alignment_distance
      (t1:D.t tree)
      (t2:D.t tree)
    : float =
    cost (get_minimal_alignment t1 t2)
end
