open Core
open Util
open My_set
open My_heap

module type DataWithPriority =
sig
  type t
  module Priority : Data
  val show : t shower
  val pp : t pper
  val compare : t comparer
  val hash : t hasher
  val hash_fold_t : t hash_folder
  val priority : t -> Priority.t
end

module PriorityQueueOf(D:DataWithPriority) =
struct
  module QueueHeap =
    HeapOf(
    struct
      type t = (D.t * D.Priority.t)
      [@@deriving show, hash]

      let compare =
        (fun (_,f1) (_,f2) ->
             (D.Priority.compare f1 f2))
      let to_string = fun _ -> "hi"
    end)

  type t = QueueHeap.t
  [@@deriving show, hash]

  type element = D.t

  let empty = (QueueHeap.empty)

  let push (h:t) (e:element) : t =
      let pri = D.priority e in
      let h' = QueueHeap.push h (e,pri) in
      (h')

  let push_all (q:t) (es:element list) : t =
    List.fold_left
      ~f:(fun q e -> push q e)
      ~init:q
      es

  let from_list (es:element list) : t =
    push_all empty es

  let singleton (e:element) : t =
    from_list [e]

  let pop ((h):t) : (D.t * D.Priority.t * t) option =
    Option.map ~f:(fun ((e,p),h') -> (e,p,(h'))) (QueueHeap.pop h)

  let pop_exn (q:t) : D.t * D.Priority.t * t =
    begin match pop q with
      | None -> failwith "failure: pop_exn"
      | Some e -> e
    end

  let peek ((h):t) : D.t option =
    Option.map ~f:fst (QueueHeap.peek h)

  let peek_exn (h:t) : D.t =
    Option.value_exn (peek h)

  let delete : t -> t option =
    Option.map ~f:trd_trip % pop

  let delete_exn : t -> t =
    trd_trip % pop_exn

  let all_remaining ((h):t) : (D.t * D.Priority.t) list =
    QueueHeap.to_list h

  let rec pop_until_min_pri_greater_than
      (q:t)
      (f:D.Priority.t)
    : (element * D.Priority.t) list * t =
      begin match pop q with
        | None -> ([],q)
        | Some (e,f',q') ->
          if D.Priority.compare f' f > 0 then
            ([],q)
          else
            let (efs,q'') = pop_until_min_pri_greater_than q' f in
            ((e,f')::efs,q'')
      end

  let rec pop_until_new_priority
      (q:t)
    : (D.Priority.t * element list * t) option =
    begin match (QueueHeap.pop_all_equiv q) with
      | Some ((pes),q) ->
        let p = snd (List.hd_exn pes) in
        let es = List.map ~f:(fst) pes in
        Some (p,es,q)
      | None -> None
    end

  let length ((h):t) : int = QueueHeap.size h

  let compare
    : (QueueHeap.t) comparer =
    let real_heap_compare
        (qh1:QueueHeap.t)
        (qh2:QueueHeap.t)
      : comparison =
      let ordered_qhl1 =
        List.sort
          ~compare:D.compare
          (List.map ~f:fst (QueueHeap.to_list qh1))
      in
      let ordered_qhl2 =
        List.sort
          ~compare:D.compare
          (List.map ~f:fst (QueueHeap.to_list qh2))
      in
      compare_list
        ~cmp:D.compare
        ordered_qhl1
        ordered_qhl2
    in
      real_heap_compare
end
