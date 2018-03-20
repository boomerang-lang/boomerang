open Core
open Util

(* Heavily using http://typeocaml.com/2015/03/12/heap-leftist-tree/ *)

module HeapOf(D:Data) =
struct
  type element = D.t
  [@@deriving show, hash]

  type t =
    | Leaf
    | Node of t * (element list) * t * int
  [@@deriving show, hash]

  let empty : t = Leaf

  let singleton (e:element) : t =
    Node(Leaf,[e],Leaf,1)

  let rank (h:t) : int =
    begin match h with
      | Leaf -> 0
      | Node (_,_,_,r) -> r
    end
    
  let rec merge (h1:t) (h2:t) : t =
    begin match (h1,h2) with
      | (Leaf,_) -> h2
      | (_,Leaf) -> h1
      | (Node(lh1,e1,rh1,_), Node(lh2,e2,rh2,i)) ->
        let cmp = D.compare (List.hd_exn e1) (List.hd_exn e2) in
        begin match make_matchable cmp with
          | GT ->
            merge h2 h1
          | LT ->
            let merged = merge rh1 h2 in
            let rank_left = rank lh1 in
            let rank_right = rank merged in
            if rank_left >= rank_right then
              Node (lh1, e1, merged, rank_right+1)
            else
              Node (merged, e1, lh1, rank_left+1)
          | EQ ->
            let h2 = Node(lh2,e2@e1,rh2,i) in
            let merged_once = merge lh1 h2 in
            merge lh2 merged_once
        end
    end
    
  let push (h:t) (e:element) : t =
    merge h (singleton e)

  let pop (h:t) : (element * t) option =
    begin match h with
      | Leaf -> None
      | Node (lh, [e], rh, _) -> Some (e, merge lh rh)
      | Node (lh, e::es, rh, i) -> Some (e, Node (lh, es, rh, i))
      | _ -> failwith "bad heap"
    end

  let pop_all_equiv (h:t) : (element list * t) option =
    begin match h with
      | Leaf -> None
      | Node (lh, es, rh, _) -> Some (es, merge lh rh)
    end

  let peek (h:t) : element option =
    begin match h with
      | Leaf -> None
      | Node (_, e::_, _, _) -> Some e
      | Node (_, _, _, _) -> failwith "bad heap"
    end

  (*let rec to_string (h:heap) : string =
    begin match h with
      | Leaf -> "Leaf"
      | Node(lh,e,rh,rank) ->
        "Node" ^
        (string_of_quadruple
           to_string
           H.to_string
           to_string
           string_of_int
           (lh,e,rh,rank))
    end*)

  let rec size (h:t) : int =
    begin match h with
      | Leaf -> 0
      | Node(lh,_,rh,_) -> 1 + (size lh) + (size rh)
    end

  let rec to_list (h:t) : element list =
    begin match h with
      | Leaf -> []
      | Node(lh,es,rh,_) -> es@((to_list lh)@(to_list rh))
    end

  let compare (h1:t) (h2:t) : comparison =
    let h1es = List.sort ~cmp:D.compare (to_list h1) in
    let h2es = List.sort ~cmp:D.compare (to_list h2) in
    compare_list
      ~cmp:D.compare
      h1es
      h2es
end
