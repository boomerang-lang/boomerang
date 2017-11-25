open Core
open Util

(* Heavily using http://typeocaml.com/2015/03/12/heap-leftist-tree/ *)

module HeapOf(D:Data) =
struct
  type element = D.t
  [@@deriving show, hash]

  type t =
    | Leaf
    | Node of t * element * t * int
  [@@deriving show, hash]

  let empty : t = Leaf

  let singleton (e:element) : t =
    Node(Leaf,e,Leaf,1)

  let rank (h:t) : int =
    begin match h with
      | Leaf -> 0
      | Node (_,_,_,r) -> r
    end
    
  let rec merge (h1:t) (h2:t) : t =
    begin match (h1,h2) with
      | (Leaf,_) -> h2
      | (_,Leaf) -> h1
      | (Node(lh,e1,rh,_), Node(_,e2,_,_)) ->
        let cmp = D.compare e1 e2 in
        if (is_gt cmp) then
          merge h2 h1
        else
          let merged = merge rh h2 in
          let rank_left = rank lh in
          let rank_right = rank merged in
          if rank_left >= rank_right then
            Node (lh, e1, merged, rank_right+1)
          else
            Node (merged, e1, lh, rank_left+1)
    end
    
  let push (h:t) (e:element) : t =
    merge h (singleton e)

  let pop (h:t) : (element * t) option =
    begin match h with
      | Leaf -> None
      | Node (lh, e, rh, _) -> Some (e, merge lh rh)
    end

  let peek (h:t) : element option =
    begin match h with
      | Leaf -> None
      | Node (_, e, _, _) -> Some e
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
      | Node(lh,e,rh,_) -> e::((to_list lh)@(to_list rh))
    end

  let compare (h1:t) (h2:t) : comparison =
    let h1es = List.sort ~cmp:D.compare (to_list h1) in
    let h2es = List.sort ~cmp:D.compare (to_list h2) in
    compare_list
      ~cmp:D.compare
      h1es
      h2es
end
