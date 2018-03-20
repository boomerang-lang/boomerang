open Core
open Util
open My_dict

(* parameter to Set modules -- we must pass in some 
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module SetOf(C : Data) =
struct
  module D = DictOf(C)(UnitModule)

  type elt = D.key
  [@@deriving ord, show, hash]

  type t = D.t
  [@@deriving ord, show, hash]

  let empty = D.empty
  let insert x s = D.insert s x ()
  let singleton x = insert x empty
  let union s1 s2 = D.fold (fun x _ s -> insert x s) s2 s1
  let member = D.member
  let subset s1 s2 =
    D.fold (fun x _ acc -> acc && (member s2 x)) true s1
  let intersect s1 s2 = 
    D.fold (fun x _ s -> if member s2 x then insert x s else s) 
      empty s1
  let minus s1 s2 =
    D.fold (fun x _ s -> if not (member s2 x) then insert x s else s)
      empty s1
  let remove x s = D.remove s x
  let choose s = 
    match D.choose s with
      | None -> None
      | Some (k,_,s') -> Some (k,s')
  let fold ~f:f ~init:u s = D.fold (fun x _ a -> f x a) u s
  let map
      ~f:(f:elt -> elt)
    : t -> t =
    fold
      ~f:(insert % f)
      ~init:empty

  let filter
      ~f:(f:elt -> bool)
    : t -> t =
    fold
      ~f:(fun e s -> if f e then insert e s else s)
      ~init:empty

  let from_list (es:elt list) : t =
    D.from_kvp_list
      (List.map
         ~f:(fun e -> (e,()))
         es)

  let as_list s =
    List.map ~f:fst (D.as_kvp_list s)

  let is_empty s = D.is_empty s

  let compare
      (s1:t)
      (s2:t)
    : comparison =
    compare_list
      ~cmp:C.compare
      (List.sort ~cmp:C.compare (as_list s1))
      (List.sort ~cmp:C.compare (as_list s2))

  let po_compare
      (s1:t)
      (s2:t)
    : partial_order_comparison =
    begin match (subset s1 s2, subset s2 s1) with
      | (true,true) -> PO_EQ
      | (true,false) -> PO_LT
      | (false,true) -> PO_GT
      | (false,false) -> PO_INCOMPARABLE
    end

  let max
      (s:t)
    : elt option =
    D.max_key s

  let max_exn
      (s:t)
    : elt =
    D.max_key_exn s

  let size : t -> int = D.size
end
