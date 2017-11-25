open Core
open Util
open My_dict

module DisjointSetOf(DA : Data) =
struct
  module D = DictOf(DA)(RefOf(DA))

  type elt = DA.t

  type t = D.t
  [@@deriving ord, show, hash]

  let empty = D.empty

  let rec find_representative (ds:t) (e:'a) : 'a =
    begin match D.lookup ds e with
      | None -> e
      | Some pref ->
        let rep = find_representative ds !pref in
        pref := rep;
        rep
    end

  let union_elements (ds:t) (e1:elt) (e2:elt) : t =
    let e1rep = find_representative ds e1 in
    let e2rep = find_representative ds e2 in
    if (e1rep = e2rep) then
      ds
    else
      D.insert ds e1rep (ref e2rep)

  let create_from_equivalences
      (equivs:(elt * elt) list) : t =
    List.fold_left
      ~f:(fun acc (e1,e2) ->
          union_elements acc e1 e2)
      ~init:(empty)
      equivs
end
