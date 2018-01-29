open Stdlib
open Lang

(***** The main LensContext module {{{ *****)
module LensContext = struct
  module OutgoingD = DictOf(Regex)(ListOf(PairOf(Lens)(Regex)))

  module DS = DisjointSetOf(Regex)

  type t = { outgoing : OutgoingD.t ;
             equivs   : DS.t        ; }
  [@@deriving ord, show, hash]

  let empty = { outgoing = OutgoingD.empty ;
                equivs   = DS.empty        ; }

  let update_outgoing (outgoing:OutgoingD.t)
      (r1:Regex.t) (r2:Regex.t) (l:Lens.t)
    : OutgoingD.t =
    let outgoing = begin match OutgoingD.lookup outgoing r1 with
      | None -> OutgoingD.insert outgoing r1 [(l,r2)]
      | Some ol -> OutgoingD.insert outgoing r1 ((l,r2)::ol)
    end in
    let outgoing = begin match OutgoingD.lookup outgoing r2 with
      | None -> OutgoingD.insert outgoing r2 [(Lens.LensInverse l,r1)]
      | Some ol -> OutgoingD.insert outgoing r2 ((Lens.LensInverse l,r1)::ol)
    end in
    outgoing

  let update_equivs (equivs:DS.t) (r1:Regex.t) (r2:Regex.t)
    : DS.t =
    DS.union_elements
      equivs
      r1
      r2

  (* TODO: is this the right thing, simpler if just between vars ? *)
  let insert (lc:t) (l:Lens.t) (r1:Regex.t) (r2:Regex.t) : t =
    begin match (r1,r2) with
      | (Regex.RegExClosed r1, Regex.RegExClosed r2) ->
        { outgoing = update_outgoing lc.outgoing r1 r2 (Lens.LensClosed l);
          equivs   = update_equivs lc.equivs r1 r2       ; }
      | _ ->
        failwith "something went wrong"
    end

  let insert_list (lc:t) (nirsl:(Lens.t * Regex.t * Regex.t) list) : t =
    List.fold_left
      ~f:(fun acc (l,r1,r2) -> insert acc l r1 r2)
      ~init:lc
      nirsl

  let get_outgoing_edges (outgoing:OutgoingD.t) (source:Regex.t)
    : (Lens.t * Regex.t) list =
    begin match OutgoingD.lookup outgoing source with
      | None -> []
      | Some connections -> connections
    end

  let create_from_list (nirsl:(Lens.t * Regex.t * Regex.t) list) : t =
    insert_list empty nirsl

  let shortest_path (lc:t) (r1:Regex.t) (r2:Regex.t)
    : Lens.t option =
    let outgoing = lc.outgoing in
    let rec shortest_path_internal (accums:(Lens.t * Regex.t) list) : Lens.t =
      let satisfying_path_option =
        List.find
          ~f:(fun (_,n) -> n = r2)
          accums
      in
      begin match satisfying_path_option with
        | None ->
          let accums =
            List.concat_map
              ~f:(fun (l,n) ->
                  let valid_outgoing_edges =
                    List.filter
                      ~f:(fun (l',_) -> not (Lens.has_common_sublens l' l))
                      (get_outgoing_edges outgoing n)
                  in
                  List.map
                    ~f:(fun (l',n') -> (Lens.LensCompose (l',l),n'))
                    valid_outgoing_edges)
              accums
          in
          shortest_path_internal accums
        | Some (l,_) -> l
      end
    in
    let regex1_rep = DS.find_representative lc.equivs r1 in
    let regex2_rep = DS.find_representative lc.equivs r2 in
    if regex1_rep <> regex2_rep then
      None
    else if r1 = r2 then
      Some (Lens.LensIdentity r1)
    else
      Some (shortest_path_internal (get_outgoing_edges outgoing r1))

  let shortest_path_exn (lc:t) (r1:Regex.t) (r2:Regex.t)
    : Lens.t =
    begin match shortest_path lc r1 r2 with
      | None -> 
        failwith "regexes not in same equivalence class"
      | Some l -> l
    end

  let shortest_path_to_rep_elt (lc:t) (r:Regex.t) : Regex.t * Lens.t =
    let rep_element = DS.find_representative lc.equivs r in
    let shortest_path = shortest_path_exn lc r rep_element in
    (rep_element,shortest_path)
end

(***** }}} *****)
