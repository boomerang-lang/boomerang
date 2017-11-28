open Stdlib
open Lang

(***** The main LensContext module {{{ *****)
module LensContext = struct
  module DefsD = DictOf(Id)(TripleOf(Lens)(Regex)(Regex))

  module OutgoingD = DictOf(Id)(ListOf(PairOf(Lens)(Id)))

  module DS = DisjointSetOf(Id)

  type t = { defs     : DefsD.t     ;
             outgoing : OutgoingD.t ;
             equivs   : DS.t        ; }
  [@@deriving ord, show, hash]

  let empty = { defs     = DefsD.empty     ;
                outgoing = OutgoingD.empty ;
                equivs   = DS.empty        ; }

  let lookup_exn (lc:t) (name:Id.t) : Lens.t*Regex.t*Regex.t =
    DefsD.lookup_exn lc.defs name

  let lookup_type_exn (lc:t) (name:Id.t) : Regex.t*Regex.t =
    let (_,r1,r2) = lookup_exn lc name in
    (r1,r2)

  let lookup_impl_exn (lc:t) (name:Id.t) : Lens.t =
    let (l,_,_) = lookup_exn lc name in
    l

  let update_defs (defs:DefsD.t)
      (name:Id.t) (l:Lens.t) (r1:Regex.t) (r2:Regex.t)
    : DefsD.t =
    if not (DefsD.member defs name) then
      DefsD.insert defs name (l,r1,r2)
    else
      failwith "bad insert"

  let update_outgoing (outgoing:OutgoingD.t)
      (id1:Id.t) (id2:Id.t) (l:Lens.t)
    : OutgoingD.t =
    let outgoing = begin match OutgoingD.lookup outgoing id1 with
      | None -> OutgoingD.insert outgoing id1 [(l,id2)]
      | Some ol -> OutgoingD.insert outgoing id1 ((l,id2)::ol)
    end in
    let outgoing = begin match OutgoingD.lookup outgoing id2 with
      | None -> OutgoingD.insert outgoing id2 [(Lens.LensInverse l,id1)]
      | Some ol -> OutgoingD.insert outgoing id2 ((Lens.LensInverse l,id1)::ol)
    end in
    outgoing

  let update_equivs (equivs:DS.t) (id1:Id.t) (id2:Id.t)
    : DS.t =
    DS.union_elements
      equivs
      id1
      id2

  (* TODO: is this the right thing, simpler if just between vars ? *)
  let insert_exn (lc:t) (name:Id.t) (l:Lens.t) (r1:Regex.t) (r2:Regex.t) : t =
    begin match (r1,r2) with
      | (Regex.RegExVariable id1, Regex.RegExVariable id2) ->
        { defs     = update_defs lc.defs name l r1 r2      ;
          outgoing = update_outgoing lc.outgoing id1 id2 (Lens.LensVariable name);
          equivs   = update_equivs lc.equivs id1 id2       ; }
      | _ -> 
        { defs     = update_defs lc.defs name l r1 r2 ;
          outgoing = lc.outgoing                      ;
          equivs   = lc.equivs                        ; }
    end

  let insert_list_exn (lc:t) (nirsl:(Id.t * Lens.t * Regex.t * Regex.t) list) : t =
    List.fold_left
      ~f:(fun acc (name,l,r1,r2) -> insert_exn acc name l r1 r2)
      ~init:lc
      nirsl

  let get_outgoing_edges (outgoing:OutgoingD.t) (source:Id.t)
    : (Lens.t * Id.t) list =
    begin match OutgoingD.lookup outgoing source with
      | None -> []
      | Some connections -> connections
    end

  let create_from_list_exn (nirsl:(Id.t * Lens.t * Regex.t * Regex.t) list) : t =
    insert_list_exn empty nirsl

  let shortest_path (lc:t) (regex1_name:Id.t) (regex2_name:Id.t)
    : Lens.t option =
    let outgoing = lc.outgoing in
    let rec shortest_path_internal (accums:(Lens.t * Id.t) list) : Lens.t =
      let satisfying_path_option =
        List.find
          ~f:(fun (_,n) -> n = regex2_name)
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
    let regex1_rep = DS.find_representative lc.equivs regex1_name in
    let regex2_rep = DS.find_representative lc.equivs regex2_name in
    if regex1_rep <> regex2_rep then
      None
    else if regex1_name = regex2_name then
      Some (Lens.LensIdentity (Regex.RegExVariable regex1_name))
    else
      Some (shortest_path_internal (get_outgoing_edges outgoing regex1_name))

  let shortest_path_exn (lc:t) (regex1_name:Id.t) (regex2_name:Id.t)
    : Lens.t =
    begin match shortest_path lc regex1_name regex2_name with
      | None -> 
        failwith "regexes not in same equivalence class"
      | Some l -> l
    end

  let shortest_path_to_rep_elt (lc:t) (regex_name:Id.t) : Id.t * Lens.t =
    let rep_element = DS.find_representative lc.equivs regex_name in
    let shortest_path = shortest_path_exn lc regex_name rep_element in
    (rep_element,shortest_path)

  let autogen_id_from_base (lc:t) (base:string) : Id.t =
    let rec fresh nopt =
      let (x,next) =
        begin match nopt with
          | None -> (base,Some 1)
          | Some n -> (Printf.sprintf "%s%d" base n, Some (n+1))
        end
      in
      if DefsD.member lc.defs (Id.Id x) then
        fresh next
      else
        x
    in
    Id.Id (fresh None)

  let autogen_id (lc:t) (l:Lens.t) : Id.t =
    let base = Lens.show l in
    let rec fresh nopt =
      let (x,next) =
        begin match nopt with
          | None -> (base,Some 1)
          | Some n -> (Printf.sprintf "%s%d" base n, Some (n+1))
        end
      in
      begin match DefsD.lookup lc.defs (Id.Id x) with
        | Some (l',_,_) ->
          if l = l' then
            x
          else
            fresh next
        | _ -> x
      end
    in
    Id.Id (fresh None)
      
  let autogen_fresh_id (lc:t) : Id.t =
    autogen_id_from_base lc "l"
end

(***** }}} *****)
