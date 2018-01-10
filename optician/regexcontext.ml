open Stdlib
open Lang

(***** The main RegexContext module {{{ *****)

module RegexContext = struct
  module D = DictOf(Id)(PairOf(Regex)(BoolModule))
  type t = D.t
  [@@deriving ord, show, hash]

    let empty = D.empty

    let lookup_everything (rc:t) (name:Id.t) : (Regex.t*bool) option =
      D.lookup rc name

    let lookup (rc:t) (name:Id.t) : Regex.t option =
      Option.map ~f:fst (lookup_everything rc name)

    let lookup_exn (rc:t) (name:Id.t) : Regex.t =
      begin match lookup rc name with
        | None -> failwith ("lookup_exn: " ^ (Id.show name) ^ " not found")
        | Some v -> v
      end

    let insert_exn (rc:t) (name:Id.t) (r:Regex.t) (is_abstract:bool) : t =
      begin match lookup_everything rc name with
        | None -> D.insert rc name (r,is_abstract)
        | Some ra ->
            if ra = (r,is_abstract) then
              rc
            else
              failwith ((Id.show name) ^ " already exists in the context")
      end

    let insert_list_exn (rc:t) (nral:(Id.t * Regex.t * bool) list) : t =
      List.fold_left
        ~f:(fun acc (name,r,b) -> insert_exn acc name r b)
        ~init:rc
        nral

    let create_from_list_exn (nral:(Id.t * Regex.t * bool) list) : t =
      insert_list_exn empty nral

    let autogen_id (_:t) (r:Regex.t) : Id.t =
      (*let base = Regex.show r in
      let rec fresh n =
        let x = Id.Id (Printf.sprintf "%s%d" base n) in
        begin match D.lookup rc x with
          | Some (r',false) ->
            if is_equal (Regex.compare r r') then
              x
            else
              fresh (n+1)
          | Some _ -> fresh (n+1)
          | _ -> x
        end
      in
        fresh 1*)
      Id.Id (Regex.show r)

    let autogen_fresh_id (rc:t) : Id.t =
      let base = "r" in
      let rec fresh n =
        let x = Id.Id (Printf.sprintf "%s%d" base n) in
        begin match D.lookup rc x with
          | Some _ -> fresh (n+1)
          | _ -> x
        end
      in
      fresh 1

    let contains = D.member

    let lookup_for_expansion_exn (rc:t) (name:Id.t) : Regex.t option =
      begin match lookup_everything rc name with
        | None -> failwith ("bad regex name: " ^ (Id.show name))
        | Some (r,abs) ->
          if abs then
            None
          else
            Some r
      end

    let merge_contexts_exn (rc1:t) (rc2:t) : t =
      let rc2_list = D.as_kvp_list rc2 in
      insert_list_exn rc1 (List.map ~f:(fun (n,(r,b)) -> (n,r,b)) rc2_list)

    let compare (rc1:t) (rc2:t) : comparison =
      D.compare rc1 rc2

    let size (rc:t) : int =
      D.size rc
end

(***** }}} *****)
