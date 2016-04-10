module type SMap = sig
  type key_t
  type key_set
  type 'a t
  val empty: 'a t 
  val is_empty : 'a t -> bool
  val size : 'a t -> int
  val domain: 'a t -> key_set
  val add: key_t -> 'a -> 'a t -> 'a t
  val combine : 'a t -> 'a t -> 'a t
  val find: key_t -> 'a t -> 'a
  val safe_find: key_t -> 'a t -> 'a -> 'a
  val from_list: (key_t * 'a) list -> 'a t
  val from_function: key_set -> (key_t -> 'a) -> 'a t
  val remove: key_t -> 'a t -> 'a t
  val mem:  key_t -> 'a t -> bool
  val project: key_set -> 'a t -> 'a t
  val list_project: key_t list -> 'a t -> 'a list
  val iter: (key_t -> 'a -> unit) -> 'a t -> unit
  val iter_with_sep: (key_t -> 'a -> unit) -> (unit -> unit) -> 'a t -> unit
  val filter: (key_t -> 'a -> bool) -> 'a t -> 'a t
  val partition: (key_t -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (key_t -> 'a -> 'b) -> 'a t -> 'b t
  val fold: (key_t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all: ('a -> bool) -> 'a t -> bool
  val for_alli: (key_t -> 'a -> bool) -> 'a t -> bool
  val dump: (key_t list -> key_t list) -> (key_t -> string) -> ('a -> unit) -> ('a -> bool) -> 'a t -> unit
end

module type S =  
  sig
    type key
    module KeySet: Set.S
    module Map : SMap with type key_t = key and type key_set = KeySet.t
  end

(* ---------------------------------------------------------------------- *)

module Make(Ord:Set.OrderedType) = struct

  module M = Map.Make(Ord)
  module KeySet = Set.Make(Ord)

  type key = Ord.t

  module Map = struct
    type 'a t = 'a M.t * KeySet.t
    type key_t = key
    type key_set = KeySet.t

    let empty = (M.empty, KeySet.empty)
    let domain (_, d) = d
    let is_empty (_, d) = KeySet.is_empty d
    let size (_,d) = (KeySet.cardinal d)
    let add x v (m, d) = (M.add x v m, KeySet.add x d)
    let combine (m1, d1) (m2, d2) =
      (M.fold M.add m2 m1, KeySet.union d1 d2)
    let find x (m, _) = M.find x m
    let safe_find x (m, _) d = 
      try M.find x m with Not_found -> d
    let from_function d f =
      let m = KeySet.fold (fun x m -> M.add x (f x) m) d M.empty in
        (m, d)
    let remove x (m, d) = (M.remove x m, KeySet.remove x d)
    let mem x (m, _) = M.mem x m
    let from_list l = 
      snd (Safelist.fold_left 
             (fun (pos,acc) (k,v) -> 
                if mem k acc then 
                  raise 
                    (Invalid_argument 
                       (Printf.sprintf "Mapplus.from_list: repeated key at %d" pos))
                else (pos+1,add k v acc))
             (0,empty) l)
    let iter f (m, _) = M.iter f m
    let iter_with_sep f sep (m, _) =
      let first = ref true in
        M.iter
          (fun k x ->
             if not !first then sep();
             first := false;
             f k x)
          m
    let map f (m, d) = M.map f m, d
    let mapi f (m, d) = M.mapi f m, d
    let fold f (m, _) = M.fold f m
    let filter f m = fold (fun k v m -> if f k v then add k v m else m) m empty
    let partition f m = fold (fun k v (ym,nm) -> 
                                if f k v then (add k v ym,nm) 
                                else (ym,add k v nm)) m (empty,empty)  
    let project ks = filter (fun k v -> KeySet.mem k ks)
    let list_project kls m = List.map (fun k -> find k m) kls
    let for_alli f s =
      (* This implementation is not as efficient as it could be: we'd prefer
         to short-circuit if we ever find a failing element.  But OCaml's
         Map module doesn't provide a primitive that allows this.  We should
         perhaps re-implement singleton maps by *copying* the OCaml
         implementation rather than using it abstractly. *)
      fold (fun k v b -> b && (f k v)) s true
    let for_all f s = for_alli (fun k v -> f v) s
      (* ### dump is probably broken, need to fix it *)
    let dump sortf name_formatter f iep u = 
      let prch (n,ch) = 
        let prf() = Util.format "@["; f ch; Util.format "@]" in
        let s = name_formatter n in
          (*       if (s = "" || s = "\"\"") then *)
          (*        prf()*)
          (*       else*)
          if iep ch then
            Util.format "%s" s
          else begin
            Util.format "@[<hv1>%s =@ " s;
            prf();
            Util.format "@]"
          end
      in
        Util.format "{@[<hv0>";
        let binds = Safelist.map (fun k -> (k, find k u))
          (sortf (KeySet.elements (domain u))) in
        Misc.iter_with_sep
          prch
          (fun()-> Util.format ",@;<1 0>")
          binds;
        Util.format "@]}"

  end (* module Map *)
end (* module Make *)

