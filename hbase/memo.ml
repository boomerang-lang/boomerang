(***************************************************)
(* The Harmony Project                             *)
(* harmony@lists.seas.upenn.edu                    *)
(*                                                 *)
(* memo.ml - memoizing infrastructure              *)
(***************************************************)
(* $Id$ *)

let memo_off = Prefs.createBool "memo-off" false
  "no memoization"
  "no memoization"

let memo_skip = Prefs.createStringList "memo-skip" 
  "skip memoization"
  "skip memoization"

let memo_both = Prefs.createStringList "memo-both" 
  "simulate both equality functions"
  "simulate both equality functions"

let memo_alt = Prefs.createStringList "memo-alt" 
  "use alternate equality function"
  "use alternate equality function"

let memo_csv = Prefs.createBool "memo-csv" false
  "dump memoization stats in CSV"
  "dump memoization stats as CSV"

type stats_t = 
    { get_length : unit -> int;
      get_hits : unit -> int;
      get_misses : unit -> int;
      get_resizes : unit -> int;
      get_bucket_sizes : unit -> int list;
    }

(* -------- instrumented Hashttables, modified from OCaml std-lib (under LGPL) ---------- *)
module Hashtblplus = 
struct

(* Hash tables *)

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

let hash x = hash_param 10 100 x

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  { mutable size: int;                        (* number of elements *)
    mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
    mutable hits:int;
    mutable misses:int;
    mutable resizes:int 
  }

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist

let create initial_size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { size = 0; data = Array.make s Empty; hits=0; misses=0; resizes=0 }

let clear h =
  for i = 0 to Array.length h.data - 1 do
    h.data.(i) <- Empty;
    h.hits <- 0;
    h.misses <- 0;
    h.resizes <- 0;
  done;
  h.size <- 0

let copy h =
  { size = h.size;
    data = Array.copy h.data;
    hits = h.hits;
    misses = h.misses;
    resizes = h.resizes
  }

let hits h = h.hits
let misses h = h.misses
let resizes h = h.resizes

let length h = h.size

let resize hashfun tbl =
  tbl.resizes <- tbl.resizes + 1;
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = min (2 * osize + 1) Sys.max_array_length in
  if nsize <> osize then begin
    let ndata = Array.create nsize Empty in
    let rec insert_bucket = function
        Empty -> ()
      | Cons(key, data, rest) ->
          insert_bucket rest; (* preserve original order of elements *)
          let nidx = (hashfun key) mod nsize in
          ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    tbl.data <- ndata;
  end

let add h key info =
  let i = (hash key) mod (Array.length h.data) in
  let bucket = Cons(key, info, h.data.(i)) in
  h.data.(i) <- bucket;
  h.size <- succ h.size;
  if h.size > Array.length h.data lsl 1 then resize hash h

let remove h key =
  let rec remove_bucket = function
      Empty ->
        Empty
    | Cons(k, i, next) ->
        if compare k key = 0
        then begin h.size <- pred h.size; next end
        else Cons(k, i, remove_bucket next) in
  let i = (hash key) mod (Array.length h.data) in
  h.data.(i) <- remove_bucket h.data.(i)

let rec find_rec key hit miss = function
    Empty ->      
      (miss (); raise Not_found)
  | Cons(k, d, rest) ->
      if compare key k = 0 then (hit (); d) else find_rec key hit miss rest

let find h key =
  let hit () = h.hits <- h.hits + 1 in 
  let miss () = h.misses <- h.misses + 1 in
  match h.data.((hash key) mod (Array.length h.data)) with
    Empty -> 
      (miss (); raise Not_found)
  | Cons(k1, d1, rest1) ->
      if compare key k1 = 0 then (hit (); d1) else
      match rest1 with
        Empty -> 
          (miss (); raise Not_found)
      | Cons(k2, d2, rest2) ->
          if compare key k2 = 0 then (hit (); d2) else
          match rest2 with
            Empty -> (miss (); raise Not_found)
          | Cons(k3, d3, rest3) ->
              if compare key k3 = 0 then (hit (); d3) else find_rec key hit miss rest3

let find_all h key =
  let rec find_in_bucket = function
    Empty ->
      []
  | Cons(k, d, rest) ->
      if compare k key = 0
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket h.data.((hash key) mod (Array.length h.data))

let replace h key info =
  let rec replace_bucket = function
      Empty ->
        raise Not_found
    | Cons(k, i, next) ->
        if compare k key = 0
        then Cons(k, info, next)
        else Cons(k, i, replace_bucket next) in
  let i = (hash key) mod (Array.length h.data) in
  let l = h.data.(i) in
  try
    h.data.(i) <- replace_bucket l
  with Not_found ->
    h.data.(i) <- Cons(key, info, l);
    h.size <- succ h.size;
    if h.size > Array.length h.data lsl 1 then resize hash h

let mem h key =
  let rec mem_in_bucket = function
  | Empty ->
      false
  | Cons(k, d, rest) ->
      compare k key = 0 || mem_in_bucket rest in
  mem_in_bucket h.data.((hash key) mod (Array.length h.data))

let iter f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(k, d, rest) ->
        f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let iteri f h =
  let rec do_bucket i = function
      Empty ->
        ()
    | Cons(k, d, rest) ->
        f i k d; do_bucket i rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket i d.(i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons(k, d, rest) ->
        do_bucket rest (f k d accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu

(* Functorial interface *)
module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type S =
  sig
    type key
    type 'a t
    val create: int -> 'a t
    val clear: 'a t -> unit
    val copy: 'a t -> 'a t
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val find: 'a t -> key -> 'a
    val find_all: 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit      
    val iteri: (int -> key -> 'a -> unit) -> 'a t -> unit      
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length: 'a t -> int
    val stats: 'a t -> stats_t
  end

module Make(H: HashedType): (S with type key = H.t) =
  struct
    type key = H.t
    type 'a hashtbl = (key, 'a) t
    type 'a t = 'a hashtbl
    let create = create
    let clear = clear
    let copy = copy

    let safehash key = (H.hash key) land max_int

    let add h key info =
      let i = (safehash key) mod (Array.length h.data) in
      let bucket = Cons(key, info, h.data.(i)) in
      h.data.(i) <- bucket;
      h.size <- succ h.size;
      if h.size > Array.length h.data lsl 1 then resize safehash h

    let remove h key =
      let rec remove_bucket = function
          Empty ->
            Empty
        | Cons(k, i, next) ->
            if H.equal k key
            then begin h.size <- pred h.size; next end
            else Cons(k, i, remove_bucket next) in
      let i = (safehash key) mod (Array.length h.data) in
      h.data.(i) <- remove_bucket h.data.(i)

    let rec find_rec key = function
        Empty ->
          raise Not_found
      | Cons(k, d, rest) ->
          if H.equal key k then d else find_rec key rest

    let rec find_rec key hit miss = function
        Empty ->      
          (miss (); raise Not_found)
      | Cons(k, d, rest) ->
          if H.equal key k then (hit (); d) else find_rec key hit miss rest

let find h key =
  let hit () = h.hits <- h.hits + 1 in 
  let miss () = h.misses <- h.misses + 1 in
    match h.data.((safehash key) mod (Array.length h.data)) with
    Empty -> 
      (miss (); raise Not_found)
  | Cons(k1, d1, rest1) ->
      if H.equal key k1 then (hit (); d1) else
      match rest1 with
        Empty -> 
          (miss (); raise Not_found)
      | Cons(k2, d2, rest2) ->
          if H.equal key k2 then (hit (); d2) else
          match rest2 with
            Empty -> (miss (); raise Not_found)
          | Cons(k3, d3, rest3) ->
              if H.equal key k3 then (hit (); d3) else find_rec key hit miss rest3

    let find_all h key =
      let rec find_in_bucket = function
        Empty ->
          []
      | Cons(k, d, rest) ->
          if H.equal k key
          then d :: find_in_bucket rest
          else find_in_bucket rest in
      find_in_bucket h.data.((safehash key) mod (Array.length h.data))

    let replace h key info =
      let rec replace_bucket = function
          Empty ->
            raise Not_found
        | Cons(k, i, next) ->
            if H.equal k key
            then Cons(k, info, next)
            else Cons(k, i, replace_bucket next) in
      let i = (safehash key) mod (Array.length h.data) in
      let l = h.data.(i) in
      try
        h.data.(i) <- replace_bucket l
      with Not_found ->
        h.data.(i) <- Cons(key, info, l);
        h.size <- succ h.size;
        if h.size > Array.length h.data lsl 1 then resize safehash h

    let mem h key =
      let rec mem_in_bucket = function
      | Empty ->
          false
      | Cons(k, d, rest) ->
          H.equal k key || mem_in_bucket rest in
      mem_in_bucket h.data.((safehash key) mod (Array.length h.data))

    let iter = iter
    let iteri = iteri
    let fold = fold
    let length = length

    let rec bucketlist_length bl = 
      let rec loop acc = function
        | Empty -> acc
        | Cons(_,_,t) -> loop (acc+1) t in 
        loop 0 bl
            
    let bucket_sizes h = Array.fold_left 
      (fun a d -> (bucketlist_length d)::a)
      [] h.data

    let stats h = 
      { get_length = (fun () -> length h);
        get_hits = (fun () -> hits h);
        get_misses = (fun () -> misses h);
        get_resizes = (fun () -> resizes h);
        get_bucket_sizes = (fun () -> bucket_sizes h)
      }
  end
end

(* ------------------- Memo tables  ------------------- *)

(* global table of all statistics *)
type s = { std:stats_t list; alt:stats_t list }
    
let all_stats = ref Name.Map.empty

let register name stats = 
  let (stds,alts) = Name.Map.safe_find name !all_stats ([],[]) in
    all_stats := Name.Map.add name (stats::stds,alts) !all_stats

let register2 name stats = 
  let (stds,alts) = Name.Map.safe_find name !all_stats ([],[]) in
    all_stats := Name.Map.add name (stds,stats::alts) !all_stats

let gen_memoize name format_arg format_res find add get_table alto = 
  match alto with
      None -> 
	(fun f arg -> 
	   let t = get_table () in 
	     try find t arg 
	     with Not_found -> 	  
	       let r = f arg in
		 add t arg r; r)
	
    | Some(f2,a2,gt2) -> 
	(fun f arg -> 
	   if Safelist.mem name (Prefs.read memo_both) then 
	     begin 
	       let t2 = gt2 () in
		 try ignore (f2 t2 arg) 
		 with Not_found -> a2 t2 arg (f arg)
	     end;
	   let t = get_table () in 
	     try find t arg 
	     with Not_found -> 	  
	       let r = f arg in
		 add t arg r; r)
	  
module type MemoType = sig
  type arg
  type res
  val name : string
  val init_size : int
  val format_arg : arg -> unit
  val format_res : res -> unit
  val hash : arg -> int
  val equal : arg -> arg -> bool
end

module type MemoType2 = sig
  include MemoType
  val hash' : arg -> int
  val equal' : arg -> arg -> bool
end

module type MemoFun = sig
  include MemoType 
  val f : arg -> res
end  

module type MemoFun2 = sig
  include MemoType2
  val f : arg -> res
end
  
module MakeBase(M:MemoType) = struct
  module H = Hashtblplus.Make(struct
    type t = M.arg
    let hash = M.hash
    let equal = M.equal
  end)
end

module MakeBase2(M:MemoType2) = struct
  let alt_hash = Safelist.mem M.name (Prefs.read memo_alt) 
  module H = Hashtblplus.Make(struct
    type t = M.arg
    let hash = if alt_hash then M.hash' else M.hash
    let equal = if alt_hash then M.equal' else M.equal
  end)

  module H2 = Hashtblplus.Make(struct
    type t = M.arg
    let hash = if alt_hash then M.hash else M.hash'
    let equal = if alt_hash then M.equal else M.equal'
  end)
  
end

module Make(M:MemoFun) = struct
  include MakeBase(M)

  let table_opt = ref None
  let get_table () = match !table_opt with 
    | Some t -> t
    | None ->
        let t = H.create M.init_size in 
          table_opt := Some t;
          register M.name (H.stats t);
          t

  let find a = match !table_opt with 
      None -> None 
    | Some t -> (try Some (H.find t a) with Not_found -> None)
    
  let memoized = 
    let mzd = gen_memoize M.name M.format_arg M.format_res H.find H.add get_table None M.f in
      (fun arg -> 
	 if (Prefs.read memo_off || Safelist.mem M.name (Prefs.read memo_skip)) then M.f arg
	 else mzd arg)
end

module Make2(M:MemoFun2) = struct
  include MakeBase2(M)

  let table_opt = ref None
  let get_table () = match !table_opt with 
    | Some t -> t
    | None ->
        let t = H.create M.init_size in 
          table_opt := Some t;
          register M.name (H.stats t);
          t

  let table2_opt = ref None
  let get_table2 () = match !table2_opt with 
    | Some t2 -> t2
    | None ->
        let t2 = H2.create M.init_size in 
          table2_opt := Some t2;
          register2 M.name (H2.stats t2);
          t2

  let find a = match !table_opt with 
      None -> None 
    | Some t -> (try Some (H.find t a) with Not_found -> None)

  let memoized =       
    let mzd = 
      gen_memoize M.name M.format_arg M.format_res 
	H.find H.add get_table 
	(Some (H2.find, H2.add, get_table2)) 
	M.f in
      (fun arg -> 
	 if (Prefs.read memo_off || Safelist.mem M.name (Prefs.read memo_skip)) then M.f arg
	 else mzd arg)
end

module MakeLater(M:MemoType) = struct
  include MakeBase(M)

  let table_opt = ref None
  let get_table () = match !table_opt with 
    | Some t -> t
    | None ->
        let t = H.create M.init_size in 
          table_opt := Some t;
          register M.name (H.stats t);
          t

  let find a = match !table_opt with 
      None -> None 
    | Some t -> (try Some (H.find t a) with Not_found -> None)

  let memoize f =
    if (Prefs.read memo_off || Safelist.mem M.name (Prefs.read memo_skip)) then f 
    else gen_memoize M.name M.format_arg M.format_res 
      H.find H.add get_table None f
end
  
module MakeLater2(M:MemoType2) = struct
  include MakeBase2(M)
    
  let table_opt = ref None
  let get_table () = match !table_opt with 
    | Some t -> t
    | None ->
        let t = H.create M.init_size in 
          table_opt := Some t;
          register M.name (H.stats t);
          t

  let table2_opt = ref None
  let get_table2 () = match !table2_opt with 
    | Some t2 -> t2
    | None ->
        let t2 = H2.create M.init_size in 
          table2_opt := Some t2;
          register2 M.name (H2.stats t2);
          t2

  let find a = match !table_opt with 
      None -> None 
    | Some t -> (try Some (H.find t a) with Not_found -> None)
	      
  let memoize f = 
      if (Prefs.read memo_off || Safelist.mem M.name (Prefs.read memo_skip)) then f 
      else 
	gen_memoize M.name M.format_arg M.format_res 
	  H.find H.add get_table 
	  (Some (H2.find, H2.add, get_table2)) 
	  f
end

(* ---- printing ----- *)
let format_stats () = 
  Trace.debug "memo+"
    (fun () ->
      let csv = Prefs.read memo_csv in 
      let headers = [ "NAME"; "HITS"; "MISSES"; "NUMBER"; "RATE"; "AVG LEN"; "AVG BUCKET LEN"; "AVG RESIZES"] in 
      let columns = Safelist.length headers in 
      let widths = Array.make columns 0 in    
      let rows_rev = Name.Map.fold  
        (fun n (stds,alts) acc -> 
          let sum_stats = Safelist.fold_left (fun (h,m,(i,l,r),(j,tbs)) s ->
            let sh = s.get_hits () in 
            let sm = s.get_misses () in 
            let sl = s.get_length () in 
            let sr = s.get_resizes () in 
            let i',l',r' = 
              if sh = 0 && sm = 0 then i,l,r 
              else i+1,l+sl,r+sr in
            let stbs,stnz = Safelist.fold_left 
              (fun a i -> if i=0 then a else let (tbs,tnz) = a in (tbs+i,tnz+1)) 
              (0,0) (s.get_bucket_sizes ()) in
            let j',tbs' = 
              if stnz = 0 then (j,tbs)
              else j+stnz,tbs+stbs in
              (h+sh,m+sm,(i',l',r'),(j',tbs')))
            (0,0,(0,0,0),(0,0)) in
          let len = Safelist.length stds in 
          let row_of_stats n (h,m,(i,l,r),(j,tbs)) = 
            let d = Printf.sprintf "%d" in 
            let f = Printf.sprintf "%.1f" in
              [n; d h; d m; d len; f (Misc.percent h (h+m)); f (Misc.divide l i); f (Misc.divide tbs j); f (Misc.divide r i)] in
            if alts = [] then row_of_stats n (sum_stats stds)::acc
            else row_of_stats "" (sum_stats alts)
              :: row_of_stats n (sum_stats stds)::acc)
        !all_stats [] in 
      let add_width = Safelist.map (fun n -> (n,String.length n)) in 
      let rows_widths = Safelist.fold_left (fun a r -> add_width r::a) [] rows_rev in 
      let headers_widths = add_width headers in 
        Safelist.iter (fun r -> Safelist.iteri (fun i (_,n) -> widths.(i) <- max widths.(i) n) r) (headers_widths::rows_widths);
        let total_width = Array.fold_left (+) 0 widths + 3 * columns in 
        let format_spacer () = 
          if not csv then 
            begin
              Util.format "@\n";
              Util.format "+";
              for i=1 to total_width-1 do Util.format "-" done;
              Util.format "+"  
            end in
        let format_top () = 
          if not csv then 
            begin 
              let s = "MEMOIZATION STATISTICS " in 
              let l = (total_width + 1 - (String.length s)) / 2 in 
              let r = total_width + 1 - l in 
                Util.format "@\n";
                for i=1 to l do Util.format " " done;
                Util.format "%s" s;
                for i=1 to r do Util.format " " done
            end in

        let format_row r = 
          Util.format "@\n";
          Safelist.iteri (fun i (s,n) ->             
            if not csv then Util.format "| "; 
            if csv && i <> 0 then Util.format ",";
            Util.format "%s" s;
            if not csv then for j=1 to (widths.(i) - n + 1) do Util.format " " done)
            r;
          if not csv then Util.format "|" in
            
          (* main printing loop *)
          format_top (); 
          format_spacer (); 
          format_row headers_widths; 
          format_spacer ();
          Safelist.iter format_row rows_widths;          
          format_spacer ();
          Util.format "@\n")
