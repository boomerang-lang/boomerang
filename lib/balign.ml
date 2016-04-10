(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2007 J. Nathan Foster and Benjamin C. Pierce                 *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or              *)
(* modify it under the terms of the GNU Lesser General Public                 *)
(* License as published by the Free Software Foundation; either               *)
(* version 2.1 of the License, or (at your option) any later version.         *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *)
(* Lesser General Public License for more details.                            *)
(******************************************************************************)
(* src/balign.ml                                                              *)
(* Alignment and Permutation                                                  *)
(* $Id: balign.ml 4901 2010-05-13 21:14:49Z cretin $ *)
(******************************************************************************)

module Arx = Barx
module C = Bcost
module T = Btag
module TmAl = T.MapAList
module KmAl = Amapblist.Make (String)
module TmImA = T.MapIntMapA
module TmI = T.MapInt
module ImA = Intmapa
module Ts = T.Set

type pos = int

let printf = Printf.printf

type link =
  | Put of pos * pos
  | Create of pos
  | Delete of pos

let check_put tag del put crt =
  let ps = T.get_predicates tag in
  let rec f ps =
    match ps with
    | [] -> None
    | p::ps ->
        let eo =
          match p with
          | T.Threshold t ->
              (* if crt + del - put < t * min del crt / 50 *)
              if crt + del - put < t * (del + crt) / 100
              then Some (fun () -> print_endline "Threshold predicate not satisfied.")
              else None
        in
        match eo with
        | None -> f ps
        | Some e -> Some e
  in
  f ps

module Alignment = struct
  type t =
    | Finite of link TmAl.t * int
    | Infinite of (unit -> unit)
  let empty = Finite (TmAl.empty, 0)
  let to_cost t =
    match t with
    | Finite (_, c) -> C.of_int c
    | Infinite _ -> C.infinite
  let to_error_option t =
    match t with
      | Infinite e -> Some e
      | Finite _  -> None
  let print_space t spc =
    print_string (spc ^ "alignment: ");
    match t with
    | Finite (g, c) ->
        print_endline (string_of_int c);
        TmAl.print_list (fun t -> print_endline (spc ^ " " ^ T.to_string t ^ ":")) (
          fun l ->
            Safelist.iter (
              fun v -> match v with
              | Put (i, j) -> printf "%s  %3i - %-3i\n" spc i j
              | Delete j    -> printf "%s      d %-3i\n" spc j
              | Create i    -> printf "%s  %3i c\n" spc i
            ) l
        ) g
    | Infinite e ->
        print_endline "error";
        e ()
  let print t = print_space t ""
  let merge a b =
    match a, b with
    | Infinite ea, Infinite eb -> Infinite (fun () -> ea (); eb ())
    | Infinite e, Finite _
    | Finite _, Infinite e -> Infinite e
    | Finite (ga, ca), Finite (gb, cb) ->
        Finite (
          TmAl.fold_list (
            fun tag l g ->
              TmAl.add_list tag (
                Safelist.rev_append l (TmAl.find_list tag ga)
              ) g
          ) gb ga,
          ca + cb
        )
  let get_new t tag j = (* for one tag, gives i such that Put(i, j) is in g *)
    let rec search l =
      match l with
      | [] -> None
      | Put (i, k)::_ when j = k -> Some i
      | Delete k::_ when j = k -> None
      | _::lt -> search lt
    in
    match t with
    | Infinite _ -> assert false
    | Finite (g, _) -> search (TmAl.find_list tag g)
  let nocost_put tag i j t =
    match t with
    | Infinite e -> Infinite e
    | Finite (g, c) -> Finite (TmAl.add tag (Put (i, j)) g, c)
  let add_put limit tag cn co rn ro t =
    let ((ccrt, _), sn), i = cn in
    let ((cdel, _), so), j = co in
    if T.key_through tag
    then nocost_put tag i j t
    else begin
      let kn = Bstring.cat_to_key sn in
      let ko = Bstring.cat_to_key so in
      (*     Printf.printf "add_put %s %d %d '%s' '%s'\n" (T.to_string tag) i j kn ko; *)
      let cput = Bstring.dist (C.to_option limit) kn ko in
      match cput with
      | None -> Infinite (fun () -> assert false)
      | Some cput ->
          let pe = check_put tag cdel cput ccrt in
          match t, pe with
          | Infinite e1, Some e2 -> Infinite (fun () -> e1 (); e2 ())
          | Infinite e, _
          | _, Some e -> Infinite e
          | Finite (g, c), None -> Finite (TmAl.add tag (Put (i, j)) g, c + cput)
    end
  let add_crtdel_deep tag ((ccd, cat), x) t create =
    let cost =
      if T.key_through tag
      then (fun (a, b) -> b - a)
      else snd
    in
    let con, crtdel_string =
      if create
      then (fun x -> Create x), "create"
      else (fun x -> Delete x), "delete"
    in
    match t with
    | Infinite e -> Infinite e
    | Finite (g, c) ->
        Finite (
          Bstring.cat_fold_on_locs (
            fun t p -> TmAl.add t (con p)
          ) cat (TmAl.add tag (con x) g),
          c + cost ccd
        )
  (* TODO: add potential predicates *)
  let add_crt_deep tag cn t =
    add_crtdel_deep tag cn t true
  let add_del_deep tag co t =
    add_crtdel_deep tag co t false
end

module G = Alignment

module Permutation :
sig
  type t
  val print : t -> unit
  val empty : t
  val add : Btag.t -> (int * int) -> t -> t
  val apply : Btag.t -> int -> t -> int
  val inv : t -> t (* fast! *)

(* [shift p s_shift v_shift acc]: *)
(* shift permutation p with s_shift for the left *)
(* side and v_shift for the right side and put the *)
(* result into acc *)
  val shift : t -> TmI.t -> TmI.t -> t -> t

(* [compose p p1 p2] called with: *)
(* . [dom p1] inter [dom p] = empty *)
(* . [dom p1] = [dom p2] *)
(* returns [d] with: *)
(* . [d] restricted to [dom p]  = [p] *)
(* . [d] restricted to [dom p1] = [a . b] *)
(*                             ie. [\x. a (b x)] *)
(* ie: compose p1 with p2 and put the result into p *)
  val compose : t -> t -> t -> t
end = struct
  type t = bool * (pos * pos) TmAl.t (* true = normal (i->j), false = inverted (i<-j) *)

  (* use carefully these functions *)
  let pair d (i,j) =
    if d then (i,j)
    else (j,i)
  let left d (i,j) =
    if d then i
    else j
  let right d (i,j) =
    if d then j
    else i

  let empty = (true, TmAl.empty)

  let print (d,p) =
    let prt (i,j) =
      if d then printf " %2d -> %2d\n" i j
      else      printf " %2d -> %2d\n" j i
    in
    print_endline "permutation:";
    TmAl.print_list
      (fun t -> print_endline (T.to_string t))
      (fun l -> Safelist.iter prt l)
      p

  (* check if the domain and the codomain are the same (debug) *)
  let check (d,p) =
    TmAl.iter_list (
      fun _ l ->
        let le, lr = Safelist.split l in
        let le = List.fast_sort compare le in
        let lr = List.fast_sort compare lr in
        List.fold_left2 (
          fun _ ie ir ->
            if (ie != ir) then print(d,p);
            assert (ie = ir)
        ) () le lr
    ) p;
    (d,p)

  let rec lsearch f n g l =
    match l with
      | [] -> raise Not_found
      | h::lt when f h = n -> g h
      | h::lt -> lsearch f n g lt

  let add tag ij (d,p) =
    (d, TmAl.add tag (pair d ij) p)
  let apply tag n (d,p) =
    lsearch (left d) n (right d) (TmAl.find_list tag p)
  let shift (d1,p1) s_shift v_shift (d,p) =
    let p =
      TmAl.fold_list (
        fun tag l1 p ->
          let s = TmI.find tag s_shift in
          let v = TmI.find tag v_shift in
          let l = TmAl.find_list tag p in
          TmAl.add_list
            tag
            (Safelist.fold_left (fun l ij -> pair d (s + left d1 ij, v + right d1 ij)::l) l l1)
            p
      ) p1 p
    in
    (d,p)
  let inv (d,p) = (not d, p)
  let compose (d,p) (d1,p1) (d2,p2) =
    let rec compose_list (d,l) (d1,l1) (d2,l2) =
      (* compose l1 and l2 putting into l *)
      match l1 with
        | [] -> l
        | ij::l1t ->
            let i,j = pair d1 ij in
            let j' = lsearch (left d2) j (right d2) l2 in
            compose_list (d, pair d (i, j')::l) (d1, l1t) (d2, l2)
    in
    let p =
      TmAl.fold_list (
        fun tag l1 p ->
          let l  = TmAl.find_list tag p  in
          let l2 = TmAl.find_list tag p2 in
          TmAl.add_list tag (compose_list (d,l) (d1,l1) (d2,l2)) p
      ) p1 p
    in
    (d,p)
end

module P = Permutation

type 'a resource = 'a TmImA.t

(* auxiliary functions *)

let print_res p r =
  print_endline "resource:";
  TmImA.print (fun t -> print_endline (T.to_string t)) (
    fun i v ->
      print_string " ";
      print_int i;
      print_string " -> ";
      p v;
      print_endline ""
  ) r

(* let perm_apply p tag n = *)
(*   let rec apply pl = *)
(*     match pl with *)
(*       | [] -> raise Not_found *)
(*       | (i,j)::plt when i = n -> j *)
(*       | h::plt -> apply plt *)
(*   in apply (TmAl.find_list tag p) *)

(* let inv_perm p = *)
(*   TmAl.map (fun (i, j) -> (j, i)) p *)

(* let perm_apply_inv p tag n : int = *)
(*   let rec apply_inv pl =  *)
(*     match pl with *)
(*       | [] -> raise Not_found *)
(*       | (i,j)::plt when j = n -> i *)
(*       | h::plt -> apply_inv plt *)
(*   in apply_inv (TmAl.find_list tag p) *)

(* let restrict_to_chunk (cat:Bstring.cat) (tl:Bstring.cat TmImA.t) : Bstring.cat TmImA.t = *)
(*   let start, len = Bstring.at_to_locs s in *)
(*     TmAl.fold_list *)
(*       (fun tag l acc -> *)
(*          let s = TmI.find tag start in *)
(*          let e = s + (TmI.find tag len) in *)
(*            TmAl.add_list tag *)
(*              (Safelist.filter (fun (at,i) -> i >= s && i < e) l) *)
(*              acc) *)
(*       tl TmAl.empty *)

(* alignment *)

let nest_align limit align tag cn co tln tlo =
  let (_, sn), i = cn in
  let (_, so), j = co in
  let g = align limit (sn, tln) (so, tlo) G.empty in
  let limit = C.limit_minus limit (G.to_cost g) in
  G.add_put limit tag cn co tln tlo g

let crt_align tag cn =
  G.add_crt_deep tag cn G.empty

let del_align tag co =
  G.add_del_deep tag co G.empty

(* matrix functions *)

let matrix_iter f m =
  Array.iteri (
    fun i row ->
      Array.iteri (
        fun j cell ->
          f i j cell
      ) row;
  ) m

let matrix_map f m = (* change matrix values *)
  Array.iteri (
    fun i row ->
      Array.iteri (
        fun j cell ->
          m.(i).(j) <- f i j cell
      ) row;
  ) m

let matrix_foldi f acc m =
  snd
    (Array.fold_left (
       fun (i,acc) row ->
         let racc =
           Array.fold_left (
             fun (j,acc) cell ->
               (succ j, f acc i j cell)
           ) (0,acc) row
         in (succ i, snd racc)
     ) (0,acc) m)

let matrix_find f m =
  matrix_foldi
    (fun acc i j cell ->
       match acc with
         | Some (i,j) -> Some (i,j)
         | None ->
             match f i j cell with
               | true -> Some (i,j)
               | none -> None
    ) None m

let find_in_row i v m =
  snd
    (Array.fold_left (
       fun (j,acc) cell ->
         match acc, v = cell with
           | Some j, _   -> succ j, Some j
           | None, true  -> succ j, Some j
           | None, false -> succ j, None
     ) (0, None) m.(i))

let find_in_col j v m =
  snd
    (Array.fold_left (
       fun (i,acc) row ->
         match acc, v = row.(j) with
           | Some i, _   -> succ i, Some i
           | None, true  -> succ i, Some i
           | None, false -> succ i, None
     ) (0, None) m)

(* alignments *)

let positional limit align tag (ln:int list) (lo:int list) tln tlo g =
  let get_new i = TmImA.find tag i tln, i in
  let get_old j = TmImA.find tag j tlo, j in
  let rec f ln lo g =
    match ln, lo with
    | [], [] -> g
    | i::ln, [] -> f ln [] (G.add_crt_deep tag (get_new i) g)
    | [], j::lo -> f [] lo (G.add_del_deep tag (get_old j) g)
    | i::ln, j::lo ->
        let cn = get_new i in
        let co = get_old j in
        let (_, sn), _ = cn in
        let (_, so), _ = co in
        let g = align limit (sn, tln) (so, tlo) g in
        let limit = C.limit_minus limit (G.to_cost g) in
        f ln lo (G.add_put limit tag cn co tln tlo g)
  in
  f ln lo g

let diffy first limit align tag (ln:int list) (lo:int list) tln tlo g =
  let iffirst =
    if first then
      (function
       | [d;t;l] -> [l;t;d]
       | _ -> assert false
      )
    else (fun x -> x)
  in
  let an = Array.map (fun i -> TmImA.find tag i tln, i) (Array.of_list ln) in
  let ao = Array.map (fun j -> TmImA.find tag j tlo, j) (Array.of_list lo) in
  let anlen = Array.length an in
  let aolen = Array.length ao in
  let extract_cost g = G.to_cost g, g in
  let costput = Array.make_matrix anlen aolen None in
  let costcrt = Array.map (fun cn -> extract_cost (crt_align tag cn)) an in
  let costdel = Array.map (fun co -> extract_cost (del_align tag co)) ao in
  let limit = (* put from the beginining then delete or create *)
    let m = min anlen aolen in
    let rec loop x acc =
      if x >= m
      then (
        let subloop x size cost acc =
          let rec subloop_aux x acc =
            if x >= size then acc
            else subloop_aux (succ x) (C.plus acc (fst cost.(x)))
          in
          subloop_aux x acc
        in
        if x = anlen
        then (  (* do deletes *)
          subloop x aolen costdel acc
        ) else (  (* do creates *)
          subloop x anlen costcrt acc
        )
      ) else (  (* do put *)
        let c, g = extract_cost (nest_align limit align tag an.(x) ao.(x) tln tlo) in
        costput.(x).(x) <- Some (c, g);
        loop (succ x) (C.plus acc c)
      )
    in
    C.min limit (loop 0 C.one)
  in
  let path = Array.make_matrix (succ anlen) (succ aolen) ((C.zero, 0), `Diag) in
  let get_pathcost i j = fst (fst (path.(i).(j))) in
  let get_costput i j =
    let cg =
      match costput.(i).(j) with
      | Some cg -> cg
      | None ->
          let limit =
            let p = fst (fst path.(i).(j)) in
            let k =
              C.min limit
                (C.min
                   (C.plus (get_pathcost (succ i) j) (fst costdel.(j)))
                   (C.plus (get_pathcost i (succ j)) (fst costcrt.(i))))
            in
            C.limit_minus k p
          in
          extract_cost (nest_align limit align tag an.(i) ao.(j) tln tlo)
    in
    costput.(i).(j) <- Some cg;
    cg
  in
  let lessthan (ca, a) (cb, b) = C.lt ca cb || (C.equal ca cb && a < b) in
  let newpath i j d =
    let a, b = fst path.(i).(j) in
    let c, _ =
      match d with
      | `Diag -> get_costput i j
      | `Top -> costcrt.(i)
      | `Left -> costdel.(j)
    in
    (C.plus a c, succ b), d
  in
  let argmin l =
    match l with
    | [] -> assert false
    | h::t ->
        Safelist.fold_left (fun m v -> if lessthan (fst v) (fst m) then v else m) h t
  in
  let rec loop i j =
    if i > anlen then ()
    else (
      if j > aolen then loop (succ i) 1
      else (
        path.(i).(j) <-
          argmin (iffirst [
                    (newpath (i-1) (j-1) `Diag);
                    (newpath (i-1) j `Top);
                    (newpath i (j-1) `Left)
                  ]);
        loop i (succ j)
      )
    )
  in
  for i = 1 to anlen do
    path.(i).(0) <- newpath (i-1) 0 `Top
  done;
  for j = 1 to aolen do
    path.(0).(j) <- newpath 0 (j-1) `Left
  done;
  if anlen <> 0 && aolen <> 0 then loop 1 1;
(*   Printf.printf "cost and path: \n"; *)
(*   let _ = *)
(*     let w = 8 in *)
(*     let h = w / 2 in *)
(*     let line s t f = *)
(*       Printf.printf "%*s" w s; *)
(*       Array.iter (fun x -> Printf.printf "%*s" w (f x)) t; *)
(*       Printf.printf "\n" *)
(*     in *)
(*     for i = 0 to anlen do *)
(*       if i = 0 *)
(*       then line "" costdel (fun (x, _) -> C.to_string x) *)
(*       else line (C.to_string (fst costcrt.(pred i))) costput.(pred i) *)
(*         (fun x -> *)
(*            match x with *)
(*            | None -> "XXX" *)
(*            | Some (c, _) -> C.to_string c); *)
(*       Printf.printf "%*s" h ""; *)
(*       for j = 0 to aolen do *)
(*         Printf.printf "%*s" w (C.to_string (fst (fst path.(i).(j)))) *)
(*       done; *)
(*       Printf.printf "\n"; *)
(*     done; *)
(*   in *)
  let rec find_path i j g =
    if i = 0 && j = 0 then g
    else (
      let i, j, cg =
        match snd path.(i).(j) with
        | `Diag -> (pred i), (pred j), get_costput (pred i) (pred j)
        | `Top  -> (pred i), j, costcrt.(pred i)
        | `Left -> i, (pred j), costdel.(pred j)
      in
      find_path i j (G.merge g (snd cg))
    )
  in
  find_path anlen aolen g

let greedy limit align tag (ln:int list) (lo:int list) tln tlo g =
  let an = Array.map (fun i -> TmImA.find tag i tln, i) (Array.of_list ln) in
  let ao = Array.map (fun j -> TmImA.find tag j tlo, j) (Array.of_list lo) in
  let anlen = Array.length an in
  let aolen = Array.length ao in
  let freen = Array.make anlen true in
  let freeo = Array.make aolen true in
  let merge g i j sg =
    freen.(i) <- false;
    freeo.(j) <- false;
    G.merge g sg
  in
  let rec calc_costs g (i:int) j edges = (* (cost, i, j, and subalignment between i and j) list *)
    if i >= anlen then (g, edges)
    else if not freeo.(j) then (* j is already aligned *)
      calc_costs g (i + (succ j)/aolen) ((succ j) mod aolen) edges
    else (
      let sg = nest_align C.infinite align tag an.(i) ao.(j) tln tlo in
      let cost = G.to_cost sg in
      match C.to_option cost with
      | Some 0 -> (* add the link to alignment because there is no cost *)
          let g = merge g i j sg in
          calc_costs g (succ i) 0 edges
      | None ->
          calc_costs g (i + (succ j)/aolen) ((succ j) mod aolen) edges
      | _ ->
	  calc_costs g (i + (succ j)/aolen) ((succ j) mod aolen) ((cost, i, j, sg) :: edges)
    )
  in
  let g, edges = if aolen > 0 then calc_costs g 0 0 [] else (g, []) in
  let edges = Safelist.rev edges in
  (* (\* debug *\) *)
  (*    let str tl i = Bstring.to_string (Bstring.of_cat (snd (TmImA.find tag i tl))) in *)
  (*   (printf "Greedy list with %d edges for alignment with %d and %d chunks\n" (Safelist.length edges) anlen aolen; *)
  (*    Safelist.iter (fun (c,i,j,sg) -> *)
  (*                     printf " (%2d,%2d) = (\"%s\",\"%s\") -> cost %s:\n" i j (C.to_string c) (str tln i) (str tlo j); *)
  (*                     G.print_space sg "    ") edges; *)
  (*    print_endline "------"); *)
  let costlnk = Array.of_list edges in
  Array.stable_sort (fun (c1,_,_,_) (c2,_,_,_) -> C.compare c1 c2) costlnk;
  let g =
    Array.fold_left
      (fun g (c,i,j,sg) ->
         if freen.(i) && freeo.(j)
         then merge g i j sg
         else g
      )
      g costlnk
  in
  let f action (g, x) free =
    (if free
     then action x g
     else g
    ),
    succ x
  in
  let g, _ =
    Array.fold_left (f (fun i -> G.add_crt_deep tag an.(i))) (g, 0) freen
  in
  let g, _ =
    Array.fold_left (f (fun j -> G.add_del_deep tag ao.(j))) (g, 0) freeo
  in
  (*       G.print_space g " "; print_endline "\n"; *)
  g


(* This is only to check if the hungarian is giving the right answer *)
(* does not change any matrix or array *)
(* let bruteforce tag cost gput gcrt gdel an ao anlen aolen len tln tlo g = *)
(*   let covern = Array.make len false in (\* rows *\) *)
(*   let rec diag d = *)
(*     if (d < len) then *)
(*       let c, l = diag (succ d) in *)
(*         (C.plus c cost.(d).(d), (d,d)::l) *)
(*     else *)
(*       (C.zero, []) *)
(*   in *)
(*   let rec minrec j l c (min,lmin) : C.t * ((int * int) list) = *)
(*     match j < len, C.lt c min with *)
(*       | true, true -> *)
(*           let min, lmin,_ =  *)
(*             Array.fold_left ( *)
(*               fun (min,lmin,i) cover -> *)
(*                 if not cover then *)
(*                   begin *)
(*                     covern.(i) <- true; *)
(*                     let rmin,rlmin = minrec (succ j) ((i,j)::l) (C.plus c cost.(i).(j)) (min,lmin) in *)
(*                       covern.(i) <- false; *)
(*                       if C.lt rmin min then *)
(*                         (rmin, rlmin, succ i) *)
(*                       else *)
(*                         (min, lmin, succ i) *)
(*                   end *)
(*                 else *)
(*                   (min,lmin,succ i) *)
(*             ) (min,lmin,0) covern *)
(*           in (min, lmin) *)
(*       | false, true -> (c,l) *)
(*       | _,    false -> (min, lmin) *)
(*   in *)
(*   let min,lmin = minrec 0 [] C.zero (diag 0) in *)
(*   let rec mk_alignment g = function *)
(*     | [] -> g *)
(*     | (i,j)::l -> *)
(*         let g = mk_alignment g l in *)
(*           match i < anlen, j < aolen with *)
(*             | true, true -> G.merge g gput.(i).(j) *)
(*             | true,false -> G.merge g gcrt.(i) *)
(*             | false,true -> G.merge g gdel.(j) *)
(*             | false,false-> g *)
(*   in *)
(*   let g = mk_alignment g lmin in *)
(*     (g, G.to_cost g, min, lmin) *)

(* based on:
   http://csclab.murraystate.edu/bob.pilgrim/445/munkres.html
   http://github.com/evansenter/gene/blob/f515fd73cb9d6a22b4d4b146d70b6c2ec6a5125b/objects/extensions/hungarian.rb
 *)
let hungarian limit align tag (ln:int list) (lo:int list) tln tlo g =
  let an = Array.map (fun i -> TmImA.find tag i tln, i) (Array.of_list ln) in
  let ao = Array.map (fun j -> TmImA.find tag j tlo, j) (Array.of_list lo) in
  let anlen = Array.length an in
  let aolen = Array.length ao in
  let len = anlen + aolen in
  let covern = Array.make len false in (* rows *)
  let covero = Array.make len false in (* columns *)
  let gput = Array.make_matrix anlen aolen G.empty in
  let gcrt = Array.map (fun cn -> crt_align tag cn) an in
  let gdel = Array.map (fun co -> del_align tag co) ao in
  let mark = Array.make_matrix len len `Normal in
  let cost = Array.make_matrix len len C.zero  in

  let reset_cover () =
    Array.iteri (fun i _ -> covern.(i) <- false; covero.(i) <- false) covern
  in
  let allcovered () =
    Array.fold_left (fun a c -> a && c) true covero
  in
  let covered i j = covern.(i) || covero.(j) in
  let find_uncovered_zero () =
    matrix_find
      (fun i j c ->
         match covern.(i), covero.(j), C.to_option c with
           | false,false,Some 0 -> true
           | _ -> false
      ) cost
  in
  let find_smallest_uncovered () =
    matrix_foldi
      (fun min i j c ->
         match covered i j, C.to_option c with
           | true, _ -> min
           | false, Some 0 -> assert false (* uncovered zero *)
           | false, _ -> C.min min c
      ) C.infinite cost
  in
  let rec update_path = function
    | [] -> ()
    | (i,j)::path ->
        (match mark.(i).(j) with
           | `Prime  -> mark.(i).(j) <- `Star
           | `Star   -> mark.(i).(j) <- `Normal
           | `Normal -> assert false);
        update_path path
  in
  let reset_primes () =
    matrix_map
      (fun _ _ m ->
         match m with
           | `Prime -> `Normal
           | m -> m
      ) mark
  in
  let read_result (g:G.t) =
    matrix_foldi
      (fun (g:G.t) i j status ->
         match status, i < anlen, j < aolen with
           | `Star,true, true -> G.merge g gput.(i).(j)
           | `Star,true,false -> G.merge g gcrt.(i)
           | `Star,false,true -> G.merge g gdel.(j)
           | _ -> g)
      g mark
  in
  let calc_costs () =
    matrix_iter (
      fun i j _ ->
        let g' = nest_align C.infinite align tag an.(i) ao.(j) tln tlo in
        gput.(i).(j) <- g';
        cost.(i).(j) <- G.to_cost g'
    ) gput;
    Array.iteri (
      fun i sg ->
        let c = G.to_cost sg in
        for j = aolen to len-1 do
          cost.(i).(j) <- c
        done
    ) gcrt;
    Array.iteri (
      fun j sg ->
        let c = G.to_cost sg in
        for i = anlen to len-1 do
          cost.(i).(j) <- c
        done
    ) gdel
  in

  (* debug *)
(*   let debugcheck g_hung = *)
(*     (\* this calls calc_costs again, which call recursively align *\) *)
(*     (\* (so debug printing lines will be printed multiple times for nested alignment) *\) *)
(*     calc_costs (); *)
(*     let g, c, min, _ = bruteforce tag cost gput gcrt gdel an ao anlen aolen len tln tlo g in *)
(* (\*       printf "Debug solution with cost %s (matrix) %s (alignment)\n" (C.to_string min) (C.to_string c); *\) *)
(* (\*       G.print_space g "  "; *\) *)
(*       assert (C.equal (G.to_cost g_hung) c); *)
(* (\*       print_endline "--> Ok!" *\) *)
(*   in *)
  (* let str tl i = Bstring.to_string (Bstring.of_cat (snd (TmImA.find tag i tl))) in *)
  (* let print_array f a = *)
  (*   Array.iter (fun v -> printf " %3s" (f v)) a; *)
  (*   printf "\n" *)
  (* in *)
  (* let print_matrix spc = *)
  (*   matrix_iter ( *)
  (*     fun i j c -> *)
  (*       let m = match mark.(i).(j) with *)
  (*         | `Star   -> "*" *)
  (*         | `Prime  -> "'" *)
  (*         | `Normal -> " " *)
  (*       in *)
  (*         if i = 0 && j = 0 then *)
  (*           (printf "%s  " spc; *)
  (*            Array.iter (fun v -> printf " %3s " (if v then "|" else "")) covero); *)
  (*         if j = 0 then printf "\n%s %s" spc (if covern.(i) then "-" else " "); *)
  (*         printf " %3s%s" (C.to_string c) m *)
  (*   ) cost; *)
  (*   printf "\n" *)
  (* in *)
  (* let initialdebug () = *)
  (*   printf "Hungarian running with:\n"; *)
  (*   printf "New (%2d) chunks: " anlen; print_array (fun (_,i) -> Printf.sprintf "(%2d,%6s)" i (str tln i)) an; *)
  (*   printf "Old (%2d) chunks: " aolen; print_array (fun (_,j) -> "("^string_of_int j^","^str tlo j^")") ao; *)
  (*   printf "len = %2d\n" len *)
  (* in *)
  (* let debug caller = *)
  (*   printf "= Running %s with input:\n" caller; *)
  (*   printf "  Cost matrix:\n"; *)
  (*   print_matrix "  "; *)
  (*   printf "\n" *)
  (* in *)

  (* algorithm.. *)
  let rec run () =
    minimize_rows ()
  and minimize_rows () =
    (* debug "minimize_rows (step 1)"; *)
    Array.iteri (
      fun i row ->
        let min = Array.fold_left (fun min c -> C.min min c) C.infinite row in (* verify: min = infinite *)
        Array.iteri (
          fun j c ->
            cost.(i).(j) <- C.minus c min
        ) row
    ) cost;
    star_zeroes ()
  and star_zeroes () =
    (* cover is reseted when we call start_zeroes *)
    (* debug "star_zeroes (step 2)"; *)
    matrix_iter (
      fun i j c ->
        match C.to_option c, covern.(i) || covero.(j) with
          | Some 0, false ->
              covern.(i) <- true;
              covero.(j) <- true;
              mark.(i).(j) <- `Star
          | _ -> ()
    ) cost;
    reset_cover ();
    cover_columns ()
  and cover_columns () =
    (* debug "cover_columns (step 3)"; *)
    matrix_iter (
      fun _ j status ->
        match status with
          | `Star -> covero.(j) <- true
          | _ -> ())
      mark;
    if allcovered () then
      finished ()
    else
      prime_zeroes ()
  and prime_zeroes () =
    (* debug "prime_zeroes (step 4)"; *)
    match find_uncovered_zero () with
      | Some (i,j) ->
          mark.(i).(j) <- `Prime;
          (match find_in_row i `Star mark with
             | None -> augment_path [(i,j)]
             | Some j ->
                 covern.(i) <- true;
                 covero.(j) <- false;
                 prime_zeroes ())
      | None ->
          adjust_matrix ()
  and augment_path path =
    (* debug "augment_path (step 5)"; *)
    let z0 = Safelist.hd path in
      match find_in_col (snd z0) `Star mark with
        | Some i -> (* z1 = (i, snd z0) *)
            (match find_in_row i `Prime mark with
               | Some j -> augment_path ((i,j)::(i, snd z0)::path)
               | None -> assert false)
        | None ->
            update_path path;
            reset_primes ();
            reset_cover ();
            cover_columns ()
  and adjust_matrix () =
    (* debug "adjust_matrix (step 6)"; *)
    let min = find_smallest_uncovered () in (* verify: min = infinite *)
    matrix_iter
      (fun i j c ->
         match covern.(i), covero.(j) with
           | true, true ->
               cost.(i).(j) <- C.plus c min;
           | false, false ->
               cost.(i).(j) <- C.minus c min;
           | _, _ -> ()
      ) cost;
    prime_zeroes ();
  and finished () = (* debug "finished"; *) ()
  in
    (* initialdebug (); *)
    calc_costs ();
    run ();
    let g = read_result g in
(*       G.print_space g " "; print_endline "\n"; *)
(*       debugcheck g; *)
      g

let align_aux tag limit align =
  (match T.get_species tag with
   | T.Positional -> positional
   | T.Diffy b -> diffy b
   | T.Greedy -> greedy
   | T.Setlike -> hungarian
  ) limit align tag

(* [g] is chained
   [tln] and [tlo] (the chunkmaps) are global to all calls to [align]
*)
let rec align limit ((sn,tln):(Bstring.cat * Bstring.chunkmap)) (so,tlo) (g:G.t) =
  let hn = Bstring.toplevel_chunks sn in
  let ho = Bstring.toplevel_chunks so in
  let domain h = TmAl.fold_list (fun t _ acc -> Ts.add t acc) h Ts.empty in
  let _, g =
    Ts.fold (
      fun tag (limit, g) ->
        let g =
          align_aux tag limit align (TmAl.find_list tag hn) (TmAl.find_list tag ho) tln tlo g
        in
        C.limit_minus limit (G.to_cost g), g
    ) (Ts.union (domain hn) (domain ho)) (limit, g)
  in
  g

(* Compositions / Resources / Permutations *)

(* The following functions (align_compose_res, res_compose_perm, etc) *)
(* use TmImA.foldi and then they repeat some computation with the tag *)
(* using TmImA.mapi_ima or TmImA.fold_ima we can avoid this repeated  *)
(* computations, for example: *)

(* (\* * Balign.align_compose_res : res -> align -> res *\) *)
(* let align_compose_res r g = *)
(* (\* (vo, k) and Put (vn, vo)  gives  (vn, k). *\) *)
(*   let rec get_new gl j = *)
(*     match gl with *)
(*       | [] -> None *)
(*       | Put(i,j')::glt when j = j' -> Some i *)
(*       | h::glt -> get_new glt j *)
(*   in *)
(*   TmImA.mapi_ima *)
(*     (fun tag ima -> *)
(*        let gl = TmAl.find_list tag g in *)
(*          ImA.fold (fun j v acc -> *)
(*                      match get_new gl j with *)
(*                        | Some i -> ImA.add i v acc *)
(*                        | None -> acc) ima ImA.empty) *)
(*     r *)

(* However, to make the code clear for the moment, we do not use this *)

(* * Balign.align_compose_res : res -> align -> res *)
let align_compose_res r g =
(* (vo, k) and Put (vn, vo)  gives  (vn, k). *)
  TmImA.foldi
    (fun tag j v acc ->
       match G.get_new g tag j with
         | Some i -> TmImA.add tag i v acc
         | _ -> acc)
    r TmImA.empty


(* let res_compose_perm_append g r p = *)
(*   TmImA.foldi ( *)
(*     fun tag i v acc -> *)
(*       TmImA.add tag (perm_apply_inv p tag i) v acc *)
(*   ) r g *)

(* * Balign.res_compose_perm : res -> perm -> res *)
let res_compose_perm r p =
(* [Balign.res_compose_perm r p] returns [s] with: *)
(* . [s x] = [r (p x)] *)
(* If p = t -> [(s, v)] *)
(*    r = t -> [(v, a)] *)
(* Then s = t -> [(s, a)] *)
  let p = P.inv p in
  TmImA.foldi (
    fun tag i v acc ->
      let j =
        try P.apply tag i p
        with Not_found -> i
      in
      TmImA.add tag j v acc
    ) r TmImA.empty


(* * Balign.res_zip : (('a * 'a) -> 'a) -> res -> res -> res -> res *)
(* [Balign.res_zip f r a b] called with: *)
(* . [dom a] inter [dom r] = empty *)
(* . [dom a] = [dom b] *)
let res_zip f r ra rb =
(* returns [d] with: *)
(* . [d] restricted to [dom r] = [r] *)
(* . [d] restricted to [dom a] = [\x. f (a x) (b x)] *)
  TmImA.foldi
    (fun tag i v acc ->
       TmImA.add tag i (f (v, (TmImA.find tag i rb))) acc)
    ra r

(* * Balign.res_unzip : ('a -> ('a * 'a)) -> res -> mark -> mark -> (res * res * res) *)
(* A mark is a TmI.t. *)
let res_unzip f r start len =
(* [Balign.res_unzip f d start len] returs [p, a, b] with: *)
(* . [Balign.res_zip f^{-1} p a b] = [d] *)
(* . [dom a] = [dom b] is [start] <= . < [start + len] *)
(* . [dom p] >= [start + len] *)
  TmImA.foldi
    (fun tag i v (a,b,p) ->
       let s = TmI.find tag start in
       let e = s + (TmI.find tag len) in
         if i >= s && i < e then
           let tmp = f v in
             TmImA.add tag i (fst tmp) a, TmImA.add tag i (snd tmp) b, TmImA.remove tag i p
         else
           (a,b,p)
    ) r (TmImA.empty,TmImA.empty,r)

