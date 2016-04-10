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
(* src/bstring.ml                                                             *)
(* Annotated strings                                                          *)
(* $Id: bstring.ml 4901 2010-05-13 21:14:49Z cretin $ *)
(******************************************************************************)

module Rx = Brx
module C = Bcost
module W = Bannot.Weight
module T = Btag
module TmAl = Btag.MapAList
module TmA = Btag.MapA
module TmI = Btag.MapInt
module TmImA = Btag.MapIntMapA
module Is = Int.Set

let error s =
  Berror.run_error (Info.M "bstring.ml") (
    fun () -> print_endline s
  )

type t = string * int * int
    (* s, i, j
       where  0 <= i <= j <= String.length s
    *)
type child =
  | TagNode of T.t * a * int option
      (* the [int option] is always [None] in [at] and [Some] in [cat]
      only for chunk annotations *)
  | Leaf of W.t
and a = (child * int) list
    (* [n1, j1; ...; nk, jk] with s, i, j
       where  i <= j1 <= ... <= jk = j
       and  n1 with s, i, j1
       and  ...
       and  nk with s, j(k-1), jk
       and k >= 1
    *)
type at = a * t  (* every component is abstract *)
type attmp = a list * t (* temporary at *)
type cat = Chunk of at
type chunkmap = ((int * int) * cat) TmImA.t

let empty = ("", 0, 0)

let of_string s =
  String.copy s, 0, String.length s

let to_attmp s :attmp =
  [[]], s

let to_string (s, i, j) = String.sub s i (j - i)

let of_at (_, s) = s

let of_attmp (_, s) = s

let of_cat (Chunk at) = of_at at

let length (s, i, j) = j - i

let old_dist s1 s2 =
  let m = String.length s1 in
  let n = String.length s2 in
  let aodd = Array.make (succ n) 0 in
  let aeven = Array.make (succ n) 0 in
  for j=0 to n do aeven.(j) <- j done;
  for i=1 to m do
    let apredi = if i mod 2 = 0 then aodd else aeven in
    let ai = if i mod 2 = 0 then aeven else aodd in
    ai.(0) <- i;
    for j = 1 to n do
      ai.(j) <-
        (if String.get s1 (pred i) = String.get s2 (pred j) then
           min apredi.(j-1)
         else
           (fun x -> x)
        ) (min (apredi.(j) + 1) (ai.(j-1) + 1))
    done
  done;
  if m mod 2 = 0 then aeven.(n) else aodd.(n)

(* returns [Some dist] if dist < limit
           [None] else *)
let dist_limit_aux limit s t =
  let n = String.length s in
  let m = String.length t in
  assert (m <= n);
  let ai0 = Array.make (succ n) 0 in
  let ai1 = Array.make (succ n) 0 in
  let aj0 = Array.make (succ m) 0 in
  let aj1 = Array.make (succ m) 0 in
  let addloop i n a = (* do [succ (a.(pred))] on [a] from [i] until [limit] and returns [end] *)
    let rec f i =
      if i > n then succ n
      else (
        a.(i) <- succ a.(pred i);
        if a.(i) >= limit then i
        else f (succ i)
      )
    in
    f i
  in
  let findloop i n a = (* returns [j] such that all [k] >= [j], [a.(k)] >= [limit] *)
    let rec f i =
      if i < 0 then 0
      else
        if a.(i) < limit then succ i
        else f (pred i)
    in
    if i > n then succ n
    else f i
  in
  let addfindloop i n a = findloop (addloop i n a) n a in
  let minloop c s i n cur a apred = (* do [min ...] on [a] from [i] until [cur] *)
    let rec f i =
      if i > n then () else (
        if i = cur
        then (
          if s.[pred i] = c
          then a.(i) <- min (succ a.(pred i)) apred.(pred i)
          else a.(i) <- succ a.(pred i);
        ) else (
          if s.[pred i] = c
          then a.(i) <- min (succ (min apred.(i) a.(pred i))) apred.(pred i)
          else a.(i) <- succ (min apred.(i) a.(pred i));
          f (succ i)
        )
      )
    in
    f i
  in
  let rec loop x curi curj apredi ai apredj aj =
(*     Printf.printf "x: %d  curi: %d  curj: %d\n" x curi curj; *)
    if x > m
    then
      if curj <= n
      then None
      else Some apredi.(n)
    else (
      if x >= curi
      then (  (* x >= curi *)
        if x <> curi
        then (  (* x > curi *)
          if x >= curj then None
          else (  (* x < curj *)
            ai.(x) <- succ apredi.(x);
            aj.(x) <- ai.(x);
            let newcuri = addfindloop (succ x) m aj in
            minloop t.[pred x] s (succ x) n curj ai apredi;
            let newcurj = addfindloop (succ curj) n ai in
            loop (succ x) newcuri newcurj ai apredi aj apredj
          )
        ) else (  (* x = curi *)
          if x = curj
          then (  (* x = curj *)
            assert (apredi.(pred x) = apredj.(pred x));
            if s.[pred x] = t.[pred x]
            then (
              ai.(x) <- apredi.(pred x);
              aj.(x) <- ai.(x);
              let newcurj = addfindloop (succ x) n ai in
              let newcuri = addfindloop (succ x) m aj in
              loop (succ x) newcuri newcurj ai apredi aj apredj
            ) else None
          ) else (  (* x < curj  because  x = curi *)
            assert (x < curj);
            if s.[pred x] = t.[pred x]
            then ai.(x) <- min (succ apredi.(x)) apredi.(pred x)
            else ai.(x) <- succ apredi.(x);
            aj.(x) <- ai.(x);
            let newcuri = addfindloop (succ x) m aj in
            minloop t.[pred x] s (succ x) n curj ai apredi;
            let newcurj = addfindloop (succ curj) n ai in
            loop (succ x) newcuri newcurj ai apredi aj apredj
          )
        )
      ) else (  (* x < curi *)
        if x >= curj
        then (  (* x >= curj *)
          if x <> curj
          then (  (* x > curj *)
            aj.(x) <- succ apredj.(x);
            ai.(x) <- aj.(x);
            let newcurj = addfindloop (succ x) n ai in
            minloop s.[pred x] t (succ x) m curi aj apredj;
            let newcuri = addfindloop (succ curi) m aj in
            loop (succ x) newcuri newcurj ai apredi aj apredj
          ) else (  (* x = curj *)
            if s.[pred x] = t.[pred x]
            then aj.(x) <- min (succ apredj.(x)) apredj.(pred x)
            else aj.(x) <- succ apredj.(x);
            ai.(x) <- aj.(x);
            let newcurj = addfindloop (succ x) n ai in
            minloop s.[pred x] t (succ x) m curi aj apredj;
            let newcuri = addfindloop (succ curi) m aj in
            loop (succ x) newcuri newcurj ai apredi aj apredj
          )
        ) else (  (* x < curj *)
            if s.[pred x] = t.[pred x]
            then aj.(x) <- min (succ (min apredj.(x) apredi.(x))) apredj.(pred x)
            else aj.(x) <- succ (min apredj.(x) apredi.(x));
            ai.(x) <- aj.(x);
            minloop t.[pred x] s (succ x) n curj ai apredi;
            let newcurj = addfindloop (succ curj) n ai in
            minloop s.[pred x] t (succ x) m curi aj apredj;
            let newcuri = addfindloop (succ curi) m aj in
            loop (succ x) newcuri newcurj ai apredi aj apredj
        )
      )
    )
  in
  let curi = addloop 1 m aj0 in
  let curj = addloop 1 n ai0 in
  let result =
    if m <> 0 && n <> 0
    then loop 1 curi curj ai0 ai1 aj0 aj1
    else Some (max m n)
  in
  result

let rev_s s =
  let n = String.length s in
  let t = String.make n '\000' in
  let rec loop x y =
    if x >= n
    then t
    else (
      String.set t x s.[y];
      loop (succ x) (pred y)
    )
  in
  loop 0 (pred n)

(* returns [m] such that dist s t <= m *)
let dist_heurist s t =
  let n = String.length s in
  let m = String.length t in
  assert (m <= n);
  let pm = pred m in
  let pn = pred n in
  let rec loop i j acc =
    if i >= pm
    then acc + 2 + (n - j)
    else
      if j >= pn
      then acc + 2 + (m - i)
      else
        if t.[i] = s.[j]
        then loop (succ i) (succ j) acc
        else
          if n - j > m - i
          then
            if t.[i] = s.[succ j]
            then loop (succ i) (j + 2) (succ acc)
            else
              if t.[succ i] = s.[j]
              then loop (i + 2) (succ j) (succ acc)
              else loop i (succ j) (succ acc)
          else
            if t.[succ i] = s.[j]
            then loop (i + 2) (succ j) (succ acc)
            else
              if t.[i] = s.[succ j]
              then loop (succ i) (j + 2) (succ acc)
              else loop (succ i) j (succ acc)
  in
  loop 0 0 0

let dist_aux limito s1 s2 =
  let limit =
    let heurist =
      min
        (dist_heurist s1 s2)
        (dist_heurist (rev_s s1) (rev_s s2))
    in
    match limito with
    | None -> heurist
    | Some limit -> min (max limit 0) heurist
  in
  let limit = succ limit in
  let result = dist_limit_aux limit s1 s2 in
(*   (let real = old_dist s1 s2 in *)
(*    match result with *)
(*    | None -> assert (real >= limit) (\* Printf.printf "'%s' '%s' %d > %d\n" s1 s2 real limit *\) *)
(*    | Some dist -> assert (real = dist) (\* Printf.printf "'%s' '%s' %d = %d\n" s1 s2 real dist *\)); *)
  match limito with
  | Some _ -> result
  | None ->
      match result with
      | None -> assert false
      | Some dist -> Some dist

let dist limit s1 s2 =
  let s1, s2 =
    if String.length s1 >= String.length s2
    then s1, s2
    else s2, s1
  in
  let result = dist_aux limit s1 s2 in
  result

let generic_at_print p (a, (s, i, j)) =
  let buf = Buffer.create 16 in
  let add_sub i j =
    Buffer.add_substring buf s i (j - i)
  in
  let add_str = Buffer.add_string buf in
  add_sub (p add_sub add_str a i) j;
  Buffer.contents buf

let at_print_flat s =
  let p add _ a i =
    let rec r add a i =
      match a with
      | [] -> i
      | (Leaf _, j)::a ->
          add i j;
          r add a j
      | (TagNode (_, _, _), j)::a ->
          r add a j
    in
    r add a i
  in
  generic_at_print p s

let cat_print_flat (Chunk at) = at_print_flat at

let at_print_all =
  let p add_sub add_str =
    let rec p a i =
      match a with
      | [] -> i
      | (Leaf w, j)::a ->
          add_str (Printf.sprintf "{:%s:" (W.to_string w));
          add_sub i j;
          add_str "}";
          p a j
      | (TagNode (tag, r, pos), j)::a ->
          let pos =
            match pos with
            | Some x -> "(" ^ string_of_int x ^ ")"
            | None -> ""
          in
          add_str ("<" ^ T.to_string tag ^ pos ^ ":");
          let i = p r i in
          assert (i = j);
          add_str ">";
          p a j
    in
    p
  in
  generic_at_print p

let toplevel_chunks (Chunk at) : int TmAl.t =
  let rec f a i (acc:int TmAl.t) =
    match a with
    | [] -> acc
    | (h, j)::a ->
        let acc =
          match h with
          | TagNode (tag, r, Some pos) ->
              TmAl.add tag pos acc
          | TagNode (_, r, None) ->
              f r i acc
          | Leaf _ ->
              acc
        in
        f a j acc
  in
  let a, (s, i, j) = at in
  let tal = TmAl.rev (f a i TmAl.empty) in
(*   print_endline ("+++++toplevel_chunks: \"" ^ to_string (of_at at) ^ "\""); *)
(*   TmAl.print_list ( *)
(*     fun t -> print_endline (T.to_string t ^ ":") *)
(*   ) ( *)
(*     fun l -> Safelist.iter (fun x -> print_int x; print_string " ") l; print_endline "" *)
(*   ) tal; *)
(*   print_endline "-----toplevel_chunks."; *)
  tal

let cat_fold_on_locs step (Chunk (a, _)) acc =
  let rec f a acc =
    Safelist.fold_left (
      fun acc h ->
        match h with
        | TagNode (tag, r, Some p), _ ->
            f r (step tag p acc)
        | TagNode (_, _, None), _
            -> assert false
        | Leaf _, _ -> acc
    ) acc a
  in
  f a acc

(* [Bstring.at_to_locs as] returns [len] describing the number of
chunks for each tag. *)
let at_to_locs (a, _) =
  let rec locs a len =
    Safelist.fold_left (
      fun len h ->
        match h with
        | TagNode (tag, r, _), _ ->
            locs r (TmI.incr tag len)
        | _ -> len
    ) len a
  in
  locs a TmI.empty

let match_rx t (s, i, j) =
  Rx.match_sub_string t s i j

(* let at_to_weight_flat (a, (s, i, j)) = *)
(*   let rec f a i l z = *)
(*     match a with *)
(*     | [] -> i, l, z *)
(*     | (Leaf w, j)::a -> *)
(*         f a j (((i, j), w)::l) (z + j - i) *)
(*     | (TagNode (_, _, _), j)::a -> *)
(*         f a j l z *)
(*   in *)
(*   let i, l, z = f a i [] 0 in *)
(*   assert (i = j); *)
(*   let aw = Array.make z W.zero in *)
(*   let r, _ = *)
(*     Safelist.fold_left ( *)
(*       fun (r, k) ((i, j), w) -> *)
(*         for p = z - k - j + i to z - k - 1 do *)
(*           aw.(p) <- w *)
(*         done; *)
(*         to_string (s, i, j) ^ r, (k + j - i) *)
(*     ) ("", 0) l *)
(*   in *)
(*   aw, r *)

let at_to_key (a, (s, i, j)) =
  let rec f a i l =
    match a with
    | [] -> i, l
    | (Leaf w, j)::a ->
        let l =
          if W.to_int w = 0 then l else (i, j)::l
        in
        f a j l
    | (TagNode (tag, an, _), j)::a when T.key_through tag ->
        let i, l = f an i l in
        assert (i = j);
        f a j l
    | (TagNode (_, _, _), j)::a ->
        f a j l
  in
  let i, l = f a i [] in
  assert (i = j);
  Safelist.fold_left (
    fun r (i, j) -> to_string (s, i, j) ^ r
  ) "" l

let cat_to_key (Chunk at) = at_to_key at

(* let at_dist at1 at2 = *)
(*   let w1, s1 = at_to_weight_flat at1 in *)
(*   let w2, s2 = at_to_weight_flat at2 in *)
(*   let m = String.length s1 in *)
(*   let n = String.length s2 in *)
(*   let aodd = Array.make (succ n) 0 in *)
(*   let aeven = Array.make (succ n) 0 in *)
(*   for j = 1 to n do *)
(*     aeven.(j) <- W.succ_int w2.(pred j) aeven.(pred j) *)
(*   done; *)
(*   for i = 1 to m do *)
(*     let apredi = if i mod 2 = 0 then aodd else aeven in *)
(*     let ai = if i mod 2 = 0 then aeven else aodd in *)
(*     ai.(0) <- W.succ_int w1.(pred i) apredi.(0); *)
(*     for j = 1 to n do *)
(*       ai.(j) <- *)
(*         (if (String.get s1 (pred i) = String.get s2 (pred j)) *)
(*            && (w1.(pred i) = w2.(pred j)) *)
(*          then min apredi.(pred j) *)
(*          else (fun x -> x) *)
(*         ) (min (W.succ_int w1.(pred i) apredi.(j)) (W.succ_int w2.(pred j) ai.(pred j))) *)
(*     done *)
(*   done; *)
(*   if m mod 2 = 0 then aeven.(n) else aodd.(n) *)

(* let cat_dist (Chunk at1) (Chunk at2) = *)
(*   at_dist at1 at2 *)

let at_crtdel_cost (a, (s, i, j)) =
  let rec f a i (cs, ct) =
    match a with
    | [] -> i, (cs, ct)
    | (Leaf w, j)::a ->
        let c = (+) (W.weight_int w (j - i)) in
        f a j (c cs, c ct)
    | (TagNode (tag, n, _), j)::a ->
        let i, (csn, ct) = f n i (cs, ct) in
        assert (i = j);
        let cs =
          if T.key_through tag
          then csn
          else cs
        in
        f a j (cs, ct)
  in
  let i, c = f a i (0, 0) in
  assert (i = j);
  c

let at_to_chunktree (a, (s, si, sj)) =
  let rec f a i (acc:a) (cur:TmI.t) (g:cat TmImA.t) =
    match a with
    | [] -> Safelist.rev acc, cur, g
    | (h, j)::a ->
        let h, acc, cur, g =
          match h with
          | Leaf _ -> h, acc, cur, g
          | TagNode (tag, r, None) ->
              let pos = TmI.find tag cur in
              let cur = TmI.incr tag cur in
              let r, cur, g = f r i [] cur g in
              let g = TmImA.add tag pos (Chunk (r, (s, i, j))) g in
              TagNode (tag, r, Some pos), acc, cur, g
          | TagNode (_, _, Some _) -> assert false
        in
        f a j ((h, j)::acc) cur g
  in
  let a, _, g = f a si [] TmI.empty TmImA.empty in
  let crtdel (Chunk at) = (at_crtdel_cost at), Chunk at in
  Chunk (a, (s, si, sj)), TmImA.map crtdel g

let pick_pos n l =
  let rec p l a n =
    match l, n with
    | [], _
    | _, 0 -> a
    | h::l, n -> p l h (pred n)
  in
  p (List.tl l) (List.hd l) n

let find_pos n l =
  let rec p l i =
    match l with
    | [] -> assert false
    | h::_ when h = n -> i
    | _::l -> p l (succ i)
  in
  p l 0

let concat_ambiguous_split n r1 r2 s =
  let p = Rx.split_positions r1 r2 (to_string s) in
  let l = Int.Set.elements p in
  let s, i, j = s in
  let k = i + pick_pos n l in
  (s, i, k), (s, k, j)

let find_concat_split r1 r2 n s =
  let p = Rx.split_positions r1 r2 s in
  let l = Int.Set.elements p in
  find_pos n l

let concat_split r1 r2 s =
  let p = Rx.split_positions r1 r2 (to_string s) in
  if Int.Set.cardinal p <> 1
  then error "concat_split: bad split";
  let s, i, j = s in
  let k = i + Int.Set.choose p in
  (s, i, k), (s, k, j)

let star_ambiguous_split ns r s =
  let rs = Rx.mk_star r in
  let r_not_empty = not (Rx.match_string r "") in
  let default = if r_not_empty then 0 else 1 in
  let rec loop ns s acc =
    if (length s = 0) && (r_not_empty || (ns = []))
    then Safelist.rev acc
    else (
      let n, ns =
        match ns with
        | [] -> default, []
        | n::ns -> n, ns
      in
      let x, s = concat_ambiguous_split n r rs s in
      loop ns s (x::acc)
    )
  in
  loop ns s []

let find_star_split r ns s =
  let rs = Rx.mk_star r in
  let rec loop ns s z acc =
    match ns with
    | [] ->
        assert (String.length s = 0);
        Safelist.rev acc
    | n::ns ->
        let i = find_concat_split r rs n s in
        let z = z - n in
        let s = String.sub s n z in
        loop ns s z (i::acc)
  in
  let is = loop ns s (String.length s) [] in
(*   print_string "["; *)
(*   Safelist.fold_left ( *)
(*     fun b i -> *)
(*       if b then print_string ", "; *)
(*       print_int i; *)
(*       true *)
(*   ) false is; *)
(*   print_endline "]"; *)
  is

let star_split r s =
  let rs = Rx.mk_star r in
  let pos = Is.remove 0 (Rx.split_positions rs rs (to_string s)) in
  let s, i, j1 = s in
  let l, j2 =
    Int.Set.fold (
      fun j (l, k) ->
        let j = i + j in
        (s, k, j)::l, j
    ) pos ([], i)
  in
  assert (j1 = j2);
  Safelist.rev l

(* pre: p1 and p2 only touch annotations *)
let do_concat r1 r2 p1 p2 ((a, s):attmp) :attmp =
  (* post: only annotations are touched *)
  let p = Rx.split_positions r1 r2 (to_string s) in
(*   if Int.Set.cardinal p <> 1 *)
(*   then ( *)
(*     Berror.run_error (Info.M "bstring.ml: do_concat") ( *)
(*       let fmt _ = Rx.format_t in *)
(*       fun () -> Util.format "@[%a@]\n@[%a@]\n@[\"%s\"@]" fmt r1 fmt r2 (to_string s) *)
(*     ) *)
(*   ); *)
  let s, i, j = s in
  let k = i + Int.Set.choose p in
  let a, _ = p1 (a, (s, i, k)) in
  let a, _ = p2 (a, (s, k, j)) in
  a, (s, i, j)

(* pre: p only touch annotations *)
let do_star r p ((a, s):attmp) :attmp =
  (* post: only annotations are touched *)
  let rs = Rx.mk_star r in
  let pos = Is.remove 0 (Rx.split_positions rs rs (to_string s)) in
  let s, i, j1 = s in
  let a, j2 =
    Int.Set.fold (
      fun j (a, k) ->
        let j = i + j in
        let a, _ = p (a, (s, k, j)) in
        a, j
    ) pos (a, i)
  in
  assert (j1 = j2);
  a, (s, i, j1)

let annot_leaf w ((a, (s, i, j)):attmp) :attmp =
  (* only touch leaf annotation *)
  (match a with
   | h::t -> ((Leaf w, j)::h)::t
   | _ -> error "annot_leaf"),
  (s, i, j)

let before_node ((a, s):attmp) :attmp =
  (* only touch node annotation *)
  []::a,
  s

let annot_node n ((a, (s, i, j)):attmp) :attmp =
  (* only touch node annotation *)
  (match a with
   | h::b::q -> ((TagNode (n, h, None), j)::b)::q
   | _ -> error "annot_node"),
  (s, i, j)

(* pre: [a] is a singleton *)
let at_of_attmp ((a, s):attmp) :at =
  let rec r a =
    Safelist.rev_map (
      function
      | Leaf w, j -> Leaf w, j
      | TagNode (n, a, None), j -> TagNode (n, r a, None), j
      | TagNode (_, _, Some _), _ -> assert false
    ) a
  in
  (match a with
   | [x] -> r x
   | _ -> error "at_of_attmp"),
  s
