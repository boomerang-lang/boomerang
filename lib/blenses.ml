(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2008 J. Nathan Foster and Benjamin C. Pierce                 *)
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
(* /src/blenses.ml                                                            *)
(* Boomerang lens combinators                                                 *)
(* $Id: blenses.ml 4901 2010-05-13 21:14:49Z cretin $ *)
(******************************************************************************)

(* ---------------------------------------------------------------------------*)
(* IMPORTS AND ABBREVIATIONS *)

module Rx = Brx
module Arx = Barx
module Err = Berror
module W = Bannot.Weight
module G = Balign.Alignment
module P = Balign.Permutation
module T = Btag
module Ts = T.Set
module TmA = T.MapA
module TmI = T.MapInt
module TmImA = T.MapIntMapA

let sprintf = Printf.sprintf
let msg = Util.format
let (@) = Safelist.append

(* ---------------------------------------------------------------------------*)
(* TYPES *)

type tag = T.t

(* ----- C complements ----- *)
type union_side = Union_left | Union_right
type complement =
  | C_box
  | C_string of Bstring.t
  | C_concat of complement * int * complement
  | C_compose of complement * complement
  | C_list of complement list
  | C_star of complement list * int list
  | C_union of union_side * complement
  | C_group of complement list

let rec print_complement c =
  match c with
  | C_box -> print_string "[_]"
  | C_string s -> print_string (Bstring.to_string s)
  | C_concat (c1, _, c2) ->
      print_complement c1;
      print_string ".";
      print_complement c2
  | C_compose (c1, c2) ->
      print_complement c1;
      print_string ";";
      print_complement c2
  | C_list cs ->
      print_string "[";
      ignore (Safelist.fold_left (
        fun b c ->
          if b then print_string ",";
          print_complement c;
          true
      ) false cs);
      print_string "]"
  | C_star (cs, _)
  | C_group(cs) ->
      print_complement (C_list cs)
  | C_union (Union_left, c) ->
      print_string "|L";
      print_complement c
  | C_union (Union_right, c) ->
      print_string "|R";
      print_complement c

let rec format_complement c =
  let fmt _ c = format_complement c in
  msg "@[";
  (match c with
   | C_box -> msg "C_box"
   | C_string s -> msg "C_string(\"%s\")" (Bstring.to_string s)
   | C_concat (a, _, b) -> msg "C_concat(%a,%a)" fmt a fmt b
   | C_compose (a, b) -> msg "C_compose(%a,%a)" fmt a fmt b
   | C_list l -> msg "C_list(%a)" (fun _ -> Misc.format_list "," format_complement) l
   | C_star (l, _) -> msg "C_star(%a)" (fun _ -> Misc.format_list "," format_complement) l
   | C_union (s, c) -> msg ("C_union(%s,%a)") (
       match s with Union_left -> "Left" | Union_right -> "Right"
     ) fmt c
   | C_group (l) -> msg "C_group(%a)" (fun _ -> Misc.format_list "," format_complement) l
  );
  msg "@]"

type ktype =
  | K_box of T.t
  | K_regexp of Rx.t
  | K_concat of ktype * ktype
  | K_union of ktype * ktype
  | K_star of ktype
  | K_permute of ktype list
  | K_group of ktype
  | K_compose of ktype * ktype

let rec ktype_equiv a b =
  match a, b with
  | K_box a, K_box b -> T.equiv a b
  | K_regexp a, K_regexp b -> Rx.equiv a b
  | K_concat (a, c), K_concat (b, d)
  | K_union (a, c), K_union (b, d)
  | K_compose (a, c), K_compose (b, d)
      -> ktype_equiv a b && ktype_equiv c d
  | K_star a, K_star b -> ktype_equiv a b
  | K_permute a, K_permute b -> (
      try Safelist.for_all2 ktype_equiv a b
      with Invalid_argument _ -> false
    )
  | K_group a, K_group b -> ktype_equiv a b
  | _ -> false

let rec format_ktype t =
  let fmt _ t = format_ktype t in
  msg "@[";
  (match t with
   | K_box t -> msg (* "K_box(%s)" *) "[%s]" (T.to_string t)
   | K_regexp r -> msg (* "K_regexp(%a)" *) "/%a/" (fun _ -> Rx.format_t) r
   | K_concat (a, b) -> msg (* "K_concat(%a,%a)" *) "(%a@,@ .@ %a)" fmt a fmt b
   | K_union (a, b) -> msg (* "K_union(%a,%a)" *) "(%a@;@ |@,@ %a)" fmt a fmt b
   | K_compose (a, b) -> msg (* "K_compose(%a,%a)" *) "(%a@;@ ;@,@ %a)" fmt a fmt b
   | K_star t -> msg (* "K_star(%a)" *) "(%a* )" fmt t
   | K_permute l -> msg (* "K_permute(%a)" *) "[%a]" (fun _ -> Misc.format_list ",@,@ " format_ktype) l
   | K_group t -> msg "group(%a)" fmt t
  );
  msg "@]"

(* ----- M match type ----- *)
type mtype = ktype TmA.t

let format_mtype mt =
  msg "@[";
  TmA.iter (fun t k -> msg "@[%s@ ->@ %a@]@;" (T.to_string t) (fun _ -> format_ktype) k) mt;
  msg "@]"

let mtype_equiv = TmA.equal ktype_equiv

let mt_add t k mt =
  match () with
  | _ when not (TmA.mem t mt) -> TmA.add t k mt
  | _ when ktype_equiv (TmA.find t mt) k -> mt
  | _ -> assert false

let mtype_match_compatible_cex t k mt =
  if not (TmA.mem t mt)
  then None
  else
    if not (TmA.compatible t mt)
    then Some (sprintf "tagname '%s' used with non compatible taginfo (species and predicate)" (T.get_name t))
    else
      if not (ktype_equiv k (TmA.find t mt))
      then Some (sprintf "tag '%s' used with different ktypes" (T.to_string t))
      else None

let mt_merge mt1 mt2 =
  TmA.fold mt_add mt1 mt2

let mtype_compatible_cex mt1 mt2 =
  TmA.fold (
    fun t k so ->
      match so with
      | Some _ -> so
      | None -> mtype_match_compatible_cex t k mt2
  ) mt1 None

let mt_compose mt1 mt2 =
  TmA.mapi (
    fun t k1 ->
      let k2 = TmA.find t mt2 in
      K_compose (k1, k2)
  ) mt1

let mtype_domain_equal = TmA.equal (fun _ _ -> true)

(* ----- equivalence relation types ----- *)
type equiv = Identity | Unknown

(* helper for merging equivs *)
let equiv_merge r1 r2 =
  match r1, r2 with
  | Identity, Identity -> Identity
  | _ -> Unknown

(* ---------------------------------------------------------------------------*)
(* HELPERS *)

(* helper for concatenating an array *)
let concat_array a =
  let buf = Buffer.create 17 in
    Array.iter (Buffer.add_string buf) a;
    Buffer.contents buf

(* ---------------------------------------------------------------------------*)
(* PERMUTATIONS *)
module Permutations = struct
  let string_of_sigma sigma =
    sprintf "[%s]" (Misc.concat_list "," (Safelist.map string_of_int sigma))

  let rec identity k =
    let rec loop acc i =
      if i < 0 then acc
      else loop (i::acc) (pred i) in
    loop [] (pred k)

  let valid_permutation sigma ls =
    let k = Safelist.length sigma in
      Safelist.length ls = k
      && Safelist.sort compare sigma = identity k

  let permutations k =
    let rec insertions n ls =
      let (is,_) =
        Safelist.fold_left
          (fun (ls_n_acc,ls_acc) i ->
             let ls_acc' = i::ls_acc in
             let ls_n_acc' = Safelist.map (fun ls_n -> i::ls_n) ls_n_acc in
               ((n::ls_acc')::ls_n_acc',ls_acc'))
          ([[n]],[]) ls in
        is in
    let rec mk_perms k =
      if k = 0 then [[]]
      else Safelist.concat (Safelist.map (insertions (pred k)) (mk_perms (pred k))) in
    let id = identity k in
    id::(Safelist.remove id (mk_perms k))

  let permutation sigma k =
    let err () =
      Err.run_error (Info.M "permutation")
        (fun () -> msg "@[%s@ is@ not@ a@ valid@ permutation@ on@ {0,..,%d}@]@\n"
           (string_of_sigma sigma) (pred k)) in
    let sigma_arr = Array.create k (-1) in
    let sigma_inv_arr = Array.create k (-1) in
    begin
      let k' =
        Safelist.fold_left
          (fun i j ->
             sigma_arr.(i) <- j;
             if sigma_inv_arr.(j) <> (-1) then err ();
             sigma_inv_arr.(j) <- i;
             succ i)
          0 sigma in
      if k' <> k then err ()
    end;
    (sigma_arr,sigma_inv_arr)

  let invert_permutation sigma =
    let sigma_arr = Array.of_list sigma in
    let sigma_inv_arr = Array.create (Array.length sigma_arr) (-1) in
    begin
      Array.iteri
        (fun i j ->
           if sigma_inv_arr.(j) = -1 then sigma_inv_arr.(j) <- i
           else
             Err.run_error (Info.M "invert_permutation")
               (fun () -> msg "@[%s@ is@ not@ a@ valid@ permutation@ on@ {0,..,%d}@]@\n"
                  (string_of_sigma sigma) (pred (Safelist.length sigma))))
        sigma_arr
    end;
    Array.to_list sigma_inv_arr

  let permute_list sigma ls =
    let ls_arr = Array.of_list ls in
    let k = Array.length ls_arr in
    let _,sigma_inv_arr = permutation sigma k in
    Array.fold_right
      (fun j ls' -> ls_arr.(j)::ls')
      sigma_inv_arr []
end

(* ---------------------------------------------------------------------------*)
(* CANONIZERS *)
module Canonizer = struct
  type d =
    | Copy of Rx.t
    | Concat of t * t
    | Union of t * t
    | Star of t
    | Normalize of Rx.t * Rx.t * (string -> string)
    | Sort of int * (int * Arx.t * Rx.t) list
    | Columnize of int * Rx.t * char * string
    | FromLens of Arx.t * Arx.t * equiv * mtype
        * (Bstring.t -> (P.t * TmI.t) -> (P.t * TmI.t))
        * (Bstring.t -> string) * (Bstring.t -> string)

  and t =
      { (* ----- meta data ----- *)
        info : Info.t;
        desc : d;
        (* ----- types ----- *)
        mutable uncanonized_atype : Arx.t option;
        mutable canonized_atype : Arx.t option;
        mutable cnrel : equiv option;
      }

  let mk i d =
    { info = i;
      desc = d;
      uncanonized_atype = None;
      canonized_atype = None;
      cnrel = None;
    }

  (* ----- accessors ----- *)
  let rec uncanonized_type cn = Arx.rxtype (uncanonized_atype cn)
  and uncanonized_atype cn =
    match cn.uncanonized_atype with
    | Some ut -> ut
    | None ->
        let ut = match cn.desc with
          | Copy r               -> Arx.mk_rx r
          | Concat (cn1, cn2)    -> Arx.mk_seq (uncanonized_atype cn1) (uncanonized_atype cn2)
          | Union (cn1, cn2)     -> Arx.mk_alt (uncanonized_atype cn1) (uncanonized_atype cn2)
          | Star cn              -> Arx.mk_star (uncanonized_atype cn)
          | Columnize (k, r, ch, nl) -> Arx.mk_rx (Rx.mk_expand r (Char.code ch) (Rx.mk_string nl))
          | Normalize (ct, ct0, f) -> Arx.mk_rx ct
          | Sort (_, irl) ->
              Arx.mk_star (Safelist.fold_left (fun acc (_, ari, _) -> Arx.mk_alt acc ari) Arx.empty irl)
          | FromLens (ct, _, _, _, _, _, _) -> ct in
        cn.uncanonized_atype <- Some ut;
        ut

  and canonized_type cn = Arx.rxtype (canonized_atype cn)
  and canonized_atype cn =
    match cn.canonized_atype with
    | Some ct -> ct
    | None ->
        let ct = match cn.desc with
          | Copy r               -> Arx.mk_rx r
          | Concat (cn1, cn2)    -> Arx.mk_seq (canonized_atype cn1) (canonized_atype cn2)
          | Union (cn1, cn2)     -> Arx.mk_alt (canonized_atype cn1) (canonized_atype cn2)
          | Star cn              -> Arx.mk_star (canonized_atype cn)
          | Columnize (k, r, ch, nl) -> Arx.mk_rx r
          | Normalize (ct, ct0, f) -> Arx.mk_rx ct0
          | Sort (_, irl) ->
              Safelist.fold_left (fun acc (_, ari, _) -> Arx.mk_seq ari acc) Arx.epsilon irl (* NB order! *)
          | FromLens (_, at, _, _, _, _, _) -> at in
        cn.canonized_atype <- Some ct;
        ct

  and cnrel cn = match cn.cnrel with
    | Some cr -> cr
    | None ->
        let cr = match cn.desc with
          | Copy _
          | Columnize _
          | Normalize _
          | Sort _
            -> Identity
          | Concat (cn1, cn2) -> equiv_merge (cnrel cn1) (cnrel cn2)
          | Union (cn1, cn2) ->
              if Rx.is_empty (Rx.mk_diff (canonized_type cn2) (canonized_type cn1)) then (cnrel cn1)
              else equiv_merge (cnrel cn1) (cnrel cn2)
          | Star cn -> cnrel cn
          | FromLens (_, _, eq, _, _, _, _) -> eq in
        cn.cnrel <- Some cr;
        cr

  and gperm cn u pi = (* returns pi *)
    let basic = pi in
    match cn.desc with
    | Copy _
    | Columnize _
    | Normalize _
      -> basic
    | Concat (cn1, cn2) ->
        let u1, u2 = Bstring.concat_split (uncanonized_type cn1) (uncanonized_type cn2) u in
        gperm cn2 u2 (gperm cn1 u1 pi)
    | Union (cn1, cn2) ->
        if Bstring.match_rx (uncanonized_type cn1) u
        then gperm cn1 u pi
        else gperm cn2 u pi
    | Star cn ->
        let us = Bstring.star_split (uncanonized_type cn) u in
        Safelist.fold_left (
          fun pi u -> gperm cn u pi
        ) pi us
    | Sort (k, irl) ->
        let p, i = pi in
        let ul = Bstring.star_split (uncanonized_type cn) u in
        if Safelist.length ul > k
        then Err.run_error (Info.M "sort.canonize") (
          fun () -> msg "@[%s@ split@ into@ more@ than@ %d@ pieces@]" (Bstring.to_string u) k
        );
        let c_shift = Array.create k i in
        let jzl, rs' =
          Safelist.fold_left (
            fun (jzl, rs) ui ->
              match Safelist.partition (fun (_, _, ri) -> Bstring.match_rx ri ui) rs with
              | [], rs' -> jzl, rs'
              | [i, arx, _], rs' ->
                  let shift = Bstring.at_to_locs (Arx.parse arx ui) in
                  for j = i + 1 to k - 1 do
                    c_shift.(j) <- TmI.plus c_shift.(j) shift
                  done;
                  (i, shift)::jzl, rs'
              | _ -> Err.run_error (Info.M "sort.canonize")
                  (fun () -> msg "@[%s@ matched@ more@ than@ one@ regexp@]" (Bstring.to_string ui))
          ) ([], irl) ul
        in
        Safelist.iter (
          fun (_, _, ri) ->
            if not (Rx.is_final ri)
            then Err.run_error (Info.M "sort.canonize")
              (fun () -> msg "@[no@ string@ matched@ %s@]" (Rx.string_of_t ri))
        ) rs';
        Safelist.fold_left (
          fun (p, i) (j, shift) ->
            let c_shift = c_shift.(j) in
            TmI.fold (
              fun t s p ->
                let ui = TmI.find t i in
                let cj = TmI.find t c_shift in
                let rec add s p =
                  if s = 0 then p
                  else
                    let s = pred s in
                    add s (P.add t (ui + s, cj + s) p)
                in
                add s p
            ) shift p,
            TmI.plus i shift
        ) pi (Safelist.rev jzl)
    | FromLens (_, _, _, _, p, _, _) -> p u pi

  and canonize cn u =
    let basic f = f (Bstring.to_string u) in
    match cn.desc with
    | Copy _ -> basic (fun u -> u)
    | Concat (cn1, cn2) ->
        let u1, u2 = Bstring.concat_split (uncanonized_type cn1) (uncanonized_type cn2) u in
        let c1 = canonize cn1 u1 in
        let c2 = canonize cn2 u2 in
        c1 ^ c2
    | Union (cn1, cn2) ->
        if Bstring.match_rx (uncanonized_type cn1) u
        then canonize cn1 u
        else canonize cn2 u
    | Star cn ->
        let buf = Buffer.create 17 in
        let us = Bstring.star_split (uncanonized_type cn) u in
        Safelist.iter (
          fun u -> Buffer.add_string buf (canonize cn u)
        ) us;
        Buffer.contents buf
    | Normalize (_, _, f) -> basic f
    | FromLens (_, _, _, _, _, get, _) -> get u
    | Sort (k, irl) ->
        (* INEFFICIENT! *)
        let ul = Bstring.star_split (uncanonized_type cn) u in
        let u_arr = Array.create k "" in
        if Safelist.length ul > k
        then Err.run_error (Info.M "sort.canonize") (
          fun () -> msg "@[%s@ split@ into@ more@ than@ %d@ pieces@]" (Bstring.to_string u) k
        );
        let rs' =
          Safelist.fold_left (
            fun rs ui ->
              match Safelist.partition (fun (_, _, ri) -> Bstring.match_rx ri ui) rs with
              | [], rs' -> rs'
              | [i, _, _], rs' -> u_arr.(i) <- Bstring.to_string ui; rs'
              | _ -> Err.run_error (Info.M "sort.canonize")
                  (fun () -> msg "@[%s@ matched@ more@ than@ one@ regexp@]" (Bstring.to_string ui))
          ) irl ul
        in
        let () =
          Safelist.iter (
            fun (_, _, ri) ->
              if not (Rx.is_final ri)
              then Err.run_error (Info.M "sort.canonize")
                (fun () -> msg "@[no@ string@ matched@ %s@]" (Rx.string_of_t ri))
          ) rs'
        in
        concat_array u_arr
    | Columnize (k, r, ch, nl) -> basic (
        fun c ->
        let c_len = String.length c in
        let nl_len = String.length nl in
        let buf = Buffer.create c_len in
        let matches s c i =
          let s_len = String.length s in
          let c_len = String.length c in
          let rec aux j =
            if j=s_len then true
            else
              (i+j < c_len)
              && ((String.get c (i+j)) = (String.get s j))
              && (aux (succ j)) in
          aux 0 in
        let rec loop i =
          if i = c_len then ()
          else
            if matches nl c i then
              (Buffer.add_char buf ch;
               loop (i + nl_len))
            else
              (Buffer.add_char buf (String.get c i);
               loop (succ i)) in
        loop 0;
        Buffer.contents buf)

  and choose cn c =
    let basic f = f (Bstring.to_string c) in
    match cn.desc with
    | Copy _ -> basic (fun c -> c)
    | Concat (cn1, cn2) ->
        let c1, c2 = Bstring.concat_ambiguous_split 0 (canonized_type cn1) (canonized_type cn2) c in
        let u1 = choose cn1 c1 in
        let u2 = choose cn2 c2 in
        u1 ^ u2
    | Union (cn1, cn2) ->
        if Bstring.match_rx (canonized_type cn1) c
        then choose cn1 c
        else choose cn2 c
    | Star cn ->
        let buf = Buffer.create 17 in
        let cs = Bstring.star_ambiguous_split [] (canonized_type cn) c in
        Safelist.iter (
          fun c -> Buffer.add_string buf (choose cn c)
        ) cs;
        Buffer.contents buf
    | Normalize _ -> basic (fun c -> c)
    | FromLens (_, _, _, _, _, _, create) -> create c
    | Sort _ -> Bstring.to_string c
    | Columnize (k, r, ch, nl) -> basic (
        fun b ->
        let b_len = String.length b in
        let nl_len = String.length nl in
        let buf = Buffer.create b_len in
        let line_buf = Buffer.create k in
        let aux_buf = Buffer.create k in
        let do_line () =
          if Buffer.length buf <> 0 && Buffer.length line_buf <> 0 then Buffer.add_string buf nl;
          Buffer.add_buffer buf line_buf;
          Buffer.reset line_buf in
        let do_space () =
          if Buffer.length line_buf <> 0 then Buffer.add_char line_buf ch;
          Buffer.add_buffer line_buf aux_buf;
          Buffer.reset aux_buf in
        let rec loop i =
          let sum =
            let nl_off = if Buffer.length buf=0 then 0 else pred nl_len in
            let aux_len = Buffer.length aux_buf in
            let line_len = let n = Buffer.length line_buf in if n=0 then n else succ n in
            nl_off + aux_len + line_len in
          if sum > k then do_line ();
          if i = b_len then (do_space (); do_line ())
          else
            let i' =
              if ch = b.[i] then (do_space (); i + 1)
              else (Buffer.add_char aux_buf b.[i]; succ i) in
            loop i' in
        loop 0;
        Buffer.contents buf)

  let info cn = cn.info

  let rec format_t cn =
    msg "@[";
    begin match cn.desc with
      | Copy(r1)              -> msg "(copy@ "; Rx.format_t r1; msg ")"
      | Concat(cn1,cn2)       -> msg "("; format_t cn1; msg "@ .@ "; format_t cn2; msg ")"
      | Union(cn1,cn2)        -> msg "("; format_t cn1; msg "@ |@ "; format_t cn2; msg ")"
      | Star(cn1)             -> format_t cn1; msg "* " (* space to avoid spurious close-comments *)
      | Normalize(r1,r2,f)    ->
          msg "(normalize@ "; Rx.format_t r1; msg "@ "; Rx.format_t r2; msg "@ <function>)"
      | FromLens(r1,r2,e1,_,_,f1,f2) ->
          msg "(canonizer_of_lens@ <lens>)" (* ??? *)
      | Sort(i1,irs)          ->
          let rec format_irs irs = match irs with
            | []       -> ()
            | [(_,ari,_)]     -> Arx.format_t ari
            | (_,ari,_)::rest -> (Arx.format_t ari; msg "@, "; format_irs rest)
          in
          msg "(asort@ #{aregexp}[@[";
          format_irs irs;
          msg "@]])"
      | Columnize(k,r,ch,nl)  ->
          msg "(columnize@ %d@ " k; Rx.format_t r; msg "@ '%c'@ \"%s\")" ch nl
    end;
    msg "@]"

  let string_of_t cn =
    Util.format_to_string
      (fun () ->
         Util.format "@[";
         format_t cn;
         Util.format "@]")

  let cnrel_identity cn = cnrel cn = Identity

  (* ----- constructors ----- *)
  let copy i r1 = mk i (Copy(r1))
  let concat i cn1 cn2 = mk i (Concat(cn1,cn2))
  let union i cn1 cn2 = mk i (Union(cn1,cn2))
  let star i cn1 = mk i (Star(cn1))
  let normalize i ct ct0 f = mk i (Normalize(ct,ct0,f))
  let sort i rl =
    let k, irl = Safelist.fold_left (fun (i,acc) ari -> (succ i,(i, ari, Arx.rxtype ari)::acc)) (0,[]) rl in
    mk i (Sort(k,irl))
  let columnize i k r sp nl = mk i (Columnize(k,r,sp,nl))
  let from_lens i ct at eq mt perm get crt = mk i (FromLens(ct,at,eq,mt,perm,get,crt))
  let iter i cn1 min maxo =
    Arx.generic_iter (copy i Rx.epsilon) (union i) (concat i) (star i)
      min maxo cn1
end

(* ---------------------------------------------------------------------------*)
(* MATCHING LENSES *)
module MLens = struct

  type d = (* description *)
    (* ----- string lenses ----- *)
    | Copy of Rx.t
    | Clobber of Rx.t * string * (string -> string)
    | Concat of t * t
    | Union of t * t
    | Star of t
    | Match of Btag.t * t

    (* ----- generic lenses ------ *)
    | Weight of (bool * W.t) * t
    | Compose of t * t
    | Align of t
    | Invert of t
    | Default of  t * Bstring.t

    (* ----- quotient lenses ----- *)
    | LeftQuot of  Canonizer.t * t
    | RightQuot of t * Canonizer.t
    | Dup1 of t * (string -> string) * Rx.t
    | Dup2 of (string -> string) * Rx.t * t

    (* ----- extensions ----- *)
    | Partition of int * (int * Rx.t) list
    | Merge of Rx.t
    | Fiat of t
    | Permute of (int * int array * int array * Rx.t array * Rx.t array) * t array

  and t =
      { (* ----- meta data ----- *)
        info : Info.t;
        desc : d;
        (* ----- types ----- *)
        mutable bij: bool option;
        mutable astype : Arx.t option;
        mutable avtype : Arx.t option;
        mutable sequiv : equiv option;
        mutable vequiv : equiv option;
        mutable ktype : ktype option;
        mutable mtype : mtype option;
      }

  let mk i d =
    { info = i;
      desc = d;
      bij = None;
      astype = None;
      avtype = None;
      sequiv = None;
      vequiv = None;
      ktype = None;
      mtype = None;
    }

  (* ----- accessors ----- *)
(*   let rec is_symmetric ml = *)
(*     Ts.equal (Barx.to_tags (avtype ml)) (Barx.to_tags (astype ml)) *)
(*   and bij ml = ... *)

  let rec bij ml = match ml.bij with
    | Some b -> b
    | None   ->
        let b = match ml.desc with
          | Copy(r1)           -> true
          | Clobber(r1,w1,f1)   -> Rx.is_singleton r1
          | Concat(ml1,ml2)    -> bij ml1 && bij ml2
          | Union(ml1,ml2)     -> bij ml1 && bij ml2 && Rx.disjoint (vtype ml1) (vtype ml2)
          | Star(ml1)          -> bij ml1
          | Weight (_, ml)     -> bij ml
          | Match (t, ml)      -> bij ml
          | Compose (ml1, ml2) -> bij ml1 && bij ml2
          | Align ml           -> bij ml
          | Invert (ml)        -> true
          | Default (ml, w)    -> bij ml
          | LeftQuot (cn, ml)  -> bij ml
          | RightQuot (ml, cn) -> bij ml
          | Dup1 (ml, f, r)    -> bij ml
          | Dup2 (f, r, ml)    -> bij ml
          | Partition(_,rs1)     ->
              let rec loop flag l = match flag,l with
                | _,[] -> true
                | true,(_,h)::t ->
                    Rx.is_empty h && loop flag t
                | false,(_,h)::t ->
                    if Rx.is_empty h then loop flag t
                    else loop true t in
              loop false rs1
          | Merge (r)          -> Rx.is_singleton r
          | Fiat ml            -> bij ml
          | Permute (_, mls)   -> Array.fold_left (fun b mli -> b && bij mli) true mls
        in
        ml.bij <- Some b;
        b

  and stype ml = Arx.rxtype (astype ml)
  and astype ml = match ml.astype with
    | Some ct -> ct
    | None ->
        let ct = match ml.desc with
          | Copy r             -> Arx.mk_rx r
          | Clobber (r, _, _)  -> Arx.mk_rx r
          | Concat (ml1, ml2)  -> Arx.mk_seq (astype ml1) (astype ml2)
          | Union (ml1, ml2)   -> Arx.mk_alt (astype ml1) (astype ml2)
          | Star ml            -> Arx.mk_star (astype ml)
          | Weight (w, ml)     -> Arx.annot_weight w (astype ml)
          | Match (t, ml)      -> Arx.mk_box t (astype ml)
          | Compose (ml, _)    -> astype ml
          | Align ml           -> Arx.mk_rx (stype ml)
          | Invert ml          -> avtype ml
          | Default (ml, _)    -> astype ml
          | LeftQuot (cn, _)   -> Canonizer.uncanonized_atype cn
          | RightQuot (ml, _)  -> astype ml
          | Dup1 (ml, _, _)    -> astype ml
          | Dup2 (_, _, ml)    -> astype ml
          | Partition (_,rs1)  -> Arx.mk_rx (Rx.mk_star (Safelist.fold_left (fun acc (_,ri) -> Rx.mk_alt acc ri) Rx.empty rs1))
          | Merge r            -> Arx.mk_rx (Rx.mk_seq r r)
          | Fiat ml            -> astype ml
          | Permute (_, mls)   ->
              Array.fold_left (fun acc mli -> Arx.mk_seq acc (astype mli))
                (Arx.mk_rx Rx.epsilon) mls
        in
        ml.astype <- Some ct;
        ct

  and vtype ml = Arx.rxtype (avtype ml)
  and avtype ml = match ml.avtype with
    | Some at -> at
    | None ->
        let at = match ml.desc with
          | Copy r             -> Arx.mk_rx r
          | Clobber (_, w, _)  -> Arx.mk_rx (Rx.mk_string w)
          | Concat (ml1, ml2)  -> Arx.mk_seq (avtype ml1) (avtype ml2)
          | Union (ml1, ml2)   -> Arx.mk_alt (avtype ml1) (avtype ml2)
          | Star ml            -> Arx.mk_star (avtype ml)
          | Weight (w, ml)     -> Arx.annot_weight w (avtype ml)
          | Match (t, ml)      -> Arx.mk_box t (avtype ml)
          | Compose (_, ml)    -> avtype ml
          | Align ml           -> Arx.mk_rx (vtype ml)
          | Invert ml          -> astype ml
          | Default (ml, _)    -> avtype ml
          | LeftQuot (_, ml)   -> avtype ml
          | RightQuot (_, cn)  -> Canonizer.uncanonized_atype cn
          | Dup1 (ml, _, r)    -> Arx.mk_seq (avtype ml) (Arx.mk_rx r)
          | Dup2 (_, r, ml)    -> Arx.mk_seq (Arx.mk_rx r) (avtype ml)
          | Partition (_,rs1)  -> Arx.mk_rx (Safelist.fold_left (fun acc (_,ri) -> Rx.mk_seq acc (Rx.mk_star ri)) Rx.epsilon rs1)
          | Merge r            -> Arx.mk_rx r
          | Fiat ml            -> avtype ml
          | Permute (p, mls)   ->
              let _, _, s2, _, _ = p in
              Array.fold_left
                (fun acc i -> Arx.mk_seq acc (avtype mls.(i)))
                (Arx.mk_rx Rx.epsilon) s2
        in
        ml.avtype <- Some at;
        at

  and ktype ml =
    match ml.ktype with
    | Some kt -> kt
    | None ->
        let kt =
          let basic () = K_regexp (stype ml) in
          match ml.desc with
          | Copy _
          | Clobber _
          | Align _
          | Invert _
          | Default _
          | Dup1 _
          | Dup2 _
          | Partition _
          | Merge _
          | Fiat _
            -> basic ()
          | Weight (_, ml)
          | LeftQuot (_, ml)
          | RightQuot (ml, _)
            -> ktype ml
          | Concat (ml1, ml2) -> K_concat (ktype ml1, ktype ml2)
          | Union (ml1, ml2) -> K_union (ktype ml1, ktype ml2)
          | Compose (ml1, ml2) -> K_compose (ktype ml1, ktype ml2)
          | Star ml -> K_star (ktype ml)
          | Match (t, _) -> K_box t
          | Permute (_, mls) ->
              K_permute (Array.to_list (Array.map ktype mls))
        in
        ml.ktype <- Some kt;
        kt

  and mtype ml =
    match ml.mtype with
    | Some mt -> mt
    | None ->
        let mt =
          let basic = TmA.empty in
          match ml.desc with
          | Copy _
          | Clobber _
          | Partition _
          | Merge _
          | Align _
          | Fiat _
          | Invert _
          | Default _
          | Dup1 _
          | Dup2 _
            -> basic
          | Concat (ml1, ml2)
          | Union (ml1, ml2)
            -> mt_merge (mtype ml1) (mtype ml2)
          | Star ml
          | Weight (_, ml)
            -> (mtype ml)
          | Match (t, ml)
            -> TmA.add t (ktype ml) (mtype ml)
          | Compose (ml1, ml2)
            -> mt_compose (mtype ml1) (mtype ml2)
          | LeftQuot (_, ml)
          | RightQuot (ml, _)
            -> (mtype ml)
          | Permute (_, mls) ->
              Array.fold_left (
                fun acc mli -> mt_merge acc (mtype mli)
              ) TmA.empty mls
        in
        ml.mtype <- Some mt;
        mt

  and sequiv ml = match ml.sequiv with
    | Some cr -> cr
    | None ->
        let cr = match ml.desc with
          | Copy (r)           -> Identity
          | Clobber (r, w, f)  -> Identity
          | Concat (ml1, ml2)  -> equiv_merge (sequiv ml1) (sequiv ml2)
          | Union (ml1, ml2)   -> equiv_merge (sequiv ml1) (sequiv ml2)
          | Star (ml)          -> sequiv ml
          | Weight (_, ml)     -> sequiv ml
          | Match (t, ml)      -> sequiv ml
          | Compose (ml, _)    -> sequiv ml
          | Align ml           -> sequiv ml
          | Invert (ml)        -> vequiv ml
          | Default (ml, w)    -> sequiv ml
          | LeftQuot (cn, ml)  -> Unknown
          | RightQuot (ml, cn) -> sequiv ml
          | Dup1 (ml, f, r)    -> sequiv ml
          | Dup2 (f, r, ml)    -> sequiv ml
          | Partition (_,rs1)  -> Identity
          | Merge (r)          -> Identity
          | Fiat ml            -> sequiv ml
          | Permute (p, mls)   ->
              Array.fold_left
                (fun acc mli -> equiv_merge acc (sequiv mli))
                Identity mls
        in ml.sequiv <- Some cr;
        cr

  and vequiv ml = match ml.vequiv with
    | Some ar -> ar
    | None ->
        let ar = match ml.desc with
          | Copy(r1)           -> Identity
          | Clobber(r1,w1,f1)  -> Identity
          | Concat(ml1,ml2)    -> equiv_merge (vequiv ml1) (vequiv ml2)
          | Union(ml1,ml2)     -> equiv_merge (vequiv ml1) (vequiv ml2)
          | Star(ml1)          -> vequiv ml1
          | Weight (_, ml)     -> vequiv ml
          | Match (t, ml)      -> vequiv ml
          | Compose(ml1,ml2)   -> vequiv ml2
          | Align ml           -> vequiv ml
          | Invert(ml1)        -> sequiv ml1
          | Default(ml1,w1)    -> vequiv ml1
          | LeftQuot(cn1,ml1)  -> vequiv ml1
          | RightQuot(ml1,cn1) -> Unknown
          | Dup1 (ml, f, r)    -> Unknown
          | Dup2 (f, r, ml)    -> Unknown
          | Partition (_,rs1)  -> Identity
          | Merge (r)          -> Identity
          | Fiat ml            -> vequiv ml
          | Permute (p, mls)   ->
              Array.fold_left (fun acc mli -> equiv_merge acc (vequiv mli))
                Identity mls
        in ml.vequiv <- Some ar;
        ar

  (* we need to prove that:
     srep s = srep s'  <=>  s ~S s'
     vrep v = vrep v'  <=>  v ~V v'
  *)
  and srep ml s =
    match ml.desc with
    | Copy _
    | Clobber _
    | Partition _
    | Merge _
      -> Bstring.to_string s
    | Concat (ml1, ml2) ->
        let s1, s2 = Bstring.concat_split (stype ml1) (stype ml2) s in
        let r1 = srep ml1 s1 in
        let r2 = srep ml2 s2 in
        r1 ^ r2
    | Union (left, right) ->
        if Bstring.match_rx (stype left) s
        then srep left s
        else srep right s
    | Star ml ->
        let ss = Bstring.star_split (stype ml) s in
        let buf = Buffer.create 17 in
        let add = Buffer.add_string buf in
        Safelist.iter (
          fun s -> add (srep ml s)
        ) ss;
        Buffer.contents buf
    | Match (_, ml)
    | Compose (ml, _)
    | Align ml
    | RightQuot (ml, _)
    | Default (ml, _)
    | Dup1 (ml, _, _)
    | Dup2 (_, _, ml)
    | Weight (_, ml)
    | Fiat ml
      -> srep ml s
    | Invert ml -> vrep ml s
    | LeftQuot (cn, _)
      -> Canonizer.choose cn (Bstring.of_string (Canonizer.canonize cn s))
    | Permute (p, mls) ->
        let k, sigma, sigma_inv, cts, ats = p in
        let s_arr_s = arr_split_s k mls cts s in
        let r_arr_s =
          Array.mapi (fun i s -> srep mls.(i) s) s_arr_s
        in concat_array r_arr_s

  and vrep ml v =
    match ml.desc with
    | Copy _
    | Clobber _
    | Partition _
    | Merge _
      -> Bstring.to_string v
    | Concat (ml1, ml2) ->
        let v1, v2 = Bstring.concat_ambiguous_split 0 (vtype ml1) (vtype ml2) v in
        let r1 = vrep ml1 v1 in
        let r2 = vrep ml2 v2 in
        r1 ^ r2
    | Union (left, right) ->
        (* this should be true, because in the intersection, the
        relations match and we always take the left representative *)
        if Bstring.match_rx (vtype left) v
        then vrep left v
        else vrep right v
    | Star ml ->
        let vs = Bstring.star_ambiguous_split [] (vtype ml) v in
        let buf = Buffer.create 17 in
        let add = Buffer.add_string buf in
        Safelist.iter (
          fun v -> add (vrep ml v)
        ) vs;
        Buffer.contents buf
    | Match (_, ml)
    | Compose (_, ml)
    | Align ml
    | Default (ml, _)
    | LeftQuot (_, ml)
    | Weight (_, ml)
    | Fiat ml
      -> vrep ml v
    | Invert ml -> srep ml v
    | RightQuot (_, cn)
      -> Canonizer.choose cn (Bstring.of_string (Canonizer.canonize cn v))
    | Dup1 (ml, _, r)
      -> (
        let v, _ = Bstring.concat_split (vtype ml) r v in
        match Rx.representative r with
        | Some r -> vrep ml v ^ r
        | None -> assert false
      )
    | Dup2 (_, r, ml)
      -> (
        let _, v = Bstring.concat_split r (vtype ml) v in
        match Rx.representative r with
        | Some r -> r ^ vrep ml v
        | None -> assert false
      )
    | Permute (p, mls) ->
        let k, sigma, sigma_inv, cts, ats = p in
        let v_arr_v = arr_split_v k mls sigma_inv ats v in
        let r_arr_v = Array.create k "" in
        Array.iteri (
          fun i j ->
            r_arr_v.(j) <- vrep mls.(i) v_arr_v.(j)
        ) sigma;
        concat_array r_arr_v

  and gperm ml s pi = (* returns pi *)
    let basic = pi in
    match ml.desc with
    | Copy _
    | Clobber _
    | Invert _
    | Default _
    | Dup1 _
    | Dup2 _
    | Partition _
    | Merge _
    | Align _
    | Fiat _
      -> basic
    | Concat (ml1, ml2) ->
        let s1, s2 = Bstring.concat_split (stype ml1) (stype ml2) s in
        gperm ml2 s2 (gperm ml1 s1 pi)
    | Union (left, right) ->
        if Bstring.match_rx (stype left) s
        then gperm left s pi
        else gperm right s pi
    | Star ml ->
        let ss = Bstring.star_split (stype ml) s in
        Safelist.fold_left (
          fun pi s -> gperm ml s pi
        ) pi ss
    | Match (tag, ml) ->
        let p, i = pi in
        let pos = TmI.find tag i in
        let pi = P.add tag (pos, pos) p, TmI.incr tag i in
        gperm ml s pi
    | Compose (ml1, ml2) ->
        let p, i = pi in
        let p1, i1 = gperm ml1 s (P.empty, i) in
        let u = rget ml1 s in
        let u = Bstring.of_string u in
        let p2, i2 = gperm ml2 u (P.empty, i) in
        assert (TmI.equal i1 i2);
        P.compose p p2 p1, i2
    | LeftQuot (cn, ml) ->
        let p, i = pi in
        let p1, i1 = Canonizer.gperm cn s (P.empty, i) in
        let u = Canonizer.canonize cn s in
        let u = Bstring.of_string u in
        let p2, i2 = gperm ml u (P.empty, i) in
        assert (TmI.equal i1 i2);
        P.compose p p2 p1, i2
    | RightQuot (ml, cn) ->
        let p, i = pi in
        let p1, i1 = gperm ml s (P.empty, i) in
        let u = rget ml s in
        let u = Bstring.of_string u in
        let p2, i2 = Canonizer.gperm cn u (P.empty, i) in
        let p2 = P.inv p2 in
        assert (TmI.equal i1 i2);
        P.compose p p2 p1, i2
    | Weight (_, ml)
      -> gperm ml s pi
    | Permute (p, mls) ->
        let k, sigma, sigma_inv, cts, ats = p in
        let s_arr_s = arr_split_s k mls cts s in
        let pi_arr_s =
          Array.mapi (
            fun i si -> gperm mls.(i) si (P.empty, TmI.empty)
          ) s_arr_s
        in
        let _, i = pi in
        let s_shift = Array.create k i in
        let v_shift = Array.create k i in
        for i = 0 to k - 1 do
          let (_, shift) = pi_arr_s.(i) in
          for j = i + 1 to k - 1 do
            s_shift.(j) <- TmI.plus s_shift.(j) shift
          done;
          for j = sigma.(i) + 1 to k - 1 do
            v_shift.(j) <- TmI.plus v_shift.(j) shift
          done
        done;
        let _, pi =
          Array.fold_left (
            fun (j, (p, i)) (pj, ij) ->
              succ j, (
                P.shift pj s_shift.(j) v_shift.(sigma.(j)) p,
                TmI.plus i ij)
          ) (0, pi) pi_arr_s
        in pi

  (* ((v, c), ri) = gget ml s (r, i)
     . [s] is in the stype of [ml].
     . [i] is used to determine the position of the chunks.  It counts
     how many chunk we read until now for each tag.
     . [r] contains the resources of the previous chunks.
     . [(r, i)] is chained through the calls to [gget], and is only
     directly modified in [Match], [Compose] and canonizer related
     lenses.
     . [v] = ml.get [s]
     . [(c, _)] = ml.res [s] (it does not hold for [r] because it's
     chained and there is an offset of [i])
  *)
  and gget ml s ri = (* returns ((v, c), ri) *)
    let basic f = (f s, C_string s), ri in
    let basic_no_op ml = basic (rget ml) in
    let no_op ml = gget ml s ri in
    match ml.desc with
    | Copy _ ->
        basic (fun s -> Bstring.to_string s)
    | Clobber (_, w, _) -> basic (fun _ -> w)
    | Concat (ml1, ml2) ->
        let s1, s2 = Bstring.concat_split (stype ml1) (stype ml2) s in
        let (v1, c1), ri = gget ml1 s1 ri in
        let (v2, c2), ri = gget ml2 s2 ri in
        let v = v1 ^ v2 in
        let n = Bstring.find_concat_split (vtype ml1) (vtype ml2) (String.length v1) v in
        (v, C_concat (c1, n, c2)), ri
    | Union (left, right) ->
        if Bstring.match_rx (stype left) s
        then
          let (v, c), ri = gget left s ri in
          (v, C_union (Union_left, c)), ri
        else
          let (v, c), ri = gget right s ri in
          (v, C_union (Union_right, c)), ri
    | Star ml ->
        let buf = Buffer.create 17 in
        let ss = Bstring.star_split (stype ml) s in
        let ns, cs, ri =
          Safelist.fold_left (
            fun (ns, cs, ri) s ->
              let (v, c), ri = gget ml s ri in
              Buffer.add_string buf v;
              (String.length v::ns, c::cs, ri)
          ) ([], [], ri) ss
        in
        let v = Buffer.contents buf in
        let ns = Bstring.find_star_split (vtype ml) (Safelist.rev ns) v in
        (v, C_star (Safelist.rev cs, ns)), ri
    | Match (tag, ml) ->
        let r, i = ri in
        let pos = TmI.find tag i in
        let ri = r, TmI.incr tag i in
        let (v, c), (r, i) = gget ml s ri in
        let ri = TmImA.add tag pos c r, i in
        (v, C_box), ri
    | Compose (ml1, ml2) ->
(*         print_endline "+++gget for Compose"; *)
        let r, i = ri in
        let (u, uc), (ur, iu) = gget ml1 s (TmImA.empty, i) in
        let u = Bstring.of_string u in
        let (v, vc), (vr, iv) = gget ml2 u (TmImA.empty, i) in
        let p, ip = gperm ml2 u (P.empty, i) in
        assert (TmI.equal ip iv);
(*         Balign.print_res print_complement c; *)
(*         Balign.print_res print_complement ur; *)
(*         Balign.print_res print_complement vr; *)
        let ur = Balign.res_compose_perm ur (P.inv p) in
(*         Balign.print_res print_complement ur; *)
        assert (TmI.equal iu iv);
        let r = Balign.res_zip (fun (x, y) -> C_compose (x, y)) r ur vr in
(*         Balign.print_res print_complement r; *)
(*         print_endline "---gget for Compose"; *)
        (v, C_compose (uc, vc)), (r, iv)
    | Weight (_, ml) -> no_op ml
    | Align ml -> basic_no_op ml
    | Invert ml -> basic (fun s -> rcreate ml s)
    | Default (ml, _) -> basic_no_op ml
    | LeftQuot (cn, ml) ->
        let u = Canonizer.canonize cn s in
        let u = Bstring.of_string u in
        gget ml u ri
    | RightQuot (ml, cn) ->
(*         print_endline "+++gget for RightQuot"; *)
        let r, i = ri in
(*         Balign.print_res print_complement r; *)
        let (u, c), (r, i1) = gget ml s ri in
(*         Balign.print_res print_complement r; *)
        let u = Bstring.of_string u in
        let v = Canonizer.choose cn u in
        let p, i2 = Canonizer.gperm cn (Bstring.of_string v) (P.empty, i) in
(*         Balign.print_perm p; *)
        assert (TmI.equal i1 i2);
        let r = Balign.res_compose_perm r p in
(*         Balign.print_res print_complement r; *)
(*         print_endline "---gget for RightQuot"; *)
        (v, c), (r, i2)
    | Dup1 (ml, f, _) -> basic (fun s -> rget ml s ^ f (Bstring.to_string s))
    | Dup2 (f, _, ml) -> basic (fun s -> f (Bstring.to_string s) ^ rget ml s)
    | Partition (k,rs1) -> basic (
        fun s ->
          let a = Array.init k (fun _ -> Buffer.create 17) in
          let find_match s =
            try fst (Safelist.find (fun (i,ri) -> Bstring.match_rx ri s) rs1)
            with Not_found ->
              Err.run_error (Info.M "partition.gget")
                (fun () -> msg "@[%s@ did not match@ any@ regexp@]" (Bstring.to_string s)) in
          let add i s = Buffer.add_string a.(i) (Bstring.to_string s) in
          let concat_buf_array a =
            let buf = Buffer.create 17 in
              Array.iter (fun b -> Buffer.add_buffer buf b) a;
              Buffer.contents buf in
          Safelist.iter
            (fun s -> add (find_match s) s)
            (Bstring.star_split (stype ml) s);
          concat_buf_array a)
    | Merge r ->
        basic (
          fun s ->
            let s1,s2 = Bstring.concat_split r r s in
            Bstring.to_string s1)
    | Fiat ml -> basic_no_op ml
    | Permute (p, mls) ->
        let k, sigma, sigma_inv, cts, ats = p in
        let s_arr_s = arr_split_s k mls cts s in
        let vcri_arr_s =
          Array.mapi (
            fun i si -> gget mls.(i) si (TmImA.empty, TmI.empty)
          ) s_arr_s
        in
        let vcri_arr_v = arr_permute vcri_arr_s sigma in
        let ri_arr_v = Array.map snd vcri_arr_v in
        let vc_arr_v = Array.map fst vcri_arr_v in
        let _, i = ri in
        let v_shift = Array.create k i in
        for i = 0 to k - 1 do
          let (_, shift) = ri_arr_v.(i) in
          for j = i + 1 to k - 1 do
            v_shift.(j) <- TmI.plus v_shift.(j) shift
          done
        done;
        let _, ri =
          Array.fold_left (
            fun (j, (r, i)) (rj, ij) ->
              succ j, (
                TmImA.append rj v_shift.(j) r,
                TmI.plus i ij)
          ) (0, ri) ri_arr_v
        in
        let v_arr_v = Array.map fst vc_arr_v in
        let c_list_v = Array.fold_right (
          fun (_, c) l -> c::l
        ) vc_arr_v [] in
        (concat_array v_arr_v, C_list c_list_v), ri

  (* (s, ri) = gput' ml (v, co) (r, i)
     . [v] is in the vtype of [ml].
     . [i] is used to determine the position of the chunks.  It counts
     how many chunk we read until now for each tag.
     . [r] is global, in the sense that we never modify it, it's
     read-only, and passed through all calls.  We read it only in
     [Match], [Compose] and canonizer related lenses.
     . [i] is chained through the calls to [gput'], and is only
     directly modified in [Match], [Compose] and canonizer related
     lenses.
     . [s] = ml.put' [v] ([co], [r - i]) (it's [r] shifted by [i])
  *)
  and gput ml (v, c) = gput' ml (v, Some c)
  and gcreate ml v = gput' ml (v, None)
  and gput' ml (v, co) ri = (* returns (s, ri) *)
    let basic f =
      let so =
        match co with
        | Some (C_string c) -> Some c
        | None -> None
        | _ -> assert false
      in
      f v so, ri
    in
    let basic_no_op ml = basic (rput' ml) in
    let no_op ml = gput' ml (v, co) ri in
    match ml.desc with
    | Copy _
      -> basic (fun v _ -> Bstring.to_string v)
    | Clobber (_, _, f) -> basic
        (fun v so ->
           match so with
           | Some s -> Bstring.to_string s
           | None -> f (Bstring.to_string v))
    | Concat (ml1, ml2) ->
        let co1, n, co2 =
          match co with
          | Some (C_concat (c1, n, c2)) -> Some c1, n, Some c2
          | None -> None, 0, None
          | _ -> assert false
        in
        let v1, v2 = Bstring.concat_ambiguous_split n (vtype ml1) (vtype ml2) v in
        let s1, ri = gput' ml1 (v1, co1) ri in
        let s2, ri = gput' ml2 (v2, co2) ri in
        s1 ^ s2, ri
    | Union (left, right) ->
        let side, co =
          match co with
          | Some (C_union (s, k)) -> s, Some k
          | None -> Union_left, None
          | _ -> assert false
        in
        let ml, co =
          match
            Bstring.match_rx (vtype left) v,
            Bstring.match_rx (vtype right) v,
            side with
              (* the cases when we can use the complement *)
            | true , _    , Union_left  -> left, co
            | _    , true , Union_right -> right, co
              (* the cases when we can NOT use the complement *)
            | true , false, Union_right -> left, None
            | false, true , Union_left  -> right, None
            | _ -> assert false
        in
        gput' ml (v, co) ri
    | Star ml ->
        let cs, ns =
          match co with
          | Some (C_star (cs, ns)) -> cs, ns
          | None -> [], []
          | _ -> assert false
        in
        let buf = Buffer.create 17 in
        let rec loop vs cs ri =
          match vs with
          | [] -> ri
          | v::vs ->
              let co, cs =
                match cs with
                | [] -> None, []
                | c::cs -> Some c, cs
              in
              let s, ri = gput' ml (v, co) ri in
              Buffer.add_string buf s;
              loop vs cs ri
        in
        let ri = loop (Bstring.star_ambiguous_split ns (vtype ml) v) cs ri in
        Buffer.contents buf, ri
    | Match (tag, ml) ->
        let r, i = ri in
        let pos = TmI.find tag i in
        let co, r = TmImA.next tag pos r in
        let ri = r, TmI.incr tag i in
        gput' ml (v, co) ri
    | Compose (ml1, ml2) ->
(*         print_endline "+++gput' for Compose"; *)
        let co1, co2 =
          match co with
          | Some (C_compose (c1, c2)) -> Some c1, Some c2
          | None -> None, None
          | _ -> assert false
        in
        let r, i = ri in
        let len = Bstring.at_to_locs (Arx.parse (avtype ml2) v) in
        let r1, r2, r = Balign.res_unzip (
          fun x ->
            match x with
            | C_compose (a, b) -> a, b
            | _ -> assert false
        ) r i len in
(*         Balign.print_res print_complement r1; *)
(*         Balign.print_res print_complement r2; *)
(*         Balign.print_res print_complement r; *)
        let u, (r2, i2) = gput' ml2 (v, co2) (r2, i) in
        let u = Bstring.of_string u in
        let p2, i3 = gperm ml2 u (P.empty, i) in
(*         Balign.print_perm p2; *)
        assert (TmI.equal i2 i3);
        let r1 = Balign.res_compose_perm r1 p2 in
(*         Balign.print_res print_complement r1; *)
        let s, (r1, i1) = gput' ml1 (u, co1) (r1, i) in
        assert (TmI.equal i1 i2);
        assert (TmImA.is_empty r1);
        assert (TmImA.is_empty r2);
(*         print_endline "---gput' for Compose"; *)
        s, (r, i1)
    | Weight (_, ml) -> no_op ml
    | Align ml -> basic_no_op ml
    | Invert ml -> basic (fun v _ -> rget ml v)
    | Default (ml, w) -> basic (
        fun v so ->
          match so with
          | Some s -> rput ml v s
          | None -> rput ml v w)
    | LeftQuot (cn, ml) ->
        let u, ri = gput' ml (v, co) ri in
        let u = Bstring.of_string u in
        let s = Canonizer.choose cn u in
        s, ri
    | RightQuot (ml, cn) ->
(*         print_endline "+++gput' for RightQuot"; *)
        let u = Canonizer.canonize cn v in
        let u = Bstring.of_string u in
        let r, i = ri in
(*         Balign.print_res print_complement r; *)
        let p, iu = Canonizer.gperm cn v (P.empty, i) in
(*         Balign.print_perm p; *)
        let r = Balign.res_compose_perm r (P.inv p) in
        let s, ri = gput' ml (u, co) (r, i) in
(*         print_endline "---gput' for RightQuot"; *)
        s, ri
    | Dup1 (ml, f, r) -> basic
        (fun v so ->
           let v, _ = Bstring.concat_split (vtype ml) r v in
           rput' ml v so)
    | Dup2 (f, r, ml) -> basic
        (fun v so ->
           let _, v = Bstring.concat_split r (vtype ml) v in
           rput' ml v so)
    | Partition (k,rs1) -> basic (
        fun v so ->
          let ss = match so with
            | Some s -> Bstring.star_split (stype ml) s
            | None -> [] in
          let a = Array.create k [] in
          let rec vloop i rs vs = match rs,vs with
            | _,[] -> ()
            | (_,rh)::rt,vh::vt ->
                if Bstring.match_rx rh vh then
                  (a.(i) <- vh::a.(i);
                   vloop i rs vt)
                else vloop (succ i) rt vs
            | [],_ ->
                Err.run_error (Info.M "partition.gput")
                  (fun () -> msg "@[[%s]@ did not match@ any@ regexps@]"
                     (Misc.concat_list "," (Safelist.map Bstring.to_string vs))) in
          vloop 0 rs1 (Bstring.star_split (stype ml) v);
          Array.iteri (fun i l -> a.(i) <- Safelist.rev a.(i)) a;
          let buf = Buffer.create 17 in
          let find_match s =
            try Safelist.find (fun (i,ri) -> Bstring.match_rx ri s) rs1
            with Not_found ->
              Err.run_error (Info.M "partition.gput")
                (fun () -> msg "@[%s@ did not match@ any@ regexp@]" (Bstring.to_string s)) in
          let add v = Buffer.add_string buf (Bstring.to_string v) in
          let rec sloop ss = match ss with
            | sh::st ->
                let i,_ = find_match sh in
                  (match a.(i) with
                     | vi::vt -> a.(i) <- vt; add vi
                     | _ -> ());
                  sloop st
            | _ -> Array.iter (fun l -> Safelist.iter add l) a in
          sloop ss;
          Buffer.contents buf)
    | Merge r -> basic
        (fun v so ->
           let v = Bstring.to_string v in
           v ^
           match so with
           | Some s ->
               let s1, s2 = Bstring.concat_split r r s in
               let s1, s2 = Bstring.to_string s1, Bstring.to_string s2 in
               if s1 = s2 then v else s2
           | None -> v)
    | Fiat ml -> basic (
        fun v so ->
          match so with
          | None -> rcreate ml v
          | Some s ->
              if rget ml s = Bstring.to_string v
              then Bstring.to_string s
              else rput ml v s
      )
    | Permute (p, mls) ->
        let k, sigma, sigma_inv, cts, ats = p in
        let v_arr_v = arr_split_v k mls sigma_inv ats v in
        let co_arr_v =
          match co with
          | Some (C_list c) -> Array.map (fun x -> Some x) (Array.of_list c)
          | None -> Array.create k None
          | _ -> assert false
        in
        let s_arr_s = Array.create k "" in
        let rec loop j ri =
          if j >= k then ri
          else (
            let i = sigma_inv.(j) in
            (* we do the put's in view order *)
            let si, ri = gput' mls.(i) (v_arr_v.(j), co_arr_v.(j)) ri in
            s_arr_s.(i) <- si;
            loop (succ j) ri
          )
        in
        let ri = loop 0 ri in
        let s = concat_array s_arr_s in
        s, ri

  (* these are the definitions for lower *)

  and rcreate ml (v:Bstring.t) =
    let ko = None in
    let ci = (TmImA.empty, TmI.empty) in
    fst (gput' ml (v, ko) ci)

  and rput ml v' (s:Bstring.t) =
(*     print_endline "+++rput"; *)
    let vparse v = Bstring.at_to_chunktree (Arx.parse (avtype ml) (Bstring.of_string (vrep ml v))) in
    let align = Balign.align Bcost.infinite in
    let (v, k), (r, _) = gget ml s (TmImA.empty, TmI.empty) in
(*     print_endline ("v = \"" ^ vrep ml (Bstring.of_string v) ^ "\" from \"" ^ v ^ "\""); *)
(*     print_endline ("v' = \"" ^ vrep ml v'  ^ "\" from \"" ^ Bstring.to_string v' ^ "\""); *)
(*     Balign.print_res print_complement r; *)
    let g = align (vparse v') (vparse (Bstring.of_string v)) G.empty in
(*     G.print g; *)
    (match G.to_error_option g with
     | Some e -> Err.run_error (Info.M "Blenses.MLens.rput") e
     | None -> ()
    );
    let r = Balign.align_compose_res r g in
(*     Balign.print_res print_complement r; *)
    let s = fst (gput ml (v', k) (r, TmI.empty)) in
(*     print_endline "---rput"; *)
    s

  and rput' ml v' so =
    match so with
    | Some s -> rput ml v' s
    | None -> rcreate ml v'

  and rget ml (s:Bstring.t) =
    let cbi = (TmImA.empty, TmI.empty) in
    fst (fst (gget ml s cbi))

  (* helpers for permute *)
  and arr_split_v k mls sigma_inv ats v =
    let res = Array.create k Bstring.empty in
    let _ =
      Array.fold_left (
        fun (i, v) j ->
          let vi, vrest =
            Bstring.concat_split (vtype mls.(j)) ats.(i) v
          in
          res.(i) <- vi;
          (succ i, vrest)
      ) (0, v) sigma_inv
    in res (* in view order *)

  and arr_split_s k mls cts s =
    let res = Array.create k Bstring.empty in
    let _ =
      Array.fold_left (
        fun (j, s) mlj ->
          let sj, srest =
            Bstring.concat_split (stype mlj) cts.(j) s
          in
          res.(j) <- sj;
          (succ j, srest)
      ) (0, s) mls
    in res (* in source order *)

  and arr_permute arr sigma =
    let res = Array.copy arr in
    let _ =
      Array.iteri (
        fun i j -> res.(j) <- arr.(i)
      ) sigma
    in res

  let info ml = ml.info

  let rec format_t ml =
    let fmt_s s = msg "\"%s\"" s in
    msg "@[";
    begin match ml.desc with
    | Copy(r1)           -> msg "(copy@ "; Rx.format_t r1; msg ")"
    | Clobber(r1,w1,f1)  -> msg "(clobber@ "; Rx.format_t r1; msg "@ "; fmt_s w1; msg "@ <function>"; msg ")"
    | Concat(dl1,dl2)    -> msg "("; format_t dl1; msg "@ .@ "; format_t dl2; msg ")"
    | Union(dl1,dl2)     -> msg "("; format_t dl1; msg "@ |@ "; format_t dl2; msg ")"
    | Star(dl1)          -> format_t dl1; msg "* " (* space to prevent spurious close-comments *)
    | Weight (bw, ml) -> msg "{:%s:" (W.to_forcestring bw); format_t ml; msg "}"
    | Match (tag, ml) -> msg "<%s:" (T.to_string tag); format_t ml; msg ">"
    | Compose(dl1,dl2)   -> msg "("; format_t dl1; msg "@ ;@ "; format_t dl2; msg ")"
    | Align ml           -> msg "(align@ "; format_t ml; msg ")"
    | Invert (ml)        -> msg "(invert@ "; format_t ml; msg ")"
    | Default (ml, w)    -> msg "(default@ "; format_t ml; msg "@ "; fmt_s (Bstring.to_string w); msg ")"
    | LeftQuot(cn1,dl1)  -> msg "(left_quot@ "; Canonizer.format_t cn1; msg "@ "; format_t dl1; msg ")"
    | RightQuot(dl1,cn1) -> msg "(right_quot@ "; format_t dl1; msg "@ "; Canonizer.format_t cn1; msg ")"
    | Dup1(dl1,f1,r1)    -> msg "(dup1@ "; format_t dl1; msg "@ <function>@ "; Rx.format_t r1; msg ")"
    | Dup2(f1,r1,dl1)    -> msg "(dup2@ <function>@ "; Rx.format_t r1; msg "@ "; format_t dl1; msg ")"
    | Partition (_,rs1)  -> msg "(partition@ "; Misc.format_list ",@ " (fun (_,ri) -> Rx.format_t ri) rs1; msg ")"
    | Merge(r1)          -> msg "(merge@ "; Rx.format_t r1; msg ")"
    | Fiat(dl1)          -> msg "(fiat@ "; format_t dl1; msg ")"
    | Permute((_,is1,is2,rs1,rs2),dls) ->
        let format_array sep fmt a =
          let rec loop i n =
            if i <> n
            then (
              fmt a.(i);
              if succ i <> n then msg sep;
              loop (succ i) n
            )
          in
          loop 0 (Array.length a)
        in
        msg "(permute@ #{int}[@[";
        format_array ",@ " (fun i -> msg "%d" i) is2;
        msg "@]]@ #{lens}[@[";
        format_array ",@ " format_t dls;
        msg "@]])"
    end;
    msg "@]"

  let string_of_t ml =
    Util.format_to_string
      (fun () ->
         Util.format "@[";
         format_t ml;
         Util.format "@]")

  let vequiv_identity dl = vequiv dl = Identity

  let sequiv_identity dl = sequiv dl = Identity

  (* ----- constructors ----- *)
  let clobber i r1 w1 f1 = mk i (Clobber(r1,w1,f1))
  let concat i ml1 ml2 = mk i (Concat (ml1, ml2))
  let union i ml1 ml2 = mk i (Union (ml1, ml2))
  let star i ml = mk i (Star ml)
  let copy i r = mk i (Copy r)
  let weight i b w ml = mk i (Weight ((b, w), ml))
  let mmatch i tag ml = mk i (Match (tag, ml))
(*   let copy_arx i a = *)
(*     let leaf l (wc, we, wd) r = *)
(*       let c = copy i r in *)
(*       let k = Bannot.LeafSet.fold (fun Bannot.Leaf.Key _ -> key i r) l c in *)
(*       let wo w = Bannot.Weight.option_to_string w in *)
(*       weight i (wo wc) (wo we) (wo wd) k *)
(*     in *)
(*     let node n ml = *)
(*       match n with *)
(*       | Bannot.Node.Chunk tag -> mmatch i (Btag.to_string tag) ml *)
(*       | Bannot.Node.Forgetkey -> forgetkey i ml *)
(*     in *)
(*     let concat = concat i in *)
(*     let union = union i in *)
(*     let star = star i in *)
(*     Barx.fold leaf node concat union star a *)
  let compose i dl1 dl2 = mk i (Compose(dl1,dl2))
  let align i ml = mk i (Align ml)
  let invert i ml = mk i (Invert (ml))
  let default i ml w = mk i (Default (ml, w))
  let left_quot i cn1 dl1 = mk i (LeftQuot(cn1,dl1))
  let right_quot i dl1 cn1 = mk i (RightQuot(dl1,cn1))
  let dup1 i dl1 f1 r1 = mk i (Dup1(dl1,f1,r1))
  let dup2 i f1 r1 dl1 = mk i (Dup2(f1,r1,dl1))
  let partition i rs1 = 
    let k, irs1 = Safelist.fold_left (fun (i,acc) ri -> (succ i,(i,ri)::acc)) (0,[]) rs1 in 
    mk i (Partition(k,Safelist.rev irs1))
  let merge i r1 = mk i (Merge(r1))
  let fiat i dl1 = mk i (Fiat(dl1))
  let permute i is mls =
    let ml_arr = Array.of_list mls in
    let k = Array.length ml_arr in
    let sigma, sigma_inv = Permutations.permutation is k in
    let ats = Array.create k Rx.empty in
    let cts = Array.create k Rx.empty in
    let _ =
      Array.fold_right
        (fun j (i, acc) ->
           ats.(i) <- acc;
           (pred i, Rx.mk_seq (vtype ml_arr.(j)) acc))
        sigma_inv (pred k, Rx.epsilon) in
    let _ = Array.fold_right
      (fun mli (i, acc) ->
         cts.(i) <- acc;
         (pred i, Rx.mk_seq (stype mli) acc))
      ml_arr (pred k, Rx.epsilon) in
      mk i (Permute ((k, sigma, sigma_inv, cts, ats), ml_arr))
  let canonizer_of_t i ml =
    Canonizer.from_lens i (astype ml) (avtype ml) (vequiv ml) (mtype ml) (gperm ml) (rget ml) (rcreate ml)
  let iter i dl1 min maxo =
    Arx.generic_iter (copy i Rx.epsilon) (union i) (concat i) (star i)
      min maxo dl1
end
