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
(* src/btag.ml                                                                *)
(* Tags                                                                       *)
(* $Id: btag.ml 4636 2009-08-26 19:28:59Z cretin $ *)
(******************************************************************************)

module ImA = Intmapa
module W = Bannot.Weight

type species =
  | Positional
  | Diffy of bool (* if put first *)
  | Greedy
  | Setlike

type predicate =
  | Threshold of int

type taginfo = species * predicate list * W.t
type tagname = string
type tag = taginfo * tagname
type t = tag

let equiv a b = a = b

let species_string = [
  Positional, "p";
  Diffy true, "df";
  Diffy false, "dl";
  Greedy, "g";
  Setlike, "s";
]

let key_through _ = false

let string_species =
  let a, b = Safelist.split species_string in
  Safelist.combine b a

let species_of_string s =
  try Safelist.assoc s string_species
  with Not_found -> Berror.run_error (Info.M "species_of_string") (
    fun () -> Util.format "'%s' is an invalid species." s
  )

let species_to_string t =
  Safelist.assoc t species_string

let break c s =
  try
    let i = String.index s c in
    let n = String.length s in
    String.sub s 0 i, Some (String.sub s (i + 1) (n - i - 1))
  with
  | Not_found -> s, None

let predicate_of_string p =
  try
    match break '%' p with
    | t, Some "" -> Threshold (int_of_string t)
    | _ -> raise (Failure "")
  with Failure _ -> Berror.run_error (Info.M "predicate_of_string") (
    fun () -> Util.format "'%s' is an invalid predicate." p
  )

let predicate_to_string p =
  match p with
  | Threshold t -> string_of_int t ^ "%"

let get_species ((s, _, _), _) = s

let get_predicates ((_, ps, _), _) = ps

let get_weight ((_, _, w), _) = w

let get_name (_, n) = n

let of_elements s pl w n = (s, pl, w), n

let to_elements ((s, p, w), n) = s, p, w, n

let predicate_list_to_string l =
  Safelist.fold_left (
    fun s p ->
      s ^ predicate_to_string p
  ) "" l

let to_string ((s, p, w), i) =
  "Tag(" ^ species_to_string s ^ ",[" ^ predicate_list_to_string p ^ "]," ^ W.to_string w ^ ",\"" ^ String.escaped i ^ "\")"

let format_t t = Util.format "@[%s@]" (to_string t)

let taginfo (i, _) = i

let predicates_equiv a b =
  let canonize p =
    let rec f p t =
      match p with
      | [] -> t
      | Threshold x::p -> f p (max x t)
    in
    f p 0
  in
  canonize a = canonize b

let info_equiv (sa, pa, wa) (sb, pb, wb) =
  sa = sb && predicates_equiv pa pb && W.equiv wa wb

module N =
struct
  type t = tagname
  let compare = compare
end

module NmA = Map.Make (N)

module MapA =
struct
  type 'a t = (taginfo * 'a) NmA.t
  let empty = NmA.empty
  let is_empty t = NmA.is_empty t
  let compatible (i, n) t = info_equiv i (fst (NmA.find n t))
  let addable (i, n) t = not (NmA.mem n t) || compatible (i, n) t
  let add (i, n) a t = assert (addable (i, n) t); NmA.add n (i, a) t
  let find (_, n) t = snd (NmA.find n t)
  let mem (_, n) t = NmA.mem n t
  let iter f t = NmA.iter (fun n (i, a) -> f (i, n) a) t
  let map f t = NmA.map (fun (i, a) -> i, f a) t
  let mapi f t = NmA.mapi (fun n (i, a) -> i, f (i, n) a) t
  let fold f t b = NmA.fold (fun n (i, a) b -> f (i, n) a b) t b
  let equal eq ta tb = NmA.equal (fun (_, a) (_, b) -> eq a b) ta tb
end

module T =
struct
  type t = tag
  let compare = compare
end

module Set =
struct
  type t = taginfo NmA.t
  let empty = NmA.empty
  let is_empty t = NmA.is_empty t
  let compatible (i, n) t = info_equiv i (NmA.find n t)
  let addable (i, n) t = not (NmA.mem n t) || compatible (i, n) t
  let add (i, n) t = assert (addable (i, n) t); NmA.add n i t
  let union ta tb = NmA.fold (fun n i t -> add (i, n) t) ta tb
  let fold f t a = NmA.fold (fun n i a -> f (i, n) a) t a
end

module NmAl = Amapblist.Make (N)

module MapAList =
struct
  type 'a t = 'a NmAl.t * taginfo NmA.t
  let empty = NmAl.empty, NmA.empty
  let compatible (i, n) (_, ts) = info_equiv i (NmA.find n ts)
  let addable (i, n) (tl, ts) = not (NmA.mem n ts) || compatible (i, n) (tl, ts)
  let add (i, n) a (tl, ts) =
    assert (addable (i, n) (tl, ts));
    NmAl.add n a tl, NmA.add n i ts
  let add_list (i, n) al (tl, ts) =
    assert (addable (i, n) (tl, ts));
    NmAl.add_list n al tl, NmA.add n i ts
  let fold_list f (tl, ts) b =
    NmAl.fold_list (
      fun n al b -> f (NmA.find n ts, n) al b
    ) tl b
  let find_list (i, n) (tl, _) = NmAl.find_list n tl
  let iter_list f (tl, ts) =
    NmAl.iter_list (
      fun n al -> f (NmA.find n ts, n) al
    ) tl
  let rev (tl, ts) = NmAl.rev tl, ts
  let print_list ft fa (tl, ts) =
    NmAl.print_list (
      fun n -> ft (NmA.find n ts, n)
    ) fa tl
end

module MapInt =
struct
  type t = int MapA.t
  let empty = MapA.empty
  let find tag m =
    try MapA.find tag m
    with Not_found -> 0
  let incr tag m =
    MapA.add tag (succ (find tag m)) m
  let plus m n =
    MapA.fold (
      fun t i mn ->
        if MapA.mem t m then mn
        else MapA.add t i mn
    ) n (MapA.mapi (fun t -> (+) (find t n)) m)
  let le m1 m2 =
    MapA.fold (fun t i b -> b && (i <= find t m2)) m1 true
  let equal m1 m2 =
    le m1 m2 && le m2 m1
  let fold step m acc =
    MapA.fold step m acc
  let of_TmA tma = tma
end

module MapIntMapA =
struct
  type 'a t = ('a ImA.t) MapA.t
  let empty = MapA.empty
  let is_empty m =
    MapA.fold (fun _ ima b -> b && ImA.is_empty ima) m true
  let find tag i m =
    ImA.find i (MapA.find tag m)
  let add tag i v m =
    try
      let ima = MapA.find tag m in
        MapA.add tag (ImA.add i v ima) m
    with Not_found -> 
        MapA.add tag (ImA.add i v ImA.empty) m
  let remove tag i m =
    try MapA.add tag (ImA.remove i (MapA.find tag m)) m
    with Not_found -> m
  let map f m =
    MapA.map (ImA.map f) m
  let foldi f m acc =
    MapA.fold (fun tag ima acc -> ImA.fold (fun i v acc -> f tag i v acc) ima acc) m acc
  let next tag i m =
    try
      let ima = MapA.find tag m in
        Some (ImA.find i ima), MapA.add tag (ImA.remove i ima) m
    with Not_found ->
      None, m
  let append m shift acc =
    MapA.fold (
      fun tag ima acc ->
        let j = MapInt.find tag shift in
        let shifted_ima =
          ImA.fold (
            fun i v acc -> ImA.add (i + j) v acc
          ) ima (
            try MapA.find tag acc
            with Not_found -> ImA.empty
          )
        in
        MapA.add tag shifted_ima acc
    ) m acc
  let add_ima tag ima m =
    MapA.add tag ima m
  let mapi_ima f m =
    MapA.mapi f m
  let find_ima tag m =
    MapA.find tag m
  let fold_ima f m acc =
    MapA.fold f m acc
  let print f g m =
    MapA.iter (
      fun tag ima ->
        f tag;
        ImA.iter g ima
    ) m
end
 

