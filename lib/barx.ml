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
(* src/barx.ml                                                                *)
(* Annotated regular expressions                                              *)
(* $Id: barx.ml 4901 2010-05-13 21:14:49Z cretin $ *)
(******************************************************************************)

module Rx = Brx
module W = Bannot.Weight
module T = Btag
module Ts = T.Set
module Err = Berror

(* ---------------------------------------------------------------------------*)
(* HELPERS *)

let sprintf = Printf.sprintf
let msg = Util.format

(* generic helper for iterating a regexp / lens / canonizer *)
let rec generic_iter epsilon union concat star min max x =
  let rec mk_cats n x = match n with
    | 0 -> epsilon
    | 1 -> x
    | 2 -> concat x x
    | _ ->
        let half_x = mk_cats (n/2) x in
        let twice_half_x = concat half_x half_x in
        if n mod 2 = 0 then twice_half_x
        else concat x twice_half_x in
  let rec mk_alts acc xi j = match j with
    | 0 -> acc
    | _ ->
        let xi1 = concat x xi in
        mk_alts (union acc xi1) xi1 (pred j) in
   match min,max with
     | (0,-1) -> star x
     | (n,-1) -> concat (mk_cats n x) (star x)
     | (0,0)  -> epsilon
     | (m,n)  ->
         let f m n =
           let m_x = mk_cats m x in
             if m=n then m_x
             else if m < n then mk_alts m_x m_x (n-m)
             else (* m > n *)
               Err.run_error (Info.M "generic_iter")
                 (fun () -> msg "@[%d greater than %d@]" m n)
         in
           if m = 0 then union epsilon (f 1 n)
           else f m n

(* ---------------------------------------------------------------------------*)
(* ANNOTATED REGEXP *)

type t =
  | Leaf of (W.t option) * Rx.t
  | Chunk of T.t * t
  | Seq of t * t
  | Alt of t * t
  | Star of t

let rxtype t =
  let rec a t =
    match t with
    | Leaf (_, r) -> r
    | Chunk (_, t) -> a t
    | Seq (t1, t2) -> Rx.mk_seq (a t1) (a t2)
    | Alt (t1, t2) -> Rx.mk_alt (a t1) (a t2)
    | Star t -> Rx.mk_star (a t)
  in
  a t

let annot_weight nw t =
  let setw (b, n) o =
    match b, n, o with
    | true, x, _
    | _, x, None
    | false, _, Some x
        -> Some x
  in
  let rec f t =
    match t with
    | Leaf (ow, x) -> Leaf (setw nw ow, x)
    | Chunk _ -> t
    | Seq (t1, t2) -> Seq (f t1, f t2)
    | Alt (t1, t2) -> Alt (f t1, f t2)
    | Star t -> Star (f t)
  in
  f t

let mk_rx r = Leaf (None, r)
let is_rx = function
    | Leaf (None, r) -> Some r
    | _ -> None
let mk_box tag t = Chunk (tag, t)
let mk_seq c1 c2 =
  match is_rx c1, is_rx c2 with
    | Some r1, Some r2 -> mk_rx (Rx.mk_seq r1 r2)
    | _ -> Seq (c1, c2)
let mk_alt c1 c2 =
  match is_rx c1, is_rx c2 with
    | Some r1, Some r2 ->mk_rx (Rx.mk_alt r1 r2)
    | _ -> Alt (c1, c2)
let mk_star c =
  match is_rx c with
    | Some r -> mk_rx (Rx.mk_iter r 0 (-1))
    | _ -> Star c
let mk_iter c i j =
  match is_rx c with
    | Some r -> mk_rx (Rx.mk_iter r i j)
    | _ ->
        generic_iter (mk_rx Rx.epsilon) mk_alt mk_seq mk_star i j c

let empty = mk_rx Rx.empty
let epsilon = mk_rx Rx.epsilon

let rec extended t = (* the regular expression w/ extended alphabet *)
  let langle =
    Rx.mk_cset [(Rx.langle_code,Rx.langle_code)]
  in
  let rangle =
    Rx.mk_cset [(Rx.rangle_code,Rx.rangle_code)]
  in
  let colon =
    Rx.mk_cset [(Rx.colon_code,Rx.colon_code)]
  in
  match t with
  | Leaf (_, r) -> r
  | Chunk (tag, t) ->
      Rx.mk_seq (
        Rx.mk_seq (
          Rx.mk_seq (
            Rx.mk_seq
              langle
              (Rx.mk_string (Btag.get_name tag))
          ) colon
        ) (extended t)
      ) rangle
  | Seq (t1, t2) -> Rx.mk_seq (extended t1) (extended t2)
  | Alt (t1, t2) -> Rx.mk_alt (extended t1) (extended t2)
  | Star t -> Rx.mk_star (extended t)

(* * Barx.format_t : Barx.t -> unit *)
let format_t t =
  Rx.format_t (extended t)

let string_of_t t =
  Rx.string_of_t (extended t)

let equiv a b =
  Rx.equiv (extended a) (extended b)

(* do the same for brx.ml? (and then here we just call brx...) *)
let equiv_cex t1 t2 =
  let r1 = extended t1 in
  let r2 = extended t2 in
    match Rx.representative (Rx.mk_diff r1 r2), Rx.representative (Rx.mk_diff r2 r1) with
      | Some w1, Some w2 ->
          Some(sprintf "'%s' and '%s' are not equivalent; [%s] is in the first but not the second and [%s] is in the second but not the first"
                 (string_of_t t1) (string_of_t t2) w1 w2)
      | Some w1, None ->
          Some(sprintf "'%s' and '%s' are not equivalent; [%s] is in the first but not the second" 
                 (string_of_t t1) (string_of_t t2) w1)
      | None, Some w2 ->
          Some(sprintf "'%s' and '%s' are not equivalent; [%s] is in the second but not the first" 
                 (string_of_t t1) (string_of_t t2) w2)
      | None, None -> None

let rec parse t s =
(*   if not (Bstring.match_rx (rxtype t) s) *)
(*   then ( *)
(*     Berror.run_error (Info.M "Barx.parse") ( *)
(*       fun () -> *)
(*         Util.format "@[%a@]@[\"%s\"@]\n" (fun _ -> Rx.format_t) (rxtype t) (Bstring.to_string s); *)
(*     ) *)
(*   ); *)
  assert (Bstring.match_rx (rxtype t) s);
  let rec p t wd s = (* wd = default weight *)
    match t with
    | Leaf (wo, _) ->
        let w =
          match wo with
          | None -> wd
          | Some w -> w
        in
        Bstring.annot_leaf w s
    | Chunk (tag, t) ->
        Bstring.annot_node tag (p t (Btag.get_weight tag) (Bstring.before_node s))
    | Seq (t1, t2) ->
        Bstring.do_concat (rxtype t1) (rxtype t2) (p t1 wd) (p t2 wd) s
    | Alt (t1, t2) -> p (
        if Bstring.match_rx (rxtype t1) (Bstring.of_attmp s)
          (* 1 <-> 2 is the same because unambiguous parsing *)
        then t1
        else t2
      ) wd s
    | Star t ->
        Bstring.do_star (rxtype t) (p t wd) s
  in
  Bstring.at_of_attmp (p t W.zero (Bstring.to_attmp s))

let drop = rxtype

let fold leaf chunk seq alt star =
  let rec f t =
    match t with
    | Leaf (w, r) -> leaf w r
    | Chunk (tag, t) -> chunk tag (f t)
    | Seq (t1, t2) -> seq (f t1) (f t2)
    | Alt (t1, t2) -> alt (f t1) (f t2)
    | Star t -> star (f t)
  in
  f

let rec to_tags = function
  | Leaf _ -> Ts.empty
  | Chunk (tag, t) -> Ts.add tag (to_tags t)
  | Seq (t1, t2) -> Ts.union (to_tags t1) (to_tags t2)
  | Alt (t1, t2) -> Ts.union (to_tags t1) (to_tags t2)
  | Star t -> to_tags t

let no_chunks t =
  Ts.is_empty (to_tags t)

let tags_match_compatible_cex tag t =
  if Ts.addable tag t
  then None
  else Some (sprintf "tagname '%s' used with non compatible taginfo (species and predicate)" (T.get_name tag))

let match_compatible_cex tag t = tags_match_compatible_cex tag (to_tags t)

let compatible_cex ta tb =
  let a = to_tags ta in
  let b = to_tags tb in
  Ts.fold (
    fun tag x ->
      match x with
      | Some _ -> x
      | None -> tags_match_compatible_cex tag b
  ) a None

(* let compatible_chunks t = *)
(*   let rec f m = function *)
(*     | Leaf _ -> m, true *)
(*     | Annot (n, tag) -> *)
(*         (try *)
(*            TmA.add tag n m, aequiv n (TmA.find tag) *)
(*          with Not_found -> *)
(*            TmA.add tag n m, true) *)
(*     | Seq (t1, t2) -> *)
(*         let m, r1 = f m t1 in *)
(*         let m, r2 = f m t2 in *)
(*           m, r1 && r2 *)
(*     | Alt (t1, t2) -> *)
(*         let m, r = f m t1 in *)
(*         let m, r2 = f m t2 in *)
(*           m, r1 && r2 *)
(*     | Star t -> f m t *)
(*   in *)
(*   snd (f TmA.empty t) *)

(* let compatible_chunks_cex t = failwith "not implemented" *)

(* let compatible t1 t2 = *)
(*   let compatible_chunks *)

(* let compatible_cex t1 t2 = failwith "not implemented" *)
