(******************************************************************************)
(* Copyright (C) 2007-2008                                                    *)
(* J. Nathan Foster and Benjamin C. Pierce                                    *)
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
(* /src/bheap.ml                                                              *)
(* Boomerang identifiers                                                      *)
(* $Id: bident.ml 4624 2009-08-12 16:13:36Z cretin $ *)
(******************************************************************************)

(* ------ imports and abbreviations ------ *)
let sprintf = Printf.sprintf

(* ------ identifiers ------ *)
module Id = struct
  (* identifiers: parsing information and a string *)
  type t = Info.t * string 
  (* constructor *)
  let mk i s = (i,s)
  (* accessors *)
  let info_of_t (i,_) = i
  let string_of_t (_,s) = s
  (* comparisons *)
  let compare ((_,x1):t) ((_,x2):t) = compare x1 x2
  let equal ((_,x1):t) ((_,x2):t) = x1 = x2
  (* modifiers *)
  let prime (i,x) = (i,x ^ "'")
  (* constants *)
  let wild = (Info.M "_", "_")
  type this_t = t
 (* ordered type *)
  module OQ = struct
    type t = this_t
    let compare = compare
  end
 (* sets of identifiers *)
  module Set = Set.Make(OQ)
  (* maps with identifiers as keys *)
  module Map = Map.Make(OQ)
end

(* ------ qualified identifiers ------ *)
module Qid = struct
  (* qualified identifiers: list of qualifiers, and an identifier *)
  type t = Id.t list * Id.t  
  (* constructor *)
  let mk qs x = (qs,x)
  let t_of_id x = mk [] x
  (* accessors *)
  let info_of_t q = match q with
    | ([],x) -> Id.info_of_t x
    | (x1::_,x) -> Info.merge_inc (Id.info_of_t x1) (Id.info_of_t x)
  let qs_of_t (qs,_) = qs
  let id_of_t (_,x) = x                 
  let string_of_t (qs,x) = 
    Printf.sprintf "%s%s"
      (Safelist.fold_left 
         (fun acc qi -> Printf.sprintf "%s%s." acc (Id.string_of_t qi)) 
         ""
         qs)    
      (Id.string_of_t x)
  (* comparisons *)
  let compare (qs1,x1) (qs2,x2) = 
    let rec ids_compare xs1 xs2 = match xs1,xs2 with
      | [],[] -> 0
      | _,[]  -> 1
      | [],_  -> -1
      | (x1::t1),(x2::t2) -> 
          let hd_compare = Id.compare x1 x2 in
          if (hd_compare <> 0) then hd_compare 
          else ids_compare t1 t2 in
    ids_compare (qs1@[x1]) (qs2@[x2])
  let equal q1 q2 = (compare q1 q2 = 0)
  (* modifiers *)
  let prime (qs,x) = (qs,Id.prime x)
  (* operations *)      
  let id_dot x1 (qs2,x2) = (x1::qs2,x2)
  let t_dot_id (qs1,x1) x2 = (qs1@[x1],x2)
  let parent_t (qs, _) =
    let rec f qs =
      match qs with
      | [] -> assert false
      | [q] -> [], q
      | h::t ->
          let qs, x = f t in
          h::qs, x
    in
    f qs
  let t_dot_t (qs1,x1) (qs2,x2) = (qs1@x1::qs2,x2)
  let id_prefix q1 il2 = 
    let (is1,i1) = q1 in 
    let il1 = is1 @ [i1] in
      ((Safelist.length il1) <= (Safelist.length il2)) 
      && (Safelist.for_all 
            (fun (i1,i2) -> Id.equal i1 i2)
            (Safelist.combine il1 (Misc.take (Safelist.length il1) il2)))
  (* constants *)
  let mk_mod_t i ml x = 
    let qs = Safelist.map (Id.mk i) ml in 
    (qs, Id.mk i x)
  let mk_native_prelude_t i = mk_mod_t i ["Native"; "Prelude"] 
  let mk_prelude_t i = mk_mod_t i ["Prelude"]
  let mk_core_t i = mk_mod_t i ["Core"]
  let mk_list_t i = mk_mod_t i ["List"]    
  (* environments keyed by qualified identifiers *)
  type this_t = t
  module Env = Env.Make(
    struct
      type t = this_t
      let compare = compare
      let to_string = string_of_t
    end) 
 (* ordered type *)
  module OQ = struct
    type t = this_t
    let compare = compare
  end
  (* sets of qualified identifiers *)
  module Set = Set.Make(OQ)
  (* maps with qualified identifiers as keys *)
  module Map = Map.Make(OQ)
end
