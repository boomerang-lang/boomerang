(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2008                                                         *)
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
(* /src/erx.ml                                                                *)
(* Boomerang extended regular expressions interface                           *)
(* $Id *)
(******************************************************************************)

type tag = string

module TagSet : 
  Set.S with type elt = tag

module TagMap : Mapplus.SMap with type key_t = string 
                             and type key_set = TagSet.t
             
type t

type spine 
type key = string
type box_content = (key * string) list 
type skeleton = spine * box_content TagMap.t

(* pretty printers *)
val format_t : t -> unit
val format_spine : spine -> unit
val format_box_content : box_content -> unit
val format_skeleton : skeleton -> unit
val string_of_t : t -> string
val string_of_spine : spine -> string
val string_of_box_content : box_content -> string
val string_of_skeleton : skeleton -> string

(* constructors *)
val mk_box : tag -> t -> t 
val mk_star : t -> t 
val mk_seq : t -> t -> t 
val mk_alt : t -> t -> t 
val mk_key : Brx.t -> t 
val mk_leaf : Brx.t -> t 

(* operations *)
val bare : t -> Brx.t
val boxes : t -> int
val iterable : t -> bool
val parse : t -> string -> skeleton
val unparse : skeleton -> string
val valid : skeleton -> bool
val box_content : skeleton -> tag -> box_content
val box_type : t -> tag -> t option
val spine_tags : spine -> TagSet.t
