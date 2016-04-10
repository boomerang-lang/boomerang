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
(* src/bannot.ml                                                              *)
(* Annotations                                                                *)
(* $Id: bannot.ml 4901 2010-05-13 21:14:49Z cretin $ *)
(******************************************************************************)

module Weight = struct
  type t = NoKey | Key
  let zero = NoKey
  let one = Key
  let of_int = function
    | 0 -> zero
    | 1 -> one
    | _ -> assert false
  let to_int = function
    | NoKey -> 0
    | Key -> 1
  let succ_int t i = i + (to_int t)
  let weight_int t i = i * (to_int t)
  let of_string s =
    try of_int (int_of_string s)
    with Failure _ -> assert false
  let to_string = function
    | NoKey -> "NoKey"
    | Key -> "Key"
  let to_forcestring (b, t) =
    (if b then "!" else "") ^ (to_string t)
  let equiv a b = a = b
end

module Ss = Set.Make (String)

