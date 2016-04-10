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
(* src/bcost.ml                                                               *)
(* Costs of alignments                                                        *)
(* $Id: bcost.ml 4628 2009-08-17 20:39:00Z cretin $ *)
(******************************************************************************)

module W = Bannot.Weight

type t =
  | Finite of int
  | Infinite

let to_option t =
  match t with
  | Infinite -> None
  | Finite i -> Some i

let to_string t =
  match t with
  | Infinite -> "oo"
  | Finite i -> string_of_int i

let zero = Finite 0
let one = Finite 1

let infinite = Infinite
let is_infinite t = t = Infinite

let plus a b =
  match a, b with
  | Infinite, _
  | _, Infinite
      -> Infinite
  | Finite a, Finite b
      -> Finite (a + b)

let minus a b =
  match a, b with
    | _, Infinite -> assert false
    | Infinite, _ -> Infinite
    | Finite a, Finite b -> Finite (a - b)

let limit_minus a b =
  match a, b with
    | _, Infinite -> Finite 0
    | Infinite, _ -> Infinite
    | Finite a, Finite b -> Finite (max (a - b) 0)

let of_weighted_int w i = Finite (W.to_int w * i)

let of_int i = of_weighted_int W.one i

let succ w c =
  plus c (of_weighted_int w 1)

let min a b =
  match a, b with
  | Infinite, x
  | x, Infinite
      -> x
  | Finite a, Finite b
      -> Finite (min a b)

let lt a b =
  match a, b with
  | Infinite, _ -> false
  | Finite _, Infinite -> true
  | Finite a, Finite b ->  a < b

let le a b =
  match a, b with
  | _, Infinite -> true
  | Infinite, Finite _ -> false
  | Finite a, Finite b ->  a <= b

let compare a b =
  match a,b with
    | Finite a, Finite b -> compare a b
    | Infinite, Infinite -> 0
    | Infinite, _ -> 1
    | _, Infinite -> -1
  
let equal a b = le a b && le b a
