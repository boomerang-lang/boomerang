(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*                                                                             *)
(* info.ml - common definitions for reporting locations of errors              *)
(*******************************************************************************)
(* $Id: info.ml 3437 2007-12-09 22:41:31Z jnfoster $ *)

type pos = int * int
type t = I of string * pos * pos | M of string

(* string_of_t : t
 *     pretty prints a location for easy parsing in emacs compile-mode *)
let string_of_t = function
    I (fn, (l1,c1),(l2,c2)) -> 
      let f = if fn = "NOFILEHERE" then "" else "File \"" ^ fn ^ "\", " in
      if l2=l1
      then Printf.sprintf "%sline %d, characters %d-%d" f l1 c1 c2
      else Printf.sprintf "%sline %d, character %d, to line %d, character %d" f l1 c1 l2 c2
  | M s -> s

(* merge_inc : t -> t -> t
 *     merge two locations; includes the endpoints *)
let merge_inc = 
  function I (fn,pos1,_) as i1 -> 
    begin 
      function I (_,_,pos2) -> I (fn,pos1,pos2)
        | m -> i1
    end
    | m -> function I _ as i2 -> i2 | _ -> m

(* merge_exc : t -> t -> t
 *     merge two locations; excludes the endpoints *)
let merge_exc = 
  function I (fn,_,pos1) as i1 -> 
    begin 
      function I (_,pos2,_) -> I (fn,pos1,pos2)
        | _ -> i1
    end
    | m -> function I _ as i2 -> i2 | _ -> m
