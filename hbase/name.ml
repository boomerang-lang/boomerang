(***************************************************)
(* The Harmony Project                             *)
(* harmony@lists.seas.upenn.edu                    *)
(*                                                 *)
(* name.ml - names and structures built from names *)
(***************************************************)
(* $Id: name.ml 3437 2007-12-09 22:41:31Z jnfoster $ *)

type name = string

module NameMap =
  Mapplus.Make(
    struct
      type t = name
      let compare = compare
      let to_string n = n
    end)

type t = name
module Map = NameMap.Map
module Set = NameMap.KeySet

module Hash = Hashtbl.Make(
  struct
    type t = name
    let equal = (==)
    let hash o = Hashtbl.hash (Obj.magic o : int)
  end)

module SetHash = Hashtbl.Make(
    struct
      type t = Set.t 
      let equal = (==)
      let hash s = (Obj.magic s : int)
    end)

