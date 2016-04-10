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
(* /src/bvalue.mli                                                            *)
(* Boomerang run-time value interface                                         *)
(* $Id: bvalue.mli 4998 2011-03-16 21:53:34Z mgree $ *)
(*****************************************************************************)

type prefs =
  | PrBol of bool Prefs.t
  | PrInt of int Prefs.t
  | PrStr of string Prefs.t
  | PrSLi of string list Prefs.t

(** {2 Boomerang Run-time Values} *)

type t = 
    | Unt of Info.t
    | Bol of Info.t * string option (* None = true ; 
                                       Some s = false w/ counterexample s *)
    | Int of Info.t * int
    | Chr of Info.t * char
    | Str of Info.t * string
    | Rx  of Info.t * Brx.t
    | Arx of Info.t * Barx.t
    | Kty of Info.t * Blenses.ktype
    | Mty of Info.t * Blenses.mtype
    | Lns of Info.t * Blenses.MLens.t
    | Can of Info.t * Blenses.Canonizer.t
    | Prf of Info.t * prefs
    | Fun of Info.t * (((unit -> t) -> unit) option -> t -> t) (* NB closure takes (optional) workqueue enqueue function *)
    | Par of Info.t * t * t
    | Vnt of Info.t * Bident.Qid.t * Bident.Id.t * t option
(** The type of boxed run-time values. *)

val info_of_t : t -> Info.t
(** [info_of_t v] extracts the parsing info associated with (the blame of) [v]. *)

val equal : t -> t -> bool
(** [equal v1 v2] returns [true] iff [v1] and [v2] represent the same
    value. As usual, [v1] and [v2] must have equality sorts. *)

val format : t -> unit
(** [format v] pretty prints [v] using [Util.format]. *)

val string_of_t : t -> string
(** [string_of_t v] pretty prints [v] as a string. *)

val sort_string_of_t : t -> string
(** [sort_string_of_t v] renders [v]'s sort as a string. *)

(** {2 Conversions on run-time values} *)

val get_u : t -> unit
val get_b : t -> bool
val get_x : t -> string option
val get_i : t -> int
val get_c : t -> char
val get_s : t -> string
val get_r : t -> Brx.t
val get_a : t -> Barx.t
val get_k : t -> Blenses.ktype
val get_m : t -> Blenses.mtype
val get_l : t -> Blenses.MLens.t
val get_q : t -> Blenses.Canonizer.t
val get_bP : t -> bool Prefs.t
val get_iP : t -> int Prefs.t
val get_sP : t -> string Prefs.t
val get_szP : t -> string list Prefs.t
val get_p : t -> (t*t)
val get_v : t -> (Bident.Id.t * t option)
val get_f : t -> (((unit -> t) -> unit) option -> t -> t)

val mk_u : Info.t -> unit -> t
val mk_b : Info.t -> bool -> t
val mk_x : Info.t -> string option -> t
val mk_i : Info.t -> int -> t
val mk_c : Info.t -> char -> t
val mk_l : Info.t -> Blenses.MLens.t -> t
val mk_r : Info.t -> Brx.t -> t
val mk_a : Info.t -> Barx.t -> t
val mk_k : Info.t -> Blenses.ktype -> t
val mk_m : Info.t -> Blenses.mtype -> t
val mk_s : Info.t -> string -> t
val mk_q : Info.t -> Blenses.Canonizer.t -> t
val mk_bP : Info.t -> bool Prefs.t -> t
val mk_iP : Info.t -> int Prefs.t -> t
val mk_sP : Info.t -> string Prefs.t -> t
val mk_szP : Info.t -> string list Prefs.t -> t
val mk_p : Info.t -> t * t -> t
val mk_f : Info.t -> (t -> t) -> t

val mk_ufun : Info.t -> (unit -> t) -> t
val mk_bfun : Info.t -> (bool -> t) -> t
val mk_xfun : Info.t -> (string option -> t) -> t
val mk_ifun : Info.t -> (int -> t) -> t
val mk_cfun : Info.t -> (char -> t) -> t
val mk_lfun : Info.t -> (Blenses.MLens.t -> t) -> t
val mk_rfun : Info.t -> (Brx.t -> t) -> t
val mk_afun : Info.t -> (Barx.t -> t) -> t
val mk_kfun : Info.t -> (Blenses.ktype -> t) -> t
val mk_mfun : Info.t -> (Blenses.mtype -> t) -> t
val mk_sfun : Info.t -> (string -> t) -> t
val mk_qfun : Info.t -> (Blenses.Canonizer.t -> t) -> t
val mk_bPfun : Info.t -> (bool Prefs.t -> t) -> t
val mk_iPfun : Info.t -> (int Prefs.t -> t) -> t
val mk_sPfun : Info.t -> (string Prefs.t -> t) -> t
val mk_szPfun : Info.t -> (string list Prefs.t -> t) -> t
val mk_pfun : Info.t -> (t * t -> t) -> t
val mk_vfun : Info.t -> (Bident.Id.t * t option -> t) -> t
val mk_ffun : Info.t -> ((t -> t) -> t) -> t

val string_of_t : t -> string

val list_qid : Bident.Qid.t
val get_list : t -> t list
val mk_list : Info.t -> t list -> t
val mk_listfun : Info.t -> (t list -> t) -> t

val option_qid : Bident.Qid.t
val get_option : t -> t option
val mk_option : Info.t -> t option -> t
val mk_optionfun : Info.t -> (t option -> t) -> t

val species_qid : Bident.Qid.t
val get_species : t -> Btag.species
val mk_species : Info.t -> Btag.species -> t
val mk_speciesfun : Info.t -> (Btag.species -> t) -> t

val predicate_qid : Bident.Qid.t
val get_predicate : t -> Btag.predicate
val mk_predicate : Info.t -> Btag.predicate -> t
val mk_predicatefun : Info.t -> (Btag.predicate -> t) -> t

val tag_qid : Bident.Qid.t
val get_tag : t -> Btag.t
val mk_tag : Info.t -> Btag.t -> t
val mk_tagfun : Info.t -> (Btag.t -> t) -> t
