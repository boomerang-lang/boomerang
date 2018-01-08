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
(* /src/bvalue.ml                                                             *)
(* Boomerang run-time values                                                  *)
(* $Id: bvalue.ml 4998 2011-03-16 21:53:34Z mgree $ *)
(******************************************************************************)

open Ubase
open Hbase

open Bsyntax
open Bident
open Benv

open Stdlib
open Optician 
open Regexcontext
open Lenscontext
open Lang

module Info = Hbase.Info
module MLens = Blenses.MLens

let rec to_boomerang_regex
    (rc:RegexContext.t)
  : Regex.t -> Brx.t =
  Regex.fold
    ~empty_f:Brx.empty
    ~base_f:Brx.mk_string
    ~concat_f:Brx.mk_seq
    ~or_f:Brx.mk_alt
    ~star_f:Brx.mk_star
    ~var_f:(fun v ->
        to_boomerang_regex
          rc
          (RegexContext.lookup_exn rc v))

let rec to_boomerang_lens
    (i:Info.t)
    (rc:RegexContext.t)
    (lc:LensContext.t)
  : Lens.t -> MLens.t =
  Lens.fold
    ~const_f:(fun s1 s2 ->
        Blenses.MLens.clobber
          i
          (Brx.mk_string s1)
          s2
          (fun _ -> s1))
    ~concat_f:(Blenses.MLens.concat i)
    ~swap_f:(fun l1 l2 ->
        MLens.permute i [1;0] [l1;l2])
    ~union_f:(MLens.union i)
    ~compose_f:(MLens.compose i)
    ~iterate_f:(MLens.star i)
    ~identity_f:((MLens.copy i) % (to_boomerang_regex rc))
    ~inverse_f:(MLens.invert i)
    ~permute_f:(fun il ml -> MLens.permute i (Permutation.to_int_list il) ml)
    ~variable_f:(fun v ->
        to_boomerang_lens
          i
          rc
          lc
          (LensContext.lookup_impl_exn lc v))

let populate_lens_context
    (relevant_regexps:Brx.t list)
    (e:CEnv.t)
  : Lenscontext.LensContext.t * RegexContext.t =
  let lens_list =
    List.filter_map
      ~f:ident
      (CEnv.fold
         (fun _ (_,v) acc -> (Bvalue.get_l_safe v)::acc)
         e
         [])
  in
  let bij_lens_list =
    List.filter
      ~f:Blenses.MLens.bij
      lens_list
  in
  let lenses_types =
    List.filter_map
      ~f:(fun l ->
          let stype_o =
            List.find
              ~f:(Brx.equiv (Blenses.MLens.stype l))
              relevant_regexps
          in
          let vtype_o =
            List.find
              ~f:(Brx.equiv (Blenses.MLens.vtype l))
              relevant_regexps
          in
          begin match (stype_o,vtype_o) with
            | (Some stype, Some vtype) -> Some (l,stype,vtype)
            | _ -> None
          end)
      bij_lens_list
  in

  let optician_lenses_types =
    List.filter_map
      ~f:(fun (l,s,v) ->
          let l_o = Blenses.MLens.to_optician_lens l in
          Option.map
            ~f:(fun l ->
                (l
                ,Brx.to_optician_regexp s
                ,Brx.to_optician_regexp v))
            l_o)
      lenses_types
  in

  let (rc,optician_lenses_types) =
    List.fold_left
      ~f:(fun (rc,acc) (l,r1,r2) ->
          let (rc,r1) = Regex_utilities.iteratively_deepen rc r1 in
          let (rc,r2) = Regex_utilities.iteratively_deepen rc r2 in
          (rc,(l,r1,r2)::acc))
      ~init:(RegexContext.empty,[])
      optician_lenses_types
  in

  (LensContext.insert_unnamed_list_exn
     LensContext.empty
     optician_lenses_types
  ,rc)

let synth
    (i:Info.t)
    (env:CEnv.t)
    (r1:Brx.t)
    (r2:Brx.t)
    (exs:(string * string) list)
  : Blenses.MLens.t =
  let subregexps = (Brx.subregexp_list r1)@(Brx.subregexp_list r2) in
  let (lc,rc) = populate_lens_context subregexps env in
  let r1 = Brx.to_optician_regexp r1 in
  let r2 = Brx.to_optician_regexp r2 in
  let (rc,r1) = Regex_utilities.iteratively_deepen rc r1 in
  let (rc,r2) = Regex_utilities.iteratively_deepen rc r2 in
  to_boomerang_lens
    i
    rc
    lc
    (Option.value_exn
       (Gen.gen_lens
          rc
          lc
          r1
          r2
          exs))
