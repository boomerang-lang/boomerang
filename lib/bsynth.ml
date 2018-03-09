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
open Lang

module Info = Hbase.Info
module MLens = Blenses.MLens

module LensModule =
struct
  type t = Blenses.MLens.t
  let hash_fold_t _ _ = failwith "unimplemented"
  let hash _ = failwith "unimplemented"
  let compare _ _ = failwith "unimplemented"
  let pp _ _ = failwith "unimplemented"
  let show _ = failwith "unimplemented"
end

module IntToLens = DictOf(IntModule)(LensModule)

let rec to_boomerang_regex
  : Regex.t -> Brx.t =
  Regex.fold
    ~empty_f:Brx.empty
    ~base_f:Brx.mk_string
    ~concat_f:Brx.mk_seq
    ~or_f:Brx.mk_alt
    ~star_f:Brx.mk_star
    ~closed_f:ident

let rec to_boomerang_lens
    (i:Info.t)
    (d:IntToLens.t)
  : Lens.t -> MLens.t =
  Lens.fold
    ~disc_f:(fun r1 r2 s1 s2 ->
        Blenses.MLens.disconnect
          i
          (to_boomerang_regex r1)
          (to_boomerang_regex r2)
          (fun _ -> s1)
          (fun _ -> s2))
    ~concat_f:(Blenses.MLens.concat i)
    ~swap_f:(fun l1 l2 ->
        MLens.permute i [1;0] [l1;l2])
    ~union_f:(MLens.union i)
    ~compose_f:(MLens.compose i)
    ~iterate_f:(MLens.star i)
    ~identity_f:((MLens.copy i) % to_boomerang_regex)
    ~inverse_f:(MLens.invert i)
    ~permute_f:(fun il ml -> MLens.permute i (Permutation.as_int_list il) ml)
    ~closed_f:(fun i _ -> IntToLens.lookup_exn d i)

let retrieve_existing_lenses
    (relevant_regexps:Brx.t list)
    (e:CEnv.t)
  : (Lens.t * Regex.t * Regex.t) list * IntToLens.t =
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

  let (_,d,lens_list) =
    List.fold_left
      ~f:(fun (i,d,lens_list) (l,s,v) ->
          let rr = Brx.to_optician_regexp v in
          let rl = Brx.to_optician_regexp s in
          let creater =
            (fun s ->
               (Blenses.MLens.rcreater
                  l
                  (Bstring.of_string s)))
          in
          let createl =
            (fun s ->
               (Blenses.MLens.rcreatel
                  l
                  (Bstring.of_string s)))
          in
          let putr =
            (fun s v ->
               (Blenses.MLens.rputr
                  l
                  (Bstring.of_string s)
                  (Bstring.of_string v)))
          in
          let putl =
            (fun v s ->
               (Blenses.MLens.rputl
                  l
                  (Bstring.of_string v)
                  (Bstring.of_string s)))
          in
          let lens =
            Lens.make_closed
              ~rr:rr
              ~rl:rl
              ~creater:creater
              ~createl:createl
              ~putr:putr
              ~putl:putl
              i
          in
          let d = IntToLens.insert d i l in
          let i = i + 1 in
          let lens_list = (lens,rl,rr)::lens_list in
          (i,d,lens_list))
      ~init:(0,IntToLens.empty,[])
      lenses_types
  in
  (lens_list,d)

let synth
    (i:Info.t)
    (env:CEnv.t)
    (r1:Brx.t)
    (r2:Brx.t)
    (creater_exs:create_examples)
    (createl_exs:create_examples)
    (putr_exs:put_examples)
    (putl_exs:put_examples)
  : Blenses.MLens.t =
  let subregexps = (Brx.subregexp_list r1)@(Brx.subregexp_list r2) in
  let (lss,d) = retrieve_existing_lenses subregexps env in
  let r1 = Brx.to_optician_regexp r1 in
  let r2 = Brx.to_optician_regexp r2 in
  to_boomerang_lens
    i
    d
    (Option.value_exn
       (Gen.gen_lens
          lss
          r1
          r2
          creater_exs
          createl_exs
          putr_exs
          putl_exs))
