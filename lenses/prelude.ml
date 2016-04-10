(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2008                                                         *)
(* J. Nathan Foster, and Benjamin C. Pierce                                   *)
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
(* /lenses/prelude.ml                                                         *)
(* OCaml definitions of lens primitives                                       *)
(* $Id: prelude.ml 4998 2011-03-16 21:53:34Z mgree $ *)
(******************************************************************************)

(* ----- imports and abbreviations ----- *)
open Bvalue 
open Bident
module S = Bsyntax
module L = Blenses.MLens
module C = Blenses.Canonizer
module P = Blenses.Permutations
let (^) = Pervasives.(^)
let sprintf = Printf.sprintf 
let msg = Util.format
let (^>) = S.(^>)
let (^*) = S.(^*)

(* ----- helpers ----- *)
let wrap_rep i r = 
  match Brx.representative r with
    | None -> 
        raise (Error.Harmony_error
                 (fun () -> 
                    msg "%s: cannot calculate representative; %s is empty."
                      (Info.string_of_t i)
                      (Brx.string_of_t r)))
    | Some w -> w

let poly_error i se1 v1 = 
  Error.simple_error 
    (sprintf "%s: expected %s but found %s"
       (Info.string_of_t i)
       (Bprint.string_of_sort se1)
       (sort_string_of_t v1))

let mk_prelude_info s = Info.M (sprintf "%s built-in" s)

(* The next several helpers construct run-time values; each constructs
   a triple consisting of its name (a Bsyntax.Qid.t), Bsyntax.sort,
   and Bvalue.t *)
let pmk0 s1 mk1 x v1 = 
  let i = mk_prelude_info x in 
  let q = Qid.mk_native_prelude_t i x in 
  let s = s1 in  
  let v = mk1 i v1 in 
  (q,s,v)

let pmk1 s1 mk1 s2 mk2 x f = 
  let i = mk_prelude_info x in 
  let q = Qid.mk_native_prelude_t i x in 
  let s = s1 ^> s2 in 
  let f = mk1 i (fun v1 -> mk2 i (f i v1)) in 
  (q,s,f)
      
let pmk2 s1 mk1 s2 mk2 s3 mk3 x f = 
  let i = mk_prelude_info x in 
  let q = Qid.mk_native_prelude_t i x in 
  let s = s1 ^> s2 ^> s3 in 
  let f = mk1 i (fun v1 -> mk2 i (fun v2 -> mk3 i (f i v1 v2))) in 
  (q,s,f)
      
let pmk3 s1 mk1 s2 mk2 s3 mk3 s4 mk4 x f = 
  let i = mk_prelude_info x in 
  let q = Qid.mk_native_prelude_t i x in 
  let s = s1 ^> s2 ^> s3 ^> s4 in 
  let f = mk1 i (fun v1 -> mk2 i (fun v2 -> mk3 i (fun v3 -> mk4 i (f i v1 v2 v3)))) in 
  (q,s,f)

let pmk4 s1 mk1 s2 mk2 s3 mk3 s4 mk4 s5 mk5 x f = 
  let i = mk_prelude_info x in 
  let q = Qid.mk_native_prelude_t i x in 
  let s = s1 ^> s2 ^> s3 ^> s4 ^> s5 in 
  let f = mk1 i (fun v1 -> mk2 i (fun v2 -> mk3 i (fun v3 -> mk4 i (fun v4 -> mk5 i (f i v1 v2 v3 v4))))) in 
  (q,s,f)

let pmk5 s1 mk1 s2 mk2 s3 mk3 s4 mk4 s5 mk5 s6 mk6 x f = 
  let i = mk_prelude_info x in
  let q = Qid.mk_native_prelude_t i x in
  let s = s1 ^> s2 ^> s3 ^> s4 ^> s5 ^> s6 in
  let f = mk1 i (
    fun v1 -> mk2 i (
      fun v2 -> mk3 i (
        fun v3 -> mk4 i (
          fun v4 -> mk5 i (
            fun v5 -> mk6 i (f i v1 v2 v3 v4 v5))))))
  in
  (q,s,f)

let mk_szR i l = mk_list i (Safelist.rev_map (mk_s i) l)
let fulldoc f name default doc = f name default doc ""

(* The helpers lift OCaml functions to work with run-time values. They
   perform unboxing of the arguments, and box the result up as a
   Bvalue.t. Each letter after the underscore indicates a sort---e.g.,
   pmk_bb lifts a (Info.t -> bool -> bool) function to a Bvalue.t Fun
   that maps a Bol to a Bol. The following legend gives the mapping
   between letters and boxed run-time values:

     u : Unt
     b : Bol (using bools)
     x : Bol (using string option)
     i : Int
     c : Chr
     s : Str
     r : Rx
     a : Arx
     l : Lns
     q : Can
     P : Prf
     e : Rel
     p : Par
     v : Vnt (unused)
     f : Fun (unused)

     z : list
     o : option
     t : tag
*)

let pmk_cs    = pmk1 S.SChar mk_cfun S.SString mk_s
let pmk_sic   = pmk2 S.SString mk_sfun S.SInteger mk_ifun S.SChar mk_c
let pmk_ci    = pmk1 S.SChar mk_cfun S.SInteger mk_i
let pmk_ic    = pmk1 S.SInteger mk_ifun S.SChar mk_c

let pmk_bb    = pmk1 S.SBool mk_bfun S.SBool mk_b
let pmk_xxx   = pmk2 S.SBool mk_xfun S.SBool mk_xfun S.SBool mk_x 

let pmk_is    = pmk1 S.SInteger mk_ifun S.SString mk_s
let pmk_iib   = pmk2 S.SInteger mk_ifun  S.SInteger mk_ifun S.SBool mk_b
let pmk_iii   = pmk2 S.SInteger mk_ifun S.SInteger mk_ifun S.SInteger mk_i 
let pmk_bill  = pmk3 S.SBool mk_bfun S.SInteger mk_ifun S.SLens mk_lfun S.SLens mk_l

let pmk_s     = pmk0 S.SString mk_s
let pmk_ss    = pmk1 S.SString mk_sfun S.SString mk_s
let pmk_si    = pmk1 S.SString mk_sfun S.SInteger mk_i
let pmk_sb    = pmk1 S.SString mk_sfun S.SBool mk_b
let pmk_ssb   = pmk2 S.SString mk_sfun S.SString mk_sfun S.SBool mk_b
let pmk_ssi   = pmk2 S.SString mk_sfun S.SString mk_sfun S.SInteger mk_i
let pmk_sss   = pmk2 S.SString mk_sfun S.SString mk_sfun S.SString mk_s
let pmk_siis  = pmk3 S.SString mk_sfun S.SInteger mk_ifun S.SInteger mk_ifun S.SString mk_s
let pmk_ssu   = pmk2 S.SString mk_sfun S.SString mk_sfun S.SUnit mk_u
let pmk_su    = pmk1 S.SString mk_sfun S.SUnit mk_u
let pmk_us    = pmk1 S.SUnit mk_ufun S.SString mk_s
let pmk_sr    = pmk1 S.SString mk_sfun S.SRegexp mk_r
let pmk_sll   = pmk2 S.SString mk_sfun S.SLens mk_lfun S.SLens mk_l
let pmk_rll   = pmk2 S.SRegexp mk_rfun S.SLens mk_lfun S.SLens mk_l
let pmk_ssll  = 
  pmk3 S.SString mk_sfun S.SString mk_sfun S.SLens mk_lfun S.SLens mk_l
let pmk_sicio = pmk3 S.SString mk_sfun S.SInteger mk_ifun S.SChar mk_cfun (S.SData([S.SInteger],option_qid)) mk_option
let pmk_scz   = pmk1 S.SString mk_sfun (S.SData([S.SChar],list_qid)) mk_list
let pmk_czs   = pmk1 (S.SData([S.SChar],list_qid)) mk_listfun S.SString mk_s
let pmk_ircsq =
  pmk4 S.SInteger mk_ifun S.SRegexp mk_rfun S.SChar mk_cfun S.SString mk_sfun
    S.SCanonizer mk_q

let pmk_r     = pmk0 S.SRegexp mk_r 
let pmk_rr    = pmk1 S.SRegexp mk_rfun S.SRegexp mk_r
let pmk_rrr   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SRegexp mk_r
let pmk_riir  = 
  pmk3 S.SRegexp mk_rfun S.SInteger mk_ifun S.SInteger mk_ifun S.SRegexp mk_r
let pmk_rb    = pmk1 S.SRegexp mk_rfun S.SBool mk_b
let pmk_rrb   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_b
let pmk_rs    = pmk1 S.SRegexp mk_rfun S.SString mk_s
let pmk_rx    = pmk1 S.SRegexp mk_rfun S.SBool mk_x
let pmk_rl    = pmk1 S.SRegexp mk_rfun S.SLens mk_l
let pmk_rrl   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SLens mk_l
let pmk_rrb   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_b
let pmk_rrs   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SString mk_s
let pmk_rrx   = pmk2 S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_x
let pmk_rsx   = pmk2 S.SRegexp mk_rfun S.SString mk_sfun S.SBool mk_x
let pmk_rsb   = pmk2 S.SRegexp mk_rfun S.SString mk_sfun S.SBool mk_b
let pmk_rsr   = pmk2 S.SRegexp mk_rfun S.SString mk_sfun S.SRegexp mk_r
let pmk_rsi   = pmk2 S.SRegexp mk_rfun S.SString mk_sfun S.SInteger mk_i
let pmk_rssl  = pmk3 S.SRegexp mk_rfun S.SString mk_sfun S.SString mk_sfun S.SLens mk_l

let pmk_aaa   = pmk2 S.SAregexp mk_afun S.SAregexp mk_afun S.SAregexp mk_a
let pmk_aa    = pmk1 S.SAregexp mk_afun S.SAregexp mk_a
let pmk_biaa  = pmk3 S.SBool mk_bfun S.SInteger mk_ifun S.SAregexp mk_afun S.SAregexp mk_a
let pmk_aab   = pmk2 S.SAregexp mk_afun S.SAregexp mk_afun S.SBool mk_b
let pmk_aax   = pmk2 S.SAregexp mk_afun S.SAregexp mk_afun S.SBool mk_x
let pmk_sssaa = pmk4 S.SString  mk_sfun S.SString  mk_sfun S.SString mk_sfun S.SAregexp mk_afun S.SAregexp mk_a

let pmk_ab    = pmk1 S.SAregexp mk_afun S.SBool mk_b
let pmk_aiia  = pmk3 S.SAregexp mk_afun S.SInteger mk_ifun S.SInteger mk_ifun S.SAregexp mk_a
let pmk_ra    = pmk1 S.SRegexp  mk_rfun S.SAregexp mk_a
let pmk_ar    = pmk1 S.SAregexp mk_afun S.SRegexp mk_r
let pmk_al    = pmk1 S.SAregexp mk_afun S.SLens mk_l
let pmk_saa   = pmk2 S.SString  mk_sfun S.SAregexp mk_afun S.SAregexp mk_a
let pmk_ssaa  = pmk3 S.SString  mk_sfun S.SString  mk_sfun S.SAregexp mk_afun S.SAregexp mk_a

let pmk_ll    = pmk1 S.SLens mk_lfun S.SLens mk_l
let pmk_lll   = pmk2 S.SLens mk_lfun S.SLens mk_lfun S.SLens mk_l
let pmk_llll  = 
  pmk3 S.SLens mk_lfun S.SLens mk_lfun S.SLens mk_lfun S.SLens mk_l
let pmk_liil  = 
  pmk3 S.SLens mk_lfun S.SInteger mk_ifun S.SInteger mk_ifun S.SLens mk_l
let pmk_lu    = pmk1 S.SLens mk_lfun S.SUnit mk_u
let pmk_lb    = pmk1 S.SLens mk_lfun S.SBool mk_b
let pmk_lr    = pmk1 S.SLens mk_lfun S.SRegexp mk_r
let pmk_la    = pmk1 S.SLens mk_lfun S.SAregexp mk_a
let pmk_lk    = pmk1 S.SLens mk_lfun S.SSkeletons mk_k
let pmk_lm    = pmk1 S.SLens mk_lfun S.SResources mk_m
let pmk_lss   = pmk2 S.SLens mk_lfun S.SString mk_sfun S.SString mk_s
let pmk_lsl   = pmk2 S.SLens mk_lfun S.SString mk_sfun S.SLens mk_l
let pmk_lsss  =
  pmk3 S.SLens mk_lfun S.SString mk_sfun S.SString mk_sfun S.SString mk_s 
let pmk_lq    = pmk1 S.SLens mk_lfun S.SCanonizer mk_q
let pmk_lql   = pmk2 S.SLens mk_lfun S.SCanonizer mk_qfun S.SLens mk_l
let pmk_lrrb  = 
  pmk3 S.SLens mk_lfun S.SRegexp mk_rfun S.SRegexp mk_rfun S.SBool mk_b

let pmk_mmb   = pmk2 S.SResources mk_mfun S.SResources mk_mfun S.SBool mk_b
let pmk_mmx   = pmk2 S.SResources mk_mfun S.SResources mk_mfun S.SBool mk_x

let pmk_qq    = pmk1 S.SCanonizer mk_qfun S.SCanonizer mk_q
let pmk_qb    = pmk1 S.SCanonizer mk_qfun S.SBool mk_b
let pmk_qqq   = pmk2 S.SCanonizer mk_qfun S.SCanonizer mk_qfun S.SCanonizer mk_q
let pmk_qr    = pmk1 S.SCanonizer mk_qfun S.SRegexp mk_r
let pmk_qa    = pmk1 S.SCanonizer mk_qfun S.SAregexp mk_a
let pmk_rq    = pmk1 S.SRegexp mk_rfun S.SCanonizer mk_q
let pmk_qss   = pmk2 S.SCanonizer mk_qfun S.SString mk_sfun S.SString mk_s
let pmk_qll   = pmk2 S.SCanonizer mk_qfun S.SLens mk_lfun S.SLens mk_l
let pmk_qiiq  =
  pmk3 S.SCanonizer mk_qfun S.SInteger mk_ifun S.SInteger mk_ifun
    S.SCanonizer mk_q
let pmk_rrfq  =
  pmk3 S.SRegexp mk_rfun S.SRegexp mk_rfun (S.SString ^> S.SString) mk_ffun
    S.SCanonizer mk_q

let pmk_sync  = 
  pmk4 S.SLens mk_lfun S.SString mk_sfun S.SString mk_sfun S.SString mk_sfun 
    (((S.SString ^* S.SString) ^* S.SString) ^* S.SString) (fun i x -> x)

let pmk_dup1  = 
  pmk3 S.SLens mk_lfun (S.SString ^> S.SString) mk_ffun S.SRegexp mk_rfun 
    S.SLens mk_l

let pmk_dup2  = 
  pmk3 (S.SString ^> S.SString) mk_ffun S.SRegexp mk_rfun S.SLens mk_lfun 
    S.SLens mk_l

let pmk_rsfl = 
  pmk3 S.SRegexp mk_rfun S.SString mk_sfun (S.SString ^> S.SString) mk_ffun 
    S.SLens mk_l

let pmk_rzl =
  pmk1 (S.SData([S.SRegexp],list_qid)) mk_listfun S.SLens mk_l

let pmk_izlzl = 
  pmk2 (S.SData([S.SInteger],list_qid)) mk_listfun 
    (S.SData([S.SLens],list_qid)) mk_listfun S.SLens mk_l 

let pmk_rzq =
  pmk1 (S.SData([S.SRegexp],list_qid)) mk_listfun S.SCanonizer mk_q

let pmk_azq =
  pmk1 (S.SData([S.SAregexp],list_qid)) mk_listfun S.SCanonizer mk_q

let pmk_iiz =
  pmk1 S.SInteger mk_ifun 
    (S.SData([S.SInteger],list_qid)) mk_list

let pmk_iizz =
  pmk1 S.SInteger mk_ifun 
    (S.SData([S.SData([S.SInteger],list_qid)],list_qid)) mk_list

let pmk_iziz =
  pmk1 (S.SData([S.SInteger],list_qid)) mk_listfun 
    (S.SData([S.SInteger],list_qid)) mk_list

let pmk_rssz =
  pmk2 S.SRegexp mk_rfun S.SString mk_sfun (S.SData([S.SString],list_qid)) mk_list

let pmk_tll  = 
  pmk2 (S.SData ([], tag_qid)) mk_tagfun S.SLens mk_lfun S.SLens mk_l

let pmk_taa  = 
  pmk2 (S.SData ([], tag_qid)) mk_tagfun S.SAregexp mk_afun S.SAregexp mk_a

let pmk_tax  = 
  pmk2 (S.SData ([], tag_qid)) mk_tagfun S.SAregexp mk_afun S.SBool mk_x

let pmk_aax  = 
  pmk2 S.SAregexp mk_afun S.SAregexp mk_afun S.SBool mk_x

let pmk_sssll  = 
  pmk4 S.SString mk_sfun S.SString mk_sfun S.SString mk_sfun S.SLens mk_lfun S.SLens mk_l

let pmk_sszsll =
  pmk4 S.SString mk_sfun (S.SData ([S.SString], list_qid)) mk_listfun S.SString mk_sfun S.SLens mk_lfun S.SLens mk_l

let pmk_sszsaa =
  pmk4 S.SString mk_sfun (S.SData ([S.SString], list_qid)) mk_listfun S.SString mk_sfun S.SAregexp mk_afun S.SAregexp mk_a

let pmk_tkmx = pmk3 (S.SData ([], tag_qid)) mk_tagfun S.SSkeletons mk_kfun S.SResources mk_mfun S.SBool mk_x

let pmk_sososoll =
  let so = S.SData ([S.SString], option_qid) in
  let mkof = mk_optionfun in
  pmk4 so mkof so mkof so mkof S.SLens mk_lfun S.SLens mk_l

let pmk_sXsXP _X mk_Xfun _PrX mk_XP =
  let s = S.SString in
  let sf = mk_sfun in
  pmk3 s sf _X mk_Xfun s sf (S.SPrefs _PrX) mk_XP

let pmk_XPX _PrX mk_XPfun _X mk_X =
  pmk1 (S.SPrefs _PrX) mk_XPfun _X mk_X

let pmk_XPsu _PrX mk_XPfun =
  pmk2 (S.SPrefs _PrX) mk_XPfun S.SString mk_sfun S.SUnit mk_u

let pmk_uXP _PrX mk_XP =
  pmk1 S.SUnit mk_ufun (S.SPrefs _PrX) mk_XP

let pmk_sbsbP = pmk_sXsXP S.SBool mk_bfun S.PrBool mk_bP
let pmk_sisiP = pmk_sXsXP S.SInteger mk_ifun S.PrInt mk_iP
let pmk_ssssP = pmk_sXsXP S.SString mk_sfun S.PrString mk_sP
let pmk_ssszP =
  let s = S.SString in
  let sf = mk_sfun in
  pmk2 s sf s sf (S.SPrefs S.PrStringList) mk_szP

let pmk_bPb = pmk_XPX S.PrBool mk_bPfun S.SBool mk_b
let pmk_iPi = pmk_XPX S.PrInt mk_iPfun S.SInteger mk_i
let pmk_sPs = pmk_XPX S.PrString mk_sPfun S.SString mk_s
let pmk_szPszR = pmk_XPX S.PrStringList mk_szPfun (S.SData ([S.SString], list_qid)) mk_szR

let pmk_bPsu = pmk_XPsu S.PrBool mk_bPfun
let pmk_iPsu = pmk_XPsu S.PrInt mk_iPfun
let pmk_sPsu = pmk_XPsu S.PrString mk_sPfun
let pmk_szPsu = pmk_XPsu S.PrStringList mk_szPfun

let pmk_ubP = pmk_uXP S.PrBool mk_bP
let pmk_uiP = pmk_uXP S.PrInt mk_iP
let pmk_usP = pmk_uXP S.PrString mk_sP
let pmk_uszP = pmk_uXP S.PrStringList mk_szP

let prelude_spec =
  [ (* lens operations *)
    pmk_lss    "get"                  (fun _ ml s -> L.rget ml (Bstring.of_string s))
  ; pmk_lsss   "put"                  (fun _ ml v s -> L.rput ml (Bstring.of_string v) (Bstring.of_string s))
  ; pmk_lss    "create"               (fun _ ml v -> L.rcreate ml (Bstring.of_string v))
  ; pmk_ll     "invert"               L.invert
                                        
  (* core lens combinators *)           
  ; pmk_rl     "copy"                 L.copy
  ; pmk_rsfl   "clobber"              (fun i r s f -> L.clobber i r s (fun s -> get_s (f (mk_s i s))))
  ; pmk_lll    "lens_union"           L.union
  ; pmk_lll    "lens_concat"          L.concat
  ; pmk_lll    "lens_swap"            (fun i l1 l2 -> L.permute i [1;0] [l1;l2])
  ; pmk_liil   "lens_iter"            L.iter
  ; pmk_ll     "lens_star"            (fun i l -> L.iter i l 0 (-1))
  ; pmk_ll     "lens_plus"            (fun i l -> L.iter i l 1 (-1))
  ; pmk_ll     "lens_option"          (fun i l -> L.iter i l 0 1)
  ; pmk_izlzl  "lens_permute"         (fun i is ls ->
                                         L.permute i
                                           (Safelist.map get_i is)
                                           (Safelist.map get_l ls))
  ; pmk_tll    "lens_match"           L.mmatch
  ; pmk_lll    "compose"              L.compose
  ; pmk_ll     "align"                L.align
  ; pmk_lsl    "default"              (fun i l w -> L.default i l (Bstring.of_string w))
  ; pmk_bill   "lens_weight"          (fun i b w -> L.weight i b (Bannot.Weight.of_int w))
  ; pmk_rzl    "partition"            (fun i rl -> L.partition i (Safelist.map get_r rl))
  ; pmk_rl     "merge"                L.merge
  ; pmk_ll     "fiat"                 L.fiat
  ; pmk_dup1   "dup1"                 (fun i l f fat ->
                                         L.dup1 i l
                                           (fun s -> get_s (f (mk_s i s))) fat)
  ; pmk_dup2   "dup2"                 (fun i f fat l ->
                                         L.dup2 i
                                           (fun s -> get_s (f (mk_s i s))) fat
                                           l)
                                      
  (* canonizer operations *)          
  ; pmk_rq     "canonizer_copy"       C.copy
  ; pmk_qqq    "canonizer_union"      C.union
  ; pmk_qqq    "canonizer_concat"     C.concat
  ; pmk_qiiq   "canonizer_iter"       C.iter
  ; pmk_qss    "canonize"             (fun _ cn u -> C.canonize cn (Bstring.of_string u))
  ; pmk_qss    "choose"               (fun _ cn c -> C.choose cn (Bstring.of_string c))
  ; pmk_qll    "left_quot"            L.left_quot
  ; pmk_lql    "right_quot"           L.right_quot
  ; pmk_ircsq  "columnize"            C.columnize
  ; pmk_rrfq   "normalize"            (fun i ct ct0 f ->
                                         C.normalize i ct ct0
                                           (fun s -> get_s (f (mk_s i s))))
  ; pmk_azq    "sort"                 (fun i arl -> C.sort i
                                         (Safelist.map get_a arl))
  ; pmk_qr     "uncanonized_type"     (fun _ -> C.uncanonized_type)
  ; pmk_qr     "canonized_type"       (fun _ -> C.canonized_type)
  ; pmk_qa     "uncanonized_atype"     (fun _ -> C.uncanonized_atype)
  ; pmk_qa     "canonized_atype"       (fun _ -> C.canonized_atype)
                                
  (* char operations *)               
  ; pmk_cs     "string_of_char"       (fun _ -> String.make 1)
  ; pmk_ic     "chr"                  (fun _ -> Char.chr)
  ; pmk_ci     "code"                 (fun _ -> Char.code)

  (* int operations *)
  ; pmk_is     "string_of_int"        (fun _ -> string_of_int)
  ; pmk_si     "int_of_string"        (fun _ -> int_of_string)
  ; pmk_iiz    "mk_seq"               (fun i k ->
					 let rec f l = function
					   | 0 -> l
					   | n -> f (mk_i i (n-1)::l) (n-1)
					 in
                                         assert (k >= 0);
                                         f [] k)
  ; pmk_iizz   "permutations"         (fun i k ->
                                         Safelist.map
                                           (fun l -> mk_list i
                                              (Safelist.map (mk_i i) l))
                                           (P.permutations k))
  ; pmk_iziz   "invert_permutation"   (fun i sigma ->
                                         Safelist.map (mk_i i)
                                           (P.invert_permutation
                                              (Safelist.map get_i sigma)))

  (* arithmetic operations *)               
  ; pmk_iib     "gt"                  (fun _ -> (>))
  ; pmk_iib     "lt"                  (fun _ -> (<))
  ; pmk_iib     "geq"                 (fun _ -> (>=))
  ; pmk_iib     "leq"                 (fun _ -> (<=))
  ; pmk_iii    "plus"                 (fun _ -> (+))
  ; pmk_iii    "minus"                (fun _ -> (-))
  ; pmk_iii    "times"                (fun _ x y -> x * y)
  ; pmk_iii    "div"                  (fun _ x y -> x / y)
  ; pmk_iii    "mod"                  (fun _ x y -> x mod y)
                                    
  (* string operations *)             
  ; pmk_sss    "string_concat"        (fun _ -> (^))    
  ; pmk_si     "length"               (fun _ -> String.length)
  ; pmk_sic    "get_char"             (fun _ -> String.get)
  ; pmk_siis   "string_sub"           (fun _ -> String.sub)
  ; pmk_sicio  "string_index_from"    (fun i s n c -> try Some (mk_i i  (String.index_from s n c)) with | Not_found -> None)
  ; pmk_sicio  "string_rindex_from"   (fun i s n c -> try Some (mk_i i (String.rindex_from s n c)) with | Not_found -> None)
  ; pmk_ssi    "string_compare"       (fun _ -> String.compare)

  (* input/output/sys operations *)
  ; pmk_ss     "read"                 (fun _ fn -> Misc.read fn)
  ; pmk_ssu    "write"                (fun _ fn s -> Misc.write fn s)
  ; pmk_ss     "exec"                 (fun _ c -> Misc.exec c)
  ; pmk_sb     "file_exists"          (fun _ -> Sys.file_exists)
  ; pmk_sb     "is_directory"         (fun _ s -> try Sys.is_directory s with Sys_error _ -> false)
  ; pmk_su     "remove"               (fun _ -> Sys.remove)
  ; pmk_ssb    "rename"               (fun _ s1 s2 -> try Sys.rename s1 s2; true with Sys_error _ -> false)
  ; pmk_us     "getcwd"               (fun _ -> Sys.getcwd)
  ; pmk_s      "os_type"              Sys.os_type
                                      
  (* regexp operations *)             
  ; pmk_sr     "regexp_of_string"     (fun _ -> Brx.mk_string)
  ; pmk_r      "empty"                Brx.empty
  ; pmk_rb     "is_empty"             (fun _ -> Brx.is_empty)
  ; pmk_rrr    "regexp_concat"        (fun _ -> Brx.mk_seq)
  ; pmk_rrr    "regexp_union"         (fun _ -> Brx.mk_alt)
  ; pmk_rrr    "diff"                 (fun _ -> Brx.mk_diff)
  ; pmk_rrr    "inter"                (fun _ -> Brx.mk_inter)
  ; pmk_riir   "regexp_iter"          (fun _ -> Brx.mk_iter)
  ; pmk_rrb    "equiv"                (fun _ -> Brx.equiv)
  ; pmk_rrx    "equiv_cex"            (fun _ t1 t2 -> 
                                         if Brx.equiv t1 t2 then None
                                         else match Brx.representative (Brx.mk_diff t1 t2) with 
                                           | Some w1 -> Some(sprintf "'%s' and '%s' are not equivalent; [%s] is in the first but not the second" 
                                                               (Brx.string_of_t t1) (Brx.string_of_t t2) w1)
                                           | None -> match Brx.representative (Brx.mk_diff t2 t1) with 
                                               | Some w2 -> Some(sprintf "'%s' and '%s' are not equivalent; [%s] is in the second but not the first" 
                                                                   (Brx.string_of_t t1) (Brx.string_of_t t2) w2)
                                               | None -> raise (Error.Harmony_error (fun () -> msg "equiv_cex: cannot calculate representative")))
  ; pmk_rs     "representative"       wrap_rep
  ; pmk_rr     "reverse"              (fun _ -> Brx.mk_reverse)
  ; pmk_rsi    "count"                (fun _ r s -> 
                                         Safelist.length (Brx.star_split r s))
  ; pmk_rsb    "matches"              (fun _ r s -> Brx.match_string r s)
  ; pmk_rsx    "matches_cex"          (fun _ r s -> 
                                         if Brx.match_string r s then None
                                         else 
                                           let s1,s2 = Brx.split_bad_prefix r s in 
                                           Some (sprintf "string does not match %s [%s] AROUND HERE [%s]" (Brx.string_of_t r) s1 s2))
  ; pmk_rrb    "splittable"           (fun _ -> Brx.splittable)
  ; pmk_rrx    "splittable_cex"       (fun _ t1 t2 -> match Brx.splittable_cex t1 t2 with
                                         | Misc.Left(w1,w2,w1',w2') -> 
                                             Some (sprintf "'%s' is ambiguously splittable into [%s] [%s] and [%s] [%s]" 
                                                     (w1^w2) w1 w2 w1' w2')
                                         | _ -> None)
  ; pmk_rb     "iterable"             (fun _ -> Brx.iterable)
  ; pmk_rx     "iterable_cex"         (fun _ t -> match Brx.iterable_cex t with
                                         | Misc.Left(w1,w2,w1',w2') -> 
                                             Some (sprintf "'%s' is ambiguously iterable: splits into [%s] [%s] and [%s] [%s]" 
                                                     (w1^w2) w1 w2 w1' w2')
                                         | _ -> None)
  ; pmk_rssz   "star_split"           (fun i r s -> Safelist.map (mk_s i) (Brx.star_split r s))
  ; pmk_rrb    "disjoint"             (fun _ -> Brx.disjoint)
  ; pmk_rrx    "disjoint_cex"         (fun _ t1 t2 -> 
                                         match Brx.disjoint_cex t1 t2 with
					   | Some(cex) ->   
					       Some (sprintf "'%s' and '%s' are not disjoint: '%s' is in the intersection" 
                                                       (Brx.string_of_t t1) 
                                                       (Brx.string_of_t t2) cex)
					   | None -> None)
  ; pmk_rsr    "derivative"           (fun _ t1 s -> Brx.derivative t1 s)
  ; pmk_rsi    "bad_prefix_position"  (fun _ -> Brx.bad_prefix_position)

  (* annotated regexp operations *)             
  ; pmk_aab    "aequiv"               (fun _ -> Barx.equiv)
  ; pmk_aax    "aequiv_cex"           (fun _ -> Barx.equiv_cex)
  ; pmk_aaa    "aregexp_concat"       (fun _ -> Barx.mk_seq)
  ; pmk_aaa    "aregexp_union"        (fun _ -> Barx.mk_alt)
  ; pmk_aiia   "aregexp_iter"         (fun _ -> Barx.mk_iter)
  ; pmk_tax    "aregexp_match_compatible_cex" (fun _ -> Barx.match_compatible_cex)
  ; pmk_aax    "aregexp_compatible_cex" (fun _ -> Barx.compatible_cex)
  ; pmk_taa    "aregexp_match"        (fun _ -> Barx.mk_box)
  ; pmk_ra     "rxlift"               (fun _ -> Barx.mk_rx)
  ; pmk_ar     "rxdrop"               (fun _ -> Barx.rxtype)
  ; pmk_biaa   "aregexp_weight"       (fun _ b w -> Barx.annot_weight (b, Bannot.Weight.of_int w))
  ; pmk_ab     "no_chunks"            (fun _ -> Barx.no_chunks)
    
  (* boolean operations *)            
  ; pmk_xxx    "land"                 (fun _ x1 x2 -> 
					 match x1,x2 with
					     None,None -> None
					   | None,Some cex
					   | Some cex,None -> Some cex
					   | Some cex1,Some cex2 -> 
                                               Some(sprintf "[%s] and [%s]"
                                                      cex1 cex2))
  ; pmk_xxx    "lor"                  (fun _ x1 x2 ->
				         match x1,x2 with
					   | Some cex1,Some cex2 -> 
                                               Some (sprintf "[%s] and [%s]"
                                                       cex1 cex2)
                                           | _ -> None)
  ; pmk_bb     "not"                  (fun _ -> not)
                                      
  (* run-time checking *)             
  ; pmk_la     "astype"               (fun _ -> L.astype)
  ; pmk_la     "avtype"               (fun _ -> L.avtype)
  ; pmk_lr     "stype"                (fun _ -> L.stype)
  ; pmk_lr     "vtype"                (fun _ -> L.vtype)
  ; pmk_lk     "ktype"                (fun _ -> L.ktype)
  ; pmk_lm     "mtype"                (fun _ -> L.mtype)
  ; pmk_tkmx   "mtype_match_compatible_cex" (fun _ -> Blenses.mtype_match_compatible_cex)
  ; pmk_mmx    "mtype_compatible_cex" (fun _ -> Blenses.mtype_compatible_cex)
  ; pmk_mmb    "mtype_domain_equal"   (fun _ -> Blenses.mtype_domain_equal)
  ; pmk_lss    "srep"                 (fun _ ml s -> L.srep ml (Bstring.of_string s))
  ; pmk_lss    "vrep"                 (fun _ ml s -> L.vrep ml (Bstring.of_string s))
  ; pmk_lb     "bij"                  (fun _ -> L.bij)

  ; pmk_lq     "canonizer_of_lens"    L.canonizer_of_t
  ; pmk_lb     "sequiv_identity"      (fun _ -> L.sequiv_identity)
  ; pmk_lb     "vequiv_identity"      (fun _ -> L.vequiv_identity)
  ; pmk_qb     "cnrel_identity"       (fun _ -> C.cnrel_identity)

  ; pmk_rs     "string_of_regexp"     (fun _ r1 -> Brx.string_of_t r1)

  (* prefs *)
  ; pmk_us     "prefs_get_prog_name"     (fun _ () -> Sys.argv.(0))

  ; pmk_sbsbP  "prefs_create_bool"       (fun _ -> fulldoc Prefs.createBool)
  ; pmk_bPsu   "prefs_alias_bool"        (fun _ -> Prefs.alias)
  ; pmk_bPb    "prefs_read_bool"         (fun _ -> Prefs.read)

  ; pmk_sisiP  "prefs_create_int"        (fun _ -> fulldoc Prefs.createInt)
  ; pmk_iPsu   "prefs_alias_int"         (fun _ -> Prefs.alias)
  ; pmk_iPi    "prefs_read_int"          (fun _ -> Prefs.read)

  ; pmk_ssssP  "prefs_create_string"     (fun _ -> fulldoc Prefs.createString)
  ; pmk_sPsu   "prefs_alias_string"      (fun _ -> Prefs.alias)
  ; pmk_sPs    "prefs_read_string"       (fun _ -> Prefs.read)

  ; pmk_ssszP  "prefs_create_string_list"(fun _ name doc -> Prefs.createStringList name doc "")
  ; pmk_szPsu  "prefs_alias_string_list" (fun _ -> Prefs.alias)
  ; pmk_szPszR "prefs_read_string_list"  (fun _ -> Prefs.read)

  ; pmk_su     "prefs_print_usage"       (fun i -> Prefs.printUsage)

  ; pmk_usP    "prefs_extern_output"     (fun i () -> Prefs.outputPref)
  ; pmk_uszP   "prefs_extern_lens"       (fun i () -> Prefs.lensPref)
  ; pmk_uszP   "prefs_extern_source"     (fun i () -> Prefs.sourcePref)
  ; pmk_uszP   "prefs_extern_view"       (fun i () -> Prefs.viewPref)
  ; pmk_uszP   "prefs_extern_expression" (fun i () -> Prefs.expressionPref)
  ; pmk_uszP   "prefs_extern_rest"       (fun i () -> Prefs.restPref)
  ; pmk_uszP   "prefs_extern_check"      (fun i () -> Prefs.checkPref)
  ; pmk_uszP   "prefs_extern_include"    (fun i () -> Prefs.includePref)
  ; pmk_uszP   "prefs_extern_test"       (fun i () -> Prefs.testPref)
  ; pmk_ubP    "prefs_extern_testall"    (fun i () -> Prefs.testallPref)
  ; pmk_uszP   "prefs_extern_debug"      (fun i () -> Prefs.debugPref)
  ; pmk_ubP    "prefs_extern_debugtimes" (fun i () -> Prefs.debugtimesPref)
  ; pmk_ubP    "prefs_extern_log"        (fun i () -> Prefs.logPref)
  ; pmk_usP    "prefs_extern_logfile"    (fun i () -> Prefs.logfilePref)
  ; pmk_ubP    "prefs_extern_terse"      (fun i () -> Prefs.tersePref)
  ; pmk_ubP    "prefs_extern_timers"     (fun i () -> Prefs.timersPref)
  ; pmk_ubP    "prefs_extern_colorize"   (fun i () -> Prefs.colorizePref)

  (* sync *)
(*   ; pmk_sync   "sync"                 (fun i l o a b ->  *)
(*                                          let mk_s s = Bvalue.Str(i,s) in  *)
(*                                          let mk_p v1 v2 = Bvalue.Par(i,v1,v2) in *)
(*                                          let xt = match L.xtype l with  *)
(*                                            | Some xt -> xt *)
(*                                            | None ->  *)
(*                                                raise  *)
(*                                                  (Error.Harmony_error *)
(*                                                     (fun () ->  *)
(*                                                        msg "%s: cannot synchronize with %s." *)
(*                                                          (Info.string_of_t i) *)
(*                                                          (L.string_of_t l))) in                                             *)
(*                                          let acts,o',a',b' = Bsync.sync xt o a b in  *)
(*                                          let s_acts = mk_s acts in *)
(*                                          let s_o,s_a,s_b = mk_s o',mk_s a',mk_s b' in  *)
(*                                          mk_p (mk_p (mk_p s_acts s_o) s_a) s_b) *)

  (* polymorphic functions *)
  ; begin 
    let i = Info.M "poly_equal built-in" in 
    let a = Id.mk i "a" in 
    let a_sort = S.SVar a in 
    (Qid.mk_native_prelude_t i "poly_equal",
     S.SForall(a,a_sort ^> a_sort ^> S.SBool),
     mk_ufun i (fun () -> 
       mk_f i (fun v1 -> 
         mk_f i (fun v2 -> 
           mk_b i (equal v1 v2)))))
  end
 
  ; begin
    let i = mk_prelude_info "poly_show" in
    let a = Id.mk i "a" in
    let a_sort = S.SVar a in 
    (Qid.mk_native_prelude_t i "poly_show",
     S.SForall(a,a_sort ^> S.SString),
     mk_ufun i (fun () ->
       mk_f i (fun v ->
         mk_s i (string_of_t v))))
  end
 
  ; begin 
    let i = mk_prelude_info "fold_left" in 
    let nil = Id.mk (mk_prelude_info "Nil") "Nil" in 
    let cons = Id.mk (mk_prelude_info "Cons") "Cons" in 
    let a = Id.mk i "a" in 
    let a_sort = S.SVar a in 
    let b = Id.mk i "b" in 
    let b_sort = S.SVar b in 
    let b_list_sort = S.SData([b_sort],Qid.mk_list_t (Qid.info_of_t Bvalue.list_qid) "t") in      
    (Qid.mk_native_prelude_t i "fold_left",
     S.SForall(a,S.SForall(b, ((a_sort ^> b_sort ^> a_sort) ^> 
                                 a_sort ^> b_list_sort ^> a_sort))),
     let b = mk_prelude_info "fold_left" in 
     mk_f b (fun achk -> 
       mk_f b (fun bchk -> 
         mk_f b (fun f0 -> 
           mk_f b (fun acc0 -> 
             mk_f b (fun l0 ->
               let f = get_f f0 in 
               let rec aux l acc = 
               let lbl,vo = get_v l in 
               if Id.equal lbl nil then acc
               else if Id.equal lbl cons then 
                 let hd,tl = match vo with 
                   | None -> poly_error b b_list_sort l
                   | Some v -> get_p v in                
                 let acc' = (get_f (f None acc)) None hd in 
                   aux tl acc'
               else 
                 poly_error b b_list_sort l in 
                 aux l0 acc0))))))
  end 

  ; begin
    let i = mk_prelude_info "valid_permutation" in
    let a = Id.mk i "a" in
    let a_sort = S.SVar a in
    let int_list_sort = S.SData([S.SInteger],Qid.mk_list_t (Qid.info_of_t Bvalue.list_qid) "t") in
    let a_list_sort = S.SData([a_sort],Qid.mk_list_t (Qid.info_of_t Bvalue.list_qid) "t") in
    (Qid.mk_native_prelude_t i "valid_permutation",
     S.SForall(a,int_list_sort ^> a_list_sort ^> S.SBool),
     mk_f i (fun achk ->
       mk_f i (fun sigma ->
           mk_f i (fun l0 ->
	     let sigma = Safelist.map get_i (get_list sigma) in
	       mk_b i (P.valid_permutation sigma
                         (get_list l0))))))
  end

  ; begin
    let i = mk_prelude_info "list_permute" in
    let a = Id.mk i "a" in
    let a_sort = S.SVar a in
    let int_list_sort = S.SData([S.SInteger],Qid.mk_list_t (Qid.info_of_t Bvalue.list_qid) "t") in
    let a_list_sort = S.SData([a_sort],Qid.mk_list_t (Qid.info_of_t Bvalue.list_qid) "t") in
    (Qid.mk_native_prelude_t i "list_permute",
     S.SForall(a,int_list_sort ^> a_list_sort ^> a_list_sort),
     mk_f i (fun achk ->
       mk_f i (fun sigma ->
           mk_f i (fun l0 ->
	     let sigma = Safelist.map get_i (get_list sigma) in
	       mk_list i (P.permute_list sigma
                            (get_list l0))))))
  end

  ; begin
    let i = mk_prelude_info "list_sort" in
    let a = Id.mk i "a" in
    let a_sort = S.SVar a in
    let a_list_sort = S.SData ([a_sort], Qid.mk_list_t (Qid.info_of_t Bvalue.list_qid) "t") in
    let a_compare_sort = a_sort ^> a_sort ^> S.SInteger in
    Qid.mk_native_prelude_t i "list_sort",
    S.SForall (a, a_compare_sort ^> a_list_sort ^> a_list_sort),
    mk_f i (fun achk -> mk_f i (fun cmp0 -> mk_f i (fun l0 ->
    let cmp a1 a2 = get_i (get_f (get_f cmp0 None a1) None a2) in
    let l = get_list l0 in
    mk_list i (List.sort cmp l))))
 end
  ]

let () =
  Safelist.iter
    (fun (x,s,v) -> Bregistry.register_native_qid x s v)
    prelude_spec
