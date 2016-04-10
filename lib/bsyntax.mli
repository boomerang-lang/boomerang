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
(* /src/bsyntax.mli                                                           *)
(* Boomerang abstract syntax interface                                        *)
(* $Id: bsyntax.mli 4998 2011-03-16 21:53:34Z mgree $ *)
(******************************************************************************)

open Bident

type prefs =
  | PrBool
  | PrInt
  | PrString
  | PrStringList

val string_of_prefs : prefs -> string

(** {2 Boomerang Abstract Syntax} *)
type sort = 
    (* base sorts *)
    | SUnit                           (* unit *)
    | SBool                           (* booleans *)
    | SInteger                        (* integers *)
    | SChar                           (* chars *)
    | SString                         (* strings *)
    | SRegexp                         (* regular expressions *)
    | SAregexp                        (* annotated regular expressions *)
    | SSkeletons                      (* skeleton set type *)
    | SResources                      (* resource set type *)
    | SLens                           (* lenses *)
    | SCanonizer                      (* canonizers *)
    | SPrefs of prefs                 (* prefs *)

    (* products and sums *)
    | SProduct of sort * sort         (* products *)
    | SData of sort list * Qid.t      (* data types *)

    (* dependent function types *)
    | SFunction of Id.t * sort * sort 
    | SRefine of Id.t * bool * sort * exp (* refinement types [bool=true -> mandatory, not to be checked in parallel] *)
    | SVar of Id.t                    (* variables *)
    | SForall of Id.t * sort          (* universals *)
 
(* parameters *)
and param = Param of Info.t * Id.t * sort

(* variable bindings *)
and binding = Bind of Info.t * pat * sort option * exp 

(* grammars *)
and rule = Rule of Info.t * exp list * exp list * (Id.t * exp) list

and prod = Prod of Info.t * Id.t * rule list

(* expressions *)
and exp = 
    (* lambda calculus *)
    | EApp  of Info.t * exp * exp 
    | EVar  of Info.t * Qid.t 
    | EOver of Info.t * op * exp list 
    | EFun  of Info.t * param * sort option * exp 
    | ELet  of Info.t * binding * exp 

    (* or rather... System F *)
    | ETyFun of Info.t * Id.t * exp 
    | ETyApp of Info.t * exp * sort

    (* with products, case *)
    | EPair of Info.t * exp * exp 
    | ECase of Info.t * exp * (pat * exp) list * sort option

    (* casts *)
    | ECast    of Info.t * sort * sort * Info.t * exp
        
    (* unit, strings, ints, character sets *)
    | EUnit    of Info.t  
    | EInteger of Info.t * int    
    | EChar    of Info.t * char
    | EString  of Info.t * string
    | ECSet    of Info.t * bool * (char * char) list 

    (* booleans with counter examples *)
    (* None ~ true; Some s ~ false with counterexample s *)
    | EBoolean of Info.t * exp option 

    (* grammar *)
    | EGrammar of Info.t * prod list
(** Expression abstract syntax. *)

and op = 
  | OIter of int * int 
  | ODot
  | OTilde
  | OMinus
  | OBar
  | OAmp
  | OBarBar
  | OAmpAmp
  | ODarrow
  | ODeqarrow
  | OEqual
  | OLt
  | OLeq
  | OGt
  | OGeq
  | OMatch
  | OWeight
(** Overloaded operator abstract syntax. *)

and pat = 
  | PWld of Info.t
  | PUnt of Info.t
  | PBol of Info.t * bool
  | PCex of Info.t * pat
  | PInt of Info.t * int
  | PStr of Info.t * string
  | PVar of Info.t * Id.t * sort option 
  | PVnt of Info.t * Qid.t * pat option 
  | PPar of Info.t * pat * pat
(** Pattern abstract syntax. *)
    
type test_result =
    | TestError
    | TestPrint
    | TestEqual of exp
    | TestSortPrint of sort option
    | TestSortEqual of sort option * sort
(** Unit test abstract syntax. *)

type decl = 
    | DLet  of Info.t * binding
    | DType of Info.t * Id.t list * Qid.t * (Id.t * sort option) list 
    | DMod  of Info.t * Id.t * decl list 
    | DTest of Info.t * exp * test_result
(** Declaration abstract syntax. *)

type modl = Mod of Info.t * Id.t * Qid.t list * decl list
(** Module abstract syntax: the name of the module, a list of "open"
    modules, and a list of declarations. *)
  
val (^>) : sort -> sort -> sort
  (** [s1 ^> s2] is the function sort from [s1] to [s2]. *)

val (^*) : sort -> sort -> sort
(** [s1 ^* s2] is the product sort between [s1] and [s2]. *)

val info_of_exp : exp -> Info.t
(** [info_of_exp e] returns the parsing info associated to expression [e]. *)

val info_of_prod : prod -> Info.t
(** [info_of_prod p] returns the parsing info associated to production [p]. *)

val info_of_rule : rule -> Info.t
(** [info_of_rule r] returns the parsing info associated to rule [r]. *)

val labels_of_rule : rule -> Id.Set.t
(** [labels_of_rule r] returns the set of labels associated to rule [r]. *)

val qlabels_of_rule : rule -> Qid.Set.t
(** [labels_of_rule r] returns the set of qualified labels associated to rule [r]. *)

val info_of_pat : pat -> Info.t
(** [info_of_pat p] returns the parsing info associated to pattern [p]. *)

val info_of_module : modl -> Info.t
(** [info_of_module m] returns the parsing info associated to module [m]. *)

val id_of_module : modl -> Bident.Id.t
(** [id_of_module m] returns the name of module [m]. *)

val sort_of_param : param -> sort
(** [sort_of_param p] returns the sort declared with parameter [p]. *)

val id_of_param : param -> Bident.Id.t
(** [sort_of_param p] returns the name of parameter [p]. *)

val pat_of_binding : binding -> pat
(** [pat_of_binding b] returns the name of the variable bound in [b]. *)

val exp_of_binding : binding -> exp
(** [exp_op_binding p] returns the expression of binding [b]. *)

val sl_of_svl : Bident.Id.t list -> sort list
(** [sl_of_svl l] converts the sort variable list [l] to a sort list. *)

val is_refined : sort -> bool
(**[is_refined s] returns true iff s is a refined type *)

(* ------ constructors ----- *)
val mk_unit : Info.t -> exp 
val mk_int : Info.t -> int -> exp 
val mk_string : Info.t -> string -> exp 
val mk_app : Info.t -> exp -> exp -> exp
val mk_app3 : Info.t -> exp -> exp -> exp -> exp
val mk_app4 : Info.t -> exp -> exp -> exp -> exp -> exp
val mk_tyapp : Info.t -> exp -> sort -> exp
val mk_let : Info.t -> Id.t -> sort -> exp -> exp -> exp
val mk_fun : Info.t -> Id.t -> sort -> exp -> exp
val mk_if : Info.t -> exp -> exp -> exp -> sort -> exp 
val mk_native_prelude_var : Info.t -> string -> exp
val mk_prelude_var : Info.t -> string -> exp

val mk_qid_var : Qid.t -> exp
val mk_var : Id.t -> exp
val mk_core_var : Info.t -> string -> exp
val mk_list_var : Info.t -> string -> exp
val mk_over : Info.t -> op -> exp list -> exp
val mk_bin_op : Info.t -> exp -> exp -> exp -> exp
val mk_tern_op : Info.t -> exp -> exp -> exp -> exp -> exp
val mk_cat : Info.t -> exp -> exp -> exp
val mk_iter : Info.t -> int -> int -> exp -> exp
val mk_acond : Info.t -> exp -> exp -> exp
val mk_cond : Info.t -> exp -> exp -> exp
val mk_swap : Info.t -> exp -> exp -> exp
val mk_diff : Info.t -> exp -> exp -> exp
val mk_inter : Info.t -> exp -> exp -> exp
val mk_compose : Info.t -> exp -> exp -> exp
val mk_set : Info.t -> exp -> exp -> exp
val mk_rx : Info.t -> exp -> exp
