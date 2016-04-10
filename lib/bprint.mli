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
(* /src/bprint.mli                                                            *)
(* Boomerang pretty printing interface                                        *)
(* $Id: bprint.mli 4607 2009-08-03 16:53:28Z ddavi $ *)
(******************************************************************************)
open Bsyntax

(** {2 Pretty Printing using formatting functions} *)

val nlify : string -> unit
(** [nlify s] pretty prints [s] in a formatting box. *)

val maybe_wrap : ('a -> unit) -> bool -> 'a -> unit
  (** [maybe_wrap fmt b x] pretty prints [x] using [fmt], and adds
      parens if [b]. *)

val format_sort : sort -> unit 
  (** [format_sort s] pretty prints [s] using [Util.format]. *)

val format_pat : pat -> unit
(** [format_pat p] pretty prints [p] using [Util.format]. *)

val format_param : param -> unit
(** [format_param p] pretty prints [p] using [Util.format]. *)

val format_binding : binding -> unit
(** [format_binding b] pretty prints [b] using [Util.format]. *)

val format_exp : exp -> unit
(** [format_exp e] pretty prints [e] using [Util.format]. *)

val format_op : op -> unit
(** [format_op o] pretty prints [o] using [Util.format]. *)

val format_test_result : test_result -> unit
(** [format_test_result tr] pretty prints [tr] using [Util.format]. *)

val format_decl : decl -> unit
(** [format_decl d] pretty prints [d] using [Util.format]. *)

val format_module : modl -> unit
(** [format_module m] pretty prints [m] using [Util.format]. *)

(** {2 Pretty Printing to strings} *)

val string_of_sort : sort -> string
(** [string_of_sort s] pretty prints [s] to a string. *)

val string_of_pat : pat -> string
(** [string_of_pat p] pretty prints [p] to a string. *)

val string_of_param : param -> string
(** [string_of_param p] pretty prints [p] to a string. *)

val string_of_binding : binding -> string
(** [string_of_binding b] pretty prints [b] to a string. *)

val string_of_exp : exp -> string
(** [string_of_exp e] pretty prints [e] to a string. *)

val string_of_op : op -> string
(** [string_of_op o] pretty prints [o] to a string. *)

val string_of_test_result : test_result -> string
(** [string_of_test_result tr] pretty prints [tr] to a string. *)

val string_of_decl : decl -> string
(** [string_of_decl d] pretty prints [d] to a string. *)

val string_of_module : modl -> string
(** [string_of_module m] pretty prints [m] to a string. *)
