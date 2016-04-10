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
(* /src/registry.mli                                                          *)
(* Boomerang run-time registry interface                                      *)
(* $Id: bregistry.mli 4646 2009-09-10 14:33:53Z jnfoster $ *)
(******************************************************************************)

(** {2 Parsing helper functions for constructing values.} *)
val parse_uid : string -> Bident.Qid.t
(** [parse_uid s] parses a [Qid.t] from an uppercase string. *)

val parse_qid : string -> Bident.Qid.t
  (** [parse_qid s] parses a [Qid.t] from [s]. *)

(** {2 Registry of Boomerang values } *)

type rs = 
  | Sort of Bsyntax.sort      
  | Unknown 
(** The type of registry sorts: either a sort, a scheme, or unknown *)

type rv = rs * Bvalue.t
(** The type of registry values: an [rs] and a value. *)

val value_of_rv : rv -> Bvalue.t
(** [value_of_rv r] returns the value from [r]. *)

val format_rv : rv -> unit
(** [format_rv r] pretty prints [r] *)

type tcon = Bident.Qid.t * Bsyntax.sort option
type tspec = Bident.Id.t list * tcon list 

(** {2 Library} *)
module REnv : sig 
  type t
  val empty : unit -> t
  val lookup : t -> Bident.Qid.t -> rv option
  val lookup_both : t -> Bident.Qid.t -> (rv * t) option
  val lookup_type: t -> Bident.Qid.t -> (Bident.Qid.t * tspec) option
  val lookup_con : t -> Bident.Qid.t -> (Bident.Qid.t * tspec) option
  val update : t -> Bident.Qid.t -> rv -> t
  val update_type : t -> Bident.Id.t list -> Bident.Qid.t -> tcon list -> t
  val iter : (Bident.Qid.t -> rv -> unit) -> t -> unit
  val iter_type : (Bident.Qid.t -> tspec -> unit) -> t -> unit
  val fold : (Bident.Qid.t -> rv -> 'a -> 'a) -> t -> 'a -> 'a
end

val reset : unit -> unit
(** Resets the library. *)

val pre_ctx : Bident.Qid.t list
(** the initial naming context, i.e., ["Core" ; "Prelude" ] *)

val get_library : unit -> REnv.t
(** Returns the library, as an environment. *)

val register_env : REnv.t -> Bident.Qid.t list -> Bident.Id.t -> unit
(** [register_env ev nctx m] registers the environment [ev] under the module name [m], resolving
    identifies in the naming context [nctx]*)

val register_native_qid : Bident.Qid.t -> Bsyntax.sort -> Bvalue.t -> unit
(** ?? *)

val register_native : string -> Bsyntax.sort -> Bvalue.t -> unit
(** ?? *)

val extensions : string list
(** ?? *)

val modl_of_path : string -> string
(** ?? *)

val load : string -> bool
(** ?? *)

val load_file : string -> bool
(** ?? *)

val lookup_library_ctx : Bident.Qid.t list -> Bident.Qid.t -> rv option
(** [lookup_library_ctx nctx q] looks up [q] from the library, using naming context [nctx] *)

val lookup_both_library_ctx : Bident.Qid.t list -> Bident.Qid.t -> (rv * REnv.t) option
(** [lookup_both_library_ctx nctx q] looks up [q] from the library, using naming context [nctx] *)

val lookup_library_ctx_o : Bident.Qid.t list -> Bident.Qid.t -> (Bident.Qid.t * rv) option
(** [lookup_library_ctx_o nctx q] looks up [q] from the library, using naming context [nctx] *)

val lookup_library : Bident.Qid.t -> rv option
(** [lookup_library q] looks up [q] from the library *)

val lookup_type_library_ctx : Bident.Qid.t list -> Bident.Qid.t -> (Bident.Qid.t * tspec) option
(** [lookup_library_ctx nctx q] looks up [q] from the library, using naming context [nctx] *)

val lookup_type_library : Bident.Qid.t -> (Bident.Qid.t * tspec) option
(** [lookup_library q] looks up [q] from the library *)

val lookup_con_library_ctx : Bident.Qid.t list -> Bident.Qid.t -> (Bident.Qid.t * tspec) option
(** [lookup_library_ctx nctx q] looks up [q] from the library, using naming context [nctx] *)

val lookup_con_library : Bident.Qid.t -> (Bident.Qid.t * tspec) option
(** [lookup_library q] looks up [q] from the library *)

(**/**)
val interp_file_impl : (string -> string -> unit) ref
val interp_string_impl : (string -> string -> string -> unit) ref
