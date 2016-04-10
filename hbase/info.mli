(*******************************************************************************)
(* The Harmony Project                                                         *)
(* harmony@lists.seas.upenn.edu                                                *)
(*                                                                             *)
(* info.mli - interface of error reporting locations                           *)
(*******************************************************************************)
(* $Id: info.mli 3437 2007-12-09 22:41:31Z jnfoster $ *)

(** {2 File location information} *)

type pos = int * int
(** An [Info.pos] represents a pin-point location in a file *)

type t = I of string * pos * pos | M of string
(** [t] represents a location that spans a chunk of a file, or a message if no precise location is available. *)

val string_of_t : t -> string 
(** [string_of_t] pretty prints a location for easy parsing by [compile-mode] in [emacs] *)

val merge_inc : t -> t -> t
(** [merge_inc i1 i2] merges the locations [i1] and [i2] into a new
    location; includes the endpoints. *)

val merge_exc : t -> t -> t
(** [merge_exc i1 i2] merges the locations [i1] and [i2] into a new
    location; excludes the endpoints. *)
