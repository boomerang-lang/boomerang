(*****************************************************)
(* The Harmony Project                               *)
(* harmony@lists.seas.upenn.edu                      *)
(*                                                   *)
(* error.mli - interface for run-time exceptions     *)
(*****************************************************)
(* $Id: error.mli 3437 2007-12-09 22:41:31Z jnfoster $ *)

(** Exceptions used throughout Harmony *)

exception Harmony_error of (unit -> unit)
  (** A [Harmony_error f] is the only exception raised in Harmony
      programs.  Information about the error can be printed by
      applying the function carried with each exception. By
      convention, the errors are printed using functions from OCaml's
      Format module *)

val simple_error : string -> 'a
  (** [simple_error s] raises a [Harmony_error] that prints [s], which
      should be a short string. *)

val exit_on_error : (unit -> 'a) -> 'a
  (** [exit_on_error f] runs [f ()] and handles errors by printing a
      description of the error to [stderr] and exiting. *)

