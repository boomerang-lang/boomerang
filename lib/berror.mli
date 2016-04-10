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
(* /src/error.mli                                                             *)
(* Boomerang errors interface                                                 *)
(* $Id: berror.mli 4607 2009-08-03 16:53:28Z ddavi $ *)
(******************************************************************************)

(* static error in the interpreter *)
val static_error : Info.t -> (unit -> unit) -> 'a

(* checked run-time errors triggered when a contract fails *)
val blame_error : Info.t -> (unit -> unit) -> 'a

(* unexpected run-time errors *)
val run_error : Info.t -> (unit -> unit) -> 'a
