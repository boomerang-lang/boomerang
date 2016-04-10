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
(* /src/berror.ml                                                             *)
(* Boomerang errors                                                           *)
(* $Id: berror.ml 4607 2009-08-03 16:53:28Z ddavi $ *)
(******************************************************************************)

let msg = Util.format

(* static errors in the interpreter *)
let static_error i msg_thk = raise
  (Error.Harmony_error
     (fun () -> 
        msg "@[%s:@ static error@\n" (Info.string_of_t i);
        msg_thk ();
        msg "@]@\n"))

(* checked run-time errors, triggered when contracts fail *)
let blame_error i msg_thk = raise 
  (Error.Harmony_error
     (fun () -> 
        msg "@[%s:@ run-time@ checking@ error@\n" (Info.string_of_t i);
        msg_thk ();
        msg "@]"))

(* unexpected run-time errors *)
let run_error i msg_thk = raise
  (Error.Harmony_error
     (fun () -> 
        msg "@[%s:@ unexpected@ run-time@ error@\n" (Info.string_of_t i);
        msg_thk ();
        msg "@]"))

