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
(* /src/bdriver.ml                                                            *)
(* Boomerang compiler driver                                                  *)
(* $Id: bdriver.ml 4621 2009-08-10 18:52:36Z cretin $ *)
(******************************************************************************)

(* ---------------------------------------------------------------------------*)
(* IMPORTS AND ABBREVIATIONS *)

open Bsyntax
open Bident
open Bcheck
open Binterp

let msg = Util.format 

(* ---------------------------------------------------------------------------*)
(* EXPORTS *)

(* parse an AST from a lexbuf *)
let parse_lexbuf lexbuf = 
  try Bparser.modl Blexer.main lexbuf with 
    | Parsing.Parse_error ->
        raise 
          (Error.Harmony_error 
             (fun () -> msg "@[%s:@ syntax@ error@\n@]"
                (Info.string_of_t (Blexer.info lexbuf))))

(* helper to check the name of a module *)
let m_check n m_str ast =
  let n_base = String.uncapitalize (Bregistry.modl_of_path n) in
  let m_low = String.uncapitalize m_str in
  if n_base = m_low then ()
  else
    Berror.static_error
      (info_of_module ast)
      (fun () -> 
         msg "@[module@ %s@ must@ appear@ in@ a@ file@ named %s.boom.@]"
           m_str m_low)
      
(* end-to-end interpretation of a lexbuf *)
let interp_lexbuf lexbuf n = 
  let ast = parse_lexbuf lexbuf in
  let m_str = Id.string_of_t (id_of_module ast) in 
  let _ = m_check n m_str ast in
  let instrumented_ast = check_module ast in 
  let _ = interp_module instrumented_ast in
    ()

(* end-to-end interpretation of a string *)
let interp_src_string fn s n = 
  let _ = Blexer.setup fn in 
  let lexbuf = Lexing.from_string (Src2fcl.fcl_of_src_str s) in
  let _ = interp_lexbuf lexbuf n in 
  Blexer.finish ()

(* end-to-end interpretation of a file  *)
let interp_src_file fn n = 
  let boom_buf = Src2fcl.fcl_of_src fn in  
  let _ = Blexer.setup fn in
  let lexbuf = Lexing.from_string boom_buf in 
  let _ = interp_lexbuf lexbuf n in
  Blexer.finish ()

(* force loading of the interpreter *)
let init () = 
  Bregistry.interp_file_impl := interp_src_file;
  Bregistry.interp_string_impl := interp_src_string;
