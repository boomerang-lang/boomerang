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
(* /src/registry.ml                                                           *)
(* Boomerang run-time registry                                                *)
(* $Id: bregistry.ml 5001 2015-12-05 03:05:11Z bcpierce $ *)
(******************************************************************************)

let sprintf = Printf.sprintf
let debug = Trace.debug "registry"
let verbose = Trace.debug "registry+"
let msg = Util.format

(* finite maps *)
module QM = Bident.Qid.Env

(* --------------- Identifier parsing -------------- *)
let parse_uid s =  
  let lexbuf = Lexing.from_string s in
    Blexer.setup "identifier constant";
    let x = 
      try Bparser.uid Blexer.main lexbuf
      with _ -> 
        raise 
          (Error.Harmony_error
             (fun () -> 
                msg "@[%s:@ syntax@ error@ in@ identifier@ %s.@]" 
                  (Info.string_of_t (Blexer.info lexbuf))
                  s)) in 
      Blexer.finish ();                    
      x

let parse_qid s =
  let lexbuf = Lexing.from_string s in
    Blexer.setup "qualified identifier constant";
    let q =
      try Bparser.qid Blexer.main lexbuf
      with _ -> raise
        (Error.Harmony_error
           (fun () ->
              msg "@[%s:@ syntax@ error@ in@ qualified@ identifier@ %s.@]"
                (Info.string_of_t (Blexer.info lexbuf))
                s)) in
      Blexer.finish ();
      q

(* --------------- Registry values -------------- *)
type rs = 
  | Sort of Bsyntax.sort      
  | Unknown 

type rv = rs * Bvalue.t

(* utility functions *)

let value_of_rv (_,v) = v

let format_rv (rs,v) = 
  msg "@[";
  Bvalue.format v;
  (match rs with
    | Sort(s)     -> msg ":@ ";  Bprint.format_sort s
    | Unknown     -> ());
    msg "@]"

(* type abbreviation for the constructors for a type *)
type tcon = Bident.Qid.t * Bsyntax.sort option
type tspec = Bident.Id.t list * tcon list

(* --------------- Boomerang library -------------- *)
(* state *)
module REnv = struct  
  (* "type map" from names (Prelude.list) to type variables (['a]) and
     constructors / optional sorts (["Nil", None; "Cons", 'a * 'a list]) *)
  type tmap = (Bident.Id.t list * tcon list) QM.t

  (* "reverse type map" from constructor names (Prelude.Nil) to types (Prelude.list) *)
  type rmap = Bident.Qid.t QM.t 

  type vmap = VMap of (rv * vmap) QM.t

  (* environments have two components: for types and values *)
  and t = (tmap * rmap * vmap)

  let empty () = (QM.empty (), QM.empty (), VMap (QM.empty ()))

  let lookup (_,_,VMap ve) q = match QM.lookup ve q with
    | Some (r,_) -> Some r
    | None -> None

  let lookup_both (tm,rm, VMap ve) q = match QM.lookup ve q with
    | Some (r,ve) -> Some (r,(tm,rm,ve))
    | None -> None

  let lookup_type (tm,_,_) q = match QM.lookup tm q with
    | Some r -> Some (q,r)
    | None -> None
        
  let lookup_con (tm,rm,_) q = match QM.lookup rm q with 
    | Some q' -> begin 
        match QM.lookup tm q' with 
          | None -> 
              Berror.run_error (Info.M "Bregistry.lookup_con") 
                (fun () -> msg "datatype %s missing" 
                   (Bident.Qid.string_of_t q'))
          | Some (sl,cl) -> 
              Some (q',(sl,cl))
      end
    | None -> None
  
  let update (tm,rm, VMap ve) q v = 
    (tm, rm, VMap (QM.update ve q (v,VMap ve)))

  let update_type (tm,rm,VMap ve) svl q cl = 
    let tm' = QM.update tm q (svl,cl) in 
    let rm' = 
      Safelist.fold_left 
        (fun rmi (qi,so) -> QM.update rmi qi q)
        rm cl in 
    (tm',rm',VMap ve)

  let iter f (_,_,VMap ve) = 
    QM.iter (fun q rvve -> f q (fst rvve)) ve 

  let iter_type f (tm,_,_) = QM.iter f tm 

  let fold f (_,_,VMap ve) init = 
    QM.fold (fun q rvve a -> f q (fst rvve) a) ve init 

end

let loaded = ref ["Native"]

let library : REnv.t ref = ref (REnv.empty ())

(* constants *)
let pre_ctx = Safelist.map parse_uid [ "Core" ; "Prelude" ]

(* utilities *)
let get_library () = !library

(* hack to reset library to native stuff in visualizer *)
let old_library = ref None
let reset () =
  if !old_library = None then old_library := Some !library;
  loaded := [];
  library := 
    match !old_library with 
        Some l -> l
      | None -> assert false

(* --------------- Lookup functions -------------- *)

let paths = Prefs.includePref

let boompath =
  try Util.splitIntoWords (Unix.getenv "BOOMPATH") ':'
  with Not_found -> ["."]

(* get the filename that a module is stored at *)
let get_module_prefix q = 
  match Bident.Qid.qs_of_t q with 
    | [] -> None
    | n::_ -> Some n

let extensions = ["boom";"src"]

let find_file modl path =
  if Sys.file_exists path && Misc.is_file path
  then (
    verbose (fun () -> msg "%s found for %s@\n" path modl);
    true
  ) else (
    verbose (fun () -> msg "%s not found@\n" path);
    false
  )

let find_modl modl = 
  let rec loop dirs =
    match dirs with
    | [] -> None
    | dir::drest ->
        let try_path ext k =
          let to_path basename =
            sprintf "%s%s%s%s%s"
              dir
              (if dir.[String.length dir - 1] = '/' then "" else "/")
              basename
              (if String.length ext = 0 then "" else ".")
              ext
          in
          let try_name name k =
            let path = to_path name in
            if find_file modl path
            then Some path
            else k ()
          in
          try_name (String.uncapitalize modl)
            (fun () -> try_name modl k)
        in
        let rec inner_loop exts =
          match exts with
          | [] -> loop drest
          | ext::erest ->
              try_path ext (fun () -> inner_loop erest)
        in
        inner_loop extensions
  in
  loop (Safelist.rev (Prefs.read paths) @ boompath)

(* backpatch hack *)
let interp_file_impl = 
  ref (fun _ _ -> 
         msg "@[Boomerang compiler is not linked! Exiting...@]"; 
         exit 2)

let interp_string_impl = 
  ref (fun _ _ _ -> 
         msg "@[Boomerang compiler is not linked! Exiting...@]"; 
         exit 2)  

let modl_of_path path =
  let chop_ext path =
    try Filename.chop_extension path
    with Invalid_argument _ -> path
  in
  String.capitalize (chop_ext (Filename.basename path))
  
let go_load comp modl source =
  debug (fun () -> msg "[@[loading %s ...@]]@\n%!" source);
  loaded := modl::(!loaded);
  comp ();
  debug (fun () -> msg "[@[loaded %s@]]@\n%!" source)

let load_file path =
  let modl = modl_of_path path in
  if Safelist.mem modl !loaded
  then true
  else (
    let exist = find_file modl path in
    if exist
    then go_load (fun () -> !interp_file_impl path modl) modl path;
    exist
  )

let load modl =
  if Safelist.mem modl !loaded
  then true
  else (
    match find_modl modl with 
    | None -> (
        (* check for baked in source *)
        let safe_find m =
          try Some (Hashtbl.find Bakery.items m)
          with Not_found -> None
        in
        let i = sprintf "<baked source for %s>" modl in  
        match (
          match safe_find modl with
          | None -> safe_find (String.uncapitalize modl)
          | x -> x
        ) with
        | None -> false
        | Some str ->
            go_load (fun () -> !interp_string_impl i str modl) modl i;
            true
      )
    | Some fn ->
        go_load (fun () -> !interp_file_impl fn modl) modl fn;
        true
  )
      
let load_var q = match get_module_prefix q with 
  | None -> ()
  | Some n -> ignore (load (Bident.Id.string_of_t n))

(* lookup in a naming context, with a lookup function *)
let lookup_library_generic select lookup_fun nctx q = 
  verbose (fun () -> msg "lookup_library_generic [%s] [%s]@\n%!" 
           (Bident.Qid.string_of_t q)
           (Misc.concat_list "," (Safelist.map Bident.Qid.string_of_t nctx)));
  let rec lookup_library_aux nctx q2 =       
    verbose (fun () -> msg "lookup_library_aux [%s] [%s]@\n%!" 
             (Bident.Qid.string_of_t q2)
             (Misc.concat_list "," (Safelist.map Bident.Qid.string_of_t nctx)));
    let try_lib () = lookup_fun !library q2 in
      (* try here first, to avoid looping on native values *)
      match try_lib () with
        | Some r -> Some (select (q2, r))
        | None -> 
            begin match load_var q2; try_lib () with
              | Some r -> Some (select (q2, r))
              | None -> match nctx with 
                  | []       -> None
                  | o::orest -> 
                      lookup_library_aux orest (Bident.Qid.t_dot_t o q) 
            end
  in
  lookup_library_aux nctx q

let lookup_library_ctx os q = 
  verbose (fun () -> msg "lookup_library_ctx [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_library_generic snd REnv.lookup os q

let lookup_both_library_ctx os q = 
  verbose (fun () -> msg "lookup_both_library_ctx [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_library_generic snd REnv.lookup_both os q

let lookup_library_ctx_o os q = 
  verbose (fun () -> msg "lookup_library_ctx_o [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_library_generic (fun x -> x) REnv.lookup os q

let lookup_type_library_ctx os q = 
  verbose (fun () -> msg "lookup_type_library_ctx [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_library_generic snd REnv.lookup_type os q

let lookup_con_library_ctx os q = 
  verbose (fun () -> msg "lookup_con_library_ctx [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_library_generic snd REnv.lookup_con os q

let lookup_library q = 
  verbose (fun () -> msg "lookup_library [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_library_ctx [] q

let lookup_type_library q = 
  verbose (fun () -> msg "lookup_type_library [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_type_library_ctx [] q

let lookup_con_library q = 
  verbose (fun () -> msg "lookup_con_library [%s]@\n%!" (Bident.Qid.string_of_t q));
  lookup_con_library_ctx [] q

(* --------------- Registration functions -------------- *)

(* lookup in a naming context, with a lookup function *)
let resolve_library lookup_fun nctx m q = 
  verbose (fun () -> msg "resolve_library [%s] [%s] in [%s]@\n" 
           (Bident.Qid.string_of_t q)
           (Misc.concat_list "," (Safelist.map Bident.Qid.string_of_t nctx))
	   (Bident.Id.string_of_t m));
  let rec resolve_library_aux nctx q2 =       
    verbose (fun () -> msg "resolve_library_aux [%s] [%s]@\n" 
             (Bident.Qid.string_of_t q2)
             (Misc.concat_list "," (Safelist.map Bident.Qid.string_of_t nctx)));
    match lookup_fun !library q2 with
      | Some r -> q2
      | None -> begin match nctx with 
          | []       -> Bident.Qid.id_dot m q
          | o::orest -> 
              resolve_library_aux orest (Bident.Qid.t_dot_t o q) 
        end
  in
  resolve_library_aux nctx q

(* --------------- Registration functions -------------- *)
(* register a value *)
let register q r = 
  library := (REnv.update (!library) q r)

let register_type q (svl,cl) = 
  library := (REnv.update_type (!library) svl q cl)

let register_native_qid q s v = 
  register q (Sort s,v)

(* register a native value *)
let register_native qs s v = 
  register_native_qid (parse_qid qs) s v

(* register a whole (rv Env.t) in m *)
let register_env ev nctx m = 
  let qualify_rv (rs,v) =
    let new_rs = match rs with
      | Sort s -> 
          Sort (Bsubst.qualify_sort (resolve_library REnv.lookup nctx m) [] s)
      | Unknown -> Unknown in
    (new_rs,v) in
  REnv.iter (fun q r -> register (Bident.Qid.id_dot m q) (qualify_rv r)) ev;  
  REnv.iter_type 
    (fun q ts -> 
       let (svl,cl) = ts in 
       let cl' = Safelist.map (fun (x,so) -> (Bident.Qid.id_dot m x,so)) cl in 
       register_type q (svl,cl')) ev
