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
(* /src/toplevel.ml                                                           *)
(* Boomerang front-end                                                        *)
(* $Id: toplevel.ml 4998 2011-03-16 21:53:34Z mgree $ *)
(******************************************************************************)

(* imports *)
module L = Blenses.MLens
open Error 

let sprintf = Printf.sprintf
let msg = Util.format

let () = 
  (* initialize unison state (sets directory name) *)
  Util.supplyFileInUnisonDirFn (fun s -> sprintf "./.%s/%s" "boomerang" s);
  (* turn off logging *)
  Prefs.set Trace.logging false;
  (* redirect tracing and debugging output to stdout *)
  Trace.redirect `FormatStdout;
  (* hack to ensure that Boomerang compiler gets linked *)
  Bdriver.init()
    
let exit x = 
  Brx.print_stats ();
  exit x

(* Debugging *)
let debug thk = Trace.debug "toplevel" (fun () -> thk (); Util.format "%!")

let debug_sync thk = Trace.debug "sync" (fun () -> thk (); Util.format "%!")

(* Registry lookup helpers *)
let lookup qid_str = Bregistry.lookup_library (Bregistry.parse_qid qid_str)

let lookup_lens qid_str =
  match lookup qid_str with
      None -> Error.simple_error (Printf.sprintf "lens %s not found" qid_str)
    | Some rv -> Bvalue.get_l (Bregistry.value_of_rv rv)
          

(* Filesystem helpers *)          
let read_string fn = 
  debug (fun () -> Util.format "Reading %s@\n" fn);
  Misc.read fn

let write_string fn s = 
  debug (fun () -> Util.format "Writing back %s@\n" fn);
  Misc.write fn s
    
(*********)
(* CHECK *)
(*********)
let modl_p x =
  let mk_cset (azs, cs) =
    let azscs =
      Safelist.fold_left
        (fun l (a, z) -> (Char.code a, Char.code z)::l)
        (Safelist.rev_map
           (fun c -> let cc = Char.code c in cc, cc)
           cs)
        azs
    in
    Brx.mk_cset azscs
  in
  let first = [('A','Z')], ['\'';'_';'-';'@'] in
  let rest = ('a','z')::('0','9')::(fst first), snd first in
  let modlrx =
    Brx.mk_seq (mk_cset first) (Brx.mk_star (mk_cset rest))
  in
  Brx.match_string modlrx x

let file_p x =
  Safelist.exists
    (Filename.check_suffix x)
    Bregistry.extensions

let check x =
  let xtype, load =
    if file_p x then "file", Bregistry.load_file
    else if modl_p x then "module", Bregistry.load
    else raise
      (Error.Harmony_error 
         (fun () -> 
            Util.format "Error: %s is neither a file or a module@\n" x))
  in
  if not (load x)
  then raise
    (Error.Harmony_error 
       (fun () -> 
          Util.format
            "Error: could not find %s %s@\n"
            xtype
            x))
      
let check_str n r x = 
  if not (Brx.match_string r x) then 
    Berror.run_error (Info.M n)
      (fun () -> 
         let s1,s2 = Brx.split_bad_prefix r x in 
           msg "@[string does not match %s [%s] AROUND HERE [%s]@]" 
             (Brx.string_of_t r) 
             s1 
             s2)

let get_str l s = 
  check_str "get" (L.stype l) s;
  L.rget l (Bstring.of_string s)  (* TODO: push this up to read_string *)

let put_str l v s = 
  check_str "put" (L.vtype l) v;
  check_str "put" (L.stype l) s;
  L.rput l (Bstring.of_string v) (Bstring.of_string s)

let create_str l v =
  check_str "create" (L.vtype l) v;
  L.rcreate l (Bstring.of_string v)

(************)
(* RUN MAIN *)
(************)
let run_main modl =  (* Note: the contracts of main are not checked *)
  let fmap f o =
    match o with
    | None -> None
    | Some x -> Some (f x)
  in
  let run main =
    let i = Info.M "argv in main built-in" in
    (match fst main with
     | Bregistry.Sort (Bsyntax.SFunction (_, Bsyntax.SUnit, Bsyntax.SInteger)) -> Bvalue.get_i
     | Bregistry.Sort (Bsyntax.SFunction (_, Bsyntax.SUnit, Bsyntax.SUnit)) -> (fun v -> Bvalue.get_u v; 0)
     | _ -> Error.simple_error (Printf.sprintf "%s.main does not have type unit -> int or unit -> unit.\n" modl)
    ) (Bvalue.get_f (Bregistry.value_of_rv main) None (Bvalue.mk_u i ()))
  in
  fmap run (lookup (modl ^ ".main"))

(*******)
(* GET *)
(*******)
let get l_n c_fn o_fn = 
  write_string o_fn (get_str (lookup_lens l_n) (read_string c_fn));
  0

(*******)
(* PUT *)
(*******)
let put l_n a_fn c_fn o_fn = 
  write_string o_fn 
    (put_str (lookup_lens l_n) (read_string a_fn) (read_string c_fn));
  0

(**********)
(* CREATE *)
(**********)
let create l_n a_fn o_fn = 
  write_string o_fn (create_str (lookup_lens l_n) (read_string a_fn));
  0

(********)
(* SYNC *)
(********)
(* let sync l_n o_fn a_fn b_fn o_fn' a_fn' b_fn' =    *)
(*   let l = lookup_lens l_n in  *)
(*   let read f = *)
(*     if Sys.file_exists f then Some (read_string f) *)
(*     else None in *)
(*   let oc, ac, bc = read o_fn, read a_fn, read b_fn in  *)
(*   let oa = Misc.map_option (get_str l) oc in  *)
(*   let aa = Misc.map_option (get_str l) ac in  *)
(*   let ba = Misc.map_option (get_str l) bc in  *)
(*   let xt = match L.xtype l with *)
(*     | Some xt -> xt      *)
(*     | None ->  *)
(*         Berror.run_error (Info.M "sync") *)
(*           (fun () -> msg "cannot synchronize with %s" (L.string_of_t l)) in *)
(*   let acts,oa',aa',ba' = Bsync.sync_opt xt oa aa ba in  *)
(*   let write_str fn vo so = match vo,so with  *)
(*     | None,_ -> Misc.remove_file_or_dir fn; *)
(*     | Some v, None -> write_string fn (create_str l v)  *)
(*     | Some v, Some s -> write_string fn (put_str l v s) in  *)
(*   Bprint.nlify acts; *)
(*   write_str o_fn' oa' oc; *)
(*   write_str a_fn' aa' ac; *)
(*   write_str b_fn' ba' bc; *)
(*   (\* Return non-zero exit code if any conflicts were detected *\) *)
(*   if aa' = ba' then 0 else 1 *)
  
(* (\* OLD SYNC *\) *)
(* let archive_fn n = Util.fileInUnisonDir (sprintf ".#%s" n) *)
(* let oldsync l o_fn c_fn a_fn =  *)
(*   let o_fn = if o_fn = "" then archive_fn c_fn else o_fn in  *)
(*   match Sys.file_exists o_fn, Sys.file_exists c_fn, Sys.file_exists a_fn with  *)
(*     | _,false,false ->  *)
(*         (\* if neither c nor a exists, clear o and return *\) *)
(*         debug_sync (fun () -> Util.format "replicas %s and %s missing; removing archive %s@\n" c_fn a_fn o_fn); *)
(*         Misc.remove_file_or_dir o_fn; *)
(*         0 *)

(*     | _,true,false ->  *)
(*         (\* if c exists but a does not, set a to GET c *\) *)
(*         debug_sync (fun () -> Util.format "(lens: %s) %s -- get --> %s\n" l c_fn a_fn); *)
(*         let c = read_string c_fn in  *)
(*           write_string o_fn c; *)
(*           write_string a_fn (get_str (lookup_lens l) c); *)
(*           0 *)
          
(*     | true,false,true -> *)
(*         (\* if c does not exist but a and o do, set c and o to PUT a o *\)         *)
(*         debug_sync (fun () -> Util.format "(lens: %s) %s <-- put -- %s %s\n" l c_fn a_fn o_fn); *)
(*         let a = read_string a_fn in *)
(*         let o = read_string o_fn in  *)
(*         let c' = put_str (lookup_lens l) a o in  *)
(*         let a' = get_str (lookup_lens l) c' in  *)
(*           write_string c_fn c'; *)
(*           write_string a_fn a'; *)
(*           write_string o_fn c'; *)
(*           0 *)
    
(*     | false,false,true -> *)
(*         (\* if c and o do not exist but a does, set c and o to CREATE a *\)         *)
(*         debug_sync (fun () -> Util.format "(lens: %s) %s <-- create -- %s\n" l c_fn a_fn); *)
(*         let a = read_string a_fn in  *)
(*         let c' = create_str (lookup_lens l) a in  *)
(*         let a' = get_str (lookup_lens l) c' in  *)
(*           write_string o_fn c'; *)
(*           write_string a_fn a'; *)
(*           write_string c_fn c'; *)
(*           0 *)

(*     | false,true,true ->  *)
(*         (\* if c and a exist but o does not and a <> GET c then conflict; otherwise set o to c *\) *)
(*         let a = read_string a_fn in  *)
(*         let c = read_string c_fn in  *)
(*         let a' = get_str (lookup_lens l) c in  *)
(*         if a = a' then  *)
(*           begin *)
(*             debug_sync (fun () -> Util.format "(lens: %s) setting archive %s to concrete replica %s\n" l o_fn c_fn); *)
(*             write_string o_fn c; *)
(*             0 *)
(*           end *)
(*         else *)
(*           begin  *)
(*             debug_sync (fun () -> Util.format "(lens: %s) %s --> conflict <-- %s\n" l c_fn a_fn);           *)
(*             1 *)
(*           end *)

(*     | true,true,true ->  *)
(*         (\* otherwise, c, a, and o exist: *)
(*            - if a=GET o set a to GET c and o to c; *)
(*            - else if c=o, set c and o to PUT a o; *)
(*            - otherwise conflict. *\) *)
(*         let o = read_string o_fn in *)
(*         let c = read_string c_fn in  *)
(*         let a = read_string a_fn in  *)
(*         let a' = get_str (lookup_lens l) c in  *)
(*         if a = a' then  *)
(*           begin  *)
(*             debug_sync (fun () -> Util.format "(lens: %s) setting archive %s to concrete replica %s\n" l o_fn c_fn); *)
(*             write_string o_fn c; *)
(*             0 *)
(*           end *)
(*         else *)
(*           let a' = get_str (lookup_lens l) o in  *)
(*             if a = a' then  *)
(*               begin  *)
(*                 debug_sync (fun () -> Util.format "(lens: %s) %s -- get --> %s\n" l c_fn a_fn);             *)
(*                 write_string a_fn (get_str (lookup_lens l) c); *)
(*                 write_string o_fn c; *)
(*                 0 *)
(*               end *)
(*             else if c = o then *)
(*               begin  *)
(*                 debug_sync (fun () -> Util.format "(lens: %s) %s <-- put -- %s %s\n" l c_fn a_fn o_fn); *)
(*                 let c' = put_str (lookup_lens l) a o in  *)
(*                 let a' = get_str (lookup_lens l) c' in  *)
(*                   write_string o_fn c'; *)
(*                   write_string a_fn a'; *)
(*                   write_string c_fn c'; *)
(*                   0 *)
(*               end             *)
(*             else  *)
(*               begin  *)
(*                 debug_sync (fun () -> Util.format "(lens: %s) %s --> conflict <-- %s\n" l c_fn a_fn); *)
(*                 1 *)
(*               end *)

let toplevel' progName () = 
  let baseUsageMsg = (* update the documentation (main.src) after changing this *)
    "Usage:\n"
    ^ "    "^progName^" [get] l S             [options] : get\n"
    ^ " or "^progName^" [put] l V S           [options] : put\n"
    ^ " or "^progName^" create l V            [options] : create\n"
    (* ^ " or "^progName^" sync l O A B          [options] : sync\n" *)
    (* ^ " or "^progName^" sync l O A B O' A' B' [options] : sync\n" *)
    ^ " or "^progName^" F.boom [F.boom...]    [options] : run unit tests\n"
    ^ "\n" in 
  let shortUsageMsg = 
    baseUsageMsg 
    ^ "For a list of options, type \"" ^progName^ " -help\".\n" in 
  let usageMsg = baseUsageMsg ^ "Options:" in 

  let bad_cmdline () = Util.format "%s" shortUsageMsg; exit 2 in 

  (* run a special module if asked to *)
  begin
    let basename prog =
      let basename = Filename.basename prog in
      try Filename.chop_extension basename
      with Invalid_argument _ -> basename
    in
    let modl basename =
      let prefix = "boomerang" in
      let prefix_len = String.length prefix in
      let basename_len = String.length basename in
      if basename_len >= prefix_len && String.sub basename 0 prefix_len = prefix
      then None
      else Some (String.capitalize basename)
    in
    let prog = Sys.argv.(0) in
    match modl (basename prog) with
    | None -> ()
    | Some modl ->
        check modl;
        let usage =
          match lookup (modl ^ ".usage") with
          | Some (_, Bvalue.Str (_, s)) -> s
          | _ -> ""
        in
        Prefs.parseCmdLine usage;
        match run_main modl with
        | None -> Error.simple_error (Printf.sprintf "%s does not contain a main function.\n" modl)
        | Some code -> exit code
  end;

  (* Parse command-line options *)
  let o_pref = Prefs.outputPref in
  let l_pref = Prefs.lensPref in
  let s_pref = Prefs.sourcePref in
  let v_pref = Prefs.viewPref in
  let e_pref = Prefs.expressionPref in
  let rest = Prefs.restPref in
  let check_pref = Prefs.checkPref in

  Prefs.parseCmdLine usageMsg;
  
  (* Read preferences *)
  let ll = Safelist.rev (Prefs.read l_pref) in 
  let sl = Safelist.rev (Prefs.read s_pref) in 
  let vl = Safelist.rev (Prefs.read v_pref) in 
  let el = Safelist.rev (Prefs.read e_pref) in
  let o = Prefs.read o_pref in
  let rest_pref = Safelist.rev (Prefs.read rest) in 

  (* run unit tests if needed *)
  if Prefs.read check_pref <> [] then
    begin
      Safelist.iter check (Prefs.read check_pref);
      if Prefs.read rest = [] then exit 0
    end;
  Util.finalize 
  (fun () -> 
     if rest_pref <> [] && 
       (Safelist.for_all 
          (fun x -> file_p x || modl_p x)
          rest_pref)
     then begin
       (* barf on spurious command line options?! *)
       Prefs.set Binterp.test_all true;
       Safelist.iter check rest_pref;
       0
     end else if el <> []
     then begin (* dash e *)
       match el with
       | [exp] -> !Bregistry.interp_string_impl "<OnTime module ('-e' argument)>" ("module OnTime =\n" ^ exp) "OnTime"; 0
       | [] -> assert false
       | _ -> bad_cmdline ()
     end else begin 
       let rest_pref,ll,sl,vl,o = match rest_pref,ll,sl,vl,o with 
         (* get *)
         | [l;s_fn],[],[],[],o
         | [s_fn],[l],[],[],o 
         | ["get"],[l],[s_fn],[],o
         | ["get"; s_fn],[l],[],[],o   
         | ["get"; l],[],[s_fn],[],o   
         | ["get"; l; s_fn],[],[],[],o -> 
             ["get"],[l],[s_fn],[],o             
         (* create *)
         | ["create"],[l],[],[v_fn],o
         | ["create"; l],[],[],[v_fn],o
         | ["create"; v_fn],[l],[],[],o 
         | ["create"; l; v_fn],[],[],[],o ->             
             ["create"],[l],[],[v_fn],o
         (* put *)
         | [l; v_fn; s_fn],[],[],[],o 
         | [v_fn; s_fn],[l],[],[],o 
         | ["put"],[l],[s_fn],[v_fn],o
         | ["put"; v_fn; s_fn],[l],[],[],o 
         | ["put"; l],[],[s_fn],[v_fn],o
         | ["put"; l; v_fn; s_fn],[],[],[],o -> 
             ["put"],[l],[s_fn],[v_fn],o
         | _ -> bad_cmdline () in 
       let o_fn = if o="" then "-" else o in 
       match rest_pref,ll,sl,vl with
         | ["get"],[l],[s_fn],[]        -> get l s_fn o_fn
         | ["create"],[l],[],[v_fn]     -> create l v_fn o_fn
         | ["put"],[l],[s_fn],[v_fn]    -> put l v_fn s_fn o_fn
         | _ -> assert false
     end)
    (fun () -> Util.flush ())
    
let toplevel progName =
  try
    exit 
      (Unix.handle_unix_error 
         (fun () -> Error.exit_on_error (toplevel' progName))
         ())
  with e -> 
    Printf.printf "Uncaught exception %s" (Printexc.to_string e); 
    exit 2
