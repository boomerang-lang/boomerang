(******************************************************************************)
(* The Harmony Project                                                        *)
(* harmony@lists.seas.upenn.edu                                               *)
(******************************************************************************)
(* Copyright (C) 2007-2008                                                    *)
(* J. Nathan Foster and Benjamin C. Pierce                                    *)
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
(* /src/bprint.ml                                                             *)
(* Boomerang printing                                                         *)
(* $Id: bprint.ml 4998 2011-03-16 21:53:34Z mgree $ *)
(******************************************************************************)

(* ----- imports and abbreviations ----- *)
open Bsyntax
open Bident
let msg = Util.format
let to_string = Util.format_to_string

(* ----- generic helpers ----- *)

(* format a string, converting newlines to @\n *)
let nlify s = Misc.format_list "@\n" 
  (Util.format "%s") 
  (Misc.split_nonescape '\n' s)

let maybe_wrap fmt b r = 
  if b then msg "(";
  fmt r;
  if b then msg ")"

(* ----- formatters for abstract syntax trees ----- *)  
let rec format_sort = function
  | SUnit -> msg "@[unit@]"
  | SBool -> msg "@[bool@]"
  | SInteger -> msg "@[int@]"
  | SChar    -> msg "@[char@]"
  | SString -> msg "@[string@]"
  | SRegexp -> msg "@[regexp@]"
  | SAregexp -> msg "@[aregexp@]"
  | SSkeletons -> msg "@[skeleton_set@]"
  | SResources -> msg "@[resource_set@]"
  | SLens -> msg "@[lens@]"
  | SCanonizer -> msg "@[canonizer@]"
  | SPrefs p -> msg "@[%sPrefs@]" (string_of_prefs p)
      
  | SFunction(x0, s1, s2) ->
      msg "@[(";
      if not (Id.equal x0 Id.wild)
      then msg "%s:@," (Id.string_of_t x0);
      format_sort s1;
      msg "@ ->@ ";
      format_sort s2;
      msg ")@]"
        
  | SProduct(s1,s2) -> 
      msg "@[<2>(";
      format_sort s1;
      msg "@ *@ ";
      format_sort s2;
      msg ")@]"

  | SData([],q1) -> msg "@[%s@]" (Qid.string_of_t q1)

  | SData([s],q1) -> 
      msg "@[";
      format_sort s;
      msg "@ ";
      msg "%s@]" (Qid.string_of_t q1)

  | SData(ms,q1) ->         
      msg "@[<2>(@[<2>";
      Misc.format_list ",@ " (format_sort) ms;      
      msg "@])@ %s@]" (Qid.string_of_t q1)

  | SVar(x) -> msg "@['%s@]" (Id.string_of_t x)

  | SForall(x,s) -> 
      msg "@[<2>(forall %s@ =>@ " (Id.string_of_t x);
      format_sort s;
      msg ")@]";

  | SRefine(x0,b0,s0,e0) ->
      msg "@[(";
      if not (Id.equal x0 Id.wild)
      then msg "%s:@," (Id.string_of_t x0);
      format_sort s0;
      msg "@ where%s@ " (if b0 then "!" else "");
      format_exp e0;
      msg ")@]"

and format_pat p0 = match p0 with 
  | PWld _ -> msg "_"
  | PUnt _ -> msg "()"
  | PInt(_,n) -> msg "%d" n
  | PBol(_,b) -> msg "%b" b
  | PCex(_,p) -> msg "@[cex@ "; format_pat p; msg "@]"
  | PStr(_,s) -> msg "%s" s
  | PVar(_,x,_) -> msg "%s" (Id.string_of_t x)
  | PPar(_,p1,p2) -> 
      msg "@[<2>(";
      format_pat p1;
      msg ",@,";
      format_pat p2;
      msg ")@]";
  | PVnt(_,l,None) -> msg "%s" (Qid.string_of_t l)
  | PVnt(_,l,Some p1) ->  
      msg "@[<2>(%s@ " (Qid.string_of_t l);
      format_pat p1;
      msg ")@]"

and format_param p0 = match p0 with
  | Param(_,x,s) ->
      msg "@[(%s:" (Id.string_of_t x);      
      format_sort s;
      msg ")@]"

and format_binding b0 = match b0 with
  | Bind (_,p,so,e) ->
      msg "@[";
      format_pat p;
      (match so with None -> () | Some s -> msg "@ :@ "; format_sort s);
      msg "@ =@ ";
      format_exp e;
      msg "@]"
        
and format_exp e0 = match e0 with 
  | EApp (_,e1,e2) ->
      msg "@[<2>(";
      format_exp e1;
      msg "@ ";
      format_exp e2;
      msg ")@]"

  | EVar(_,q) -> 
      msg "@[%s@]" (Qid.string_of_t q)
	
  | EOver(_,OWeight,[b;w;e]) ->
      msg "@[{:%a%a:%a}@]" (fun _ -> format_exp) b (fun _ -> format_exp) w (fun _ -> format_exp) e

  | EOver(_,OMatch,[t;e]) ->
      msg "@["; msg "<%a:%a>@]" (fun _ -> format_exp) t (fun _ -> format_exp) e

  | EOver(_,op,e1::rest) -> 
      msg "@[<2>(";
      format_exp e1;
      (match rest with 
         | [] -> 
             msg ")";
             format_op op;
             msg "@]"
         | _ -> 
             msg " ";
             format_op op;
             msg " ";
             Misc.format_list "" format_exp rest; (* HACK! *)
             msg ")@]")

  | EOver _ -> assert false
      
  | EFun (_,p,s,e) ->
      msg "@[<2>(fun@ ";
      format_param p;
      (match s with
	 | None -> ()
         | Some s -> msg "@ :@ "; format_sort s);
      msg "@ ->@ ";
      format_exp e;
      msg ")@]";
      
  | ELet (_,b,e) ->
      msg "@[<2>let ";
      format_binding b;
      msg "@ in@ ";
      format_exp e;
      msg "@]";

  | ETyFun(_,x,e) -> 
      msg "@[<2>(tyfun@ %s@ ->@ '" (Id.string_of_t x);
      format_exp e;
      msg ")@]"
	
  | ETyApp(_,e,s) -> 
      msg "@[<2>";
      format_exp e;
      msg "@,{@["; 
      format_sort s; 
      msg "@]}@]"
	
  | EUnit _ -> msg "()"
      
  | EPair(_,e1,e2) -> 
      msg "@[<2>(@[";
      format_exp e1;
      msg ",@,";
      format_exp e2;
      msg "@])@]"
	
  | ECase(_,e1,pl,s) -> 
      msg "@[<2>(match@ ";
      format_exp e1;
      msg "@ with@ ";
      Misc.format_list "@ | "
        (fun (p,e) -> 
           msg "@[<2>";
           format_pat p;
           msg "@ ->@ ";
           format_exp e;
           msg "@]")
        pl;
      msg ")@]";
      begin match s with
	| Some s ->
	    msg ")@ :@ ";
	    format_sort s;
	    msg "@]"
	| None ->
	    msg ")@]"
      end

  | ECast(_,f,t,b,e) -> 
      msg "@[<2><|";
      format_sort t;
      msg "@ <=@ ";
      format_sort f;
      msg "|>@ ";
      format_exp e;
      msg "@]"

  | EBoolean (_,None) -> 
      msg "@[true@]"

  | EBoolean (_,Some (EString(_,""))) ->
      msg "@[false@]"

  | EBoolean (_,Some e) ->
      msg "@[false (with counterexample: ";
      format_exp e;
      msg "@]"

  | EChar(_,c) -> msg "'%s'" (Char.escaped c)

  | EString (_,s) ->
      msg "@[\"%s\"@]" (String.escaped s)

  | EInteger (_,i) ->
      msg "@[%d@]" i

  | ECSet (_,pos, ranges) ->
      msg "@[[";
      (if pos then () else msg "^");
      Misc.format_list ""
	(fun (first, last) ->
	   if first = last
	   then msg "%s" (Char.escaped first)
	   else msg "%s-%s" 
	     (Char.escaped first)
	     (Char.escaped last))
	ranges;
      msg "]@]"

  | EGrammar(_,ps) ->
      msg "@[grammar";
      Misc.format_list "@\nand@ " format_prod ps;
      msg "end@]"

and format_op = function
  | OIter(0,-1) -> msg "*"
  | OIter(1,-1) -> msg "+"
  | OIter(0,1)  -> msg "?"
  | OIter(m,-1) -> msg "{%d,}" m
  | OIter(m,n)  -> 
      if m=n then 
        msg "{%d}" m
      else
        msg "{%d,%d}" m n
  | ODot -> msg "."
  | OTilde -> msg "~"
  | OMinus -> msg "-"
  | OBar -> msg "|"
  | OAmp -> msg "&"
  | OBarBar -> msg "||"
  | OAmpAmp -> msg "&&"
  | ODarrow -> msg "<->" 
  | ODeqarrow -> msg "<=>" 
  | OEqual  -> msg "="
  | OLt     -> msg "<"
  | OLeq     -> msg "<="
  | OGt     -> msg ">"
  | OGeq    -> msg ">="
  | OMatch -> msg "< : >"
  | OWeight -> msg "{: : }"

and format_prod (Prod(_,x,rs)) =
  msg "@[%s@ ::= @[" (Id.string_of_t x);
  Misc.format_list "@\n|@ " format_rule rs;
  msg "@]@]"

and format_rule (Rule(_,xs,ys,bs)) =
  msg "@[";
  Misc.format_list "" format_exp xs;
  msg "@ <-> @ ";
  Misc.format_list "" format_exp ys;
  msg "@\nwhere@ ";
  Misc.format_list "@\nand@ "
    (fun (li,ei) -> msg "@[%s:" (Id.string_of_t li); format_exp ei; msg "@]") bs;
  msg "@]"

and format_test_result tr =
  match tr with
    | TestEqual e -> 
        msg "= @[<2>" ; 
        format_exp e;
        msg "@]";
    | TestError -> msg "= @[<2>error@]"
    | TestPrint -> msg "= @[<2>?@]"
    | TestSortPrint _ -> msg ": @[<2>?@]"
    | TestSortEqual(_,s) -> 
        msg ": @[<2>";
        format_sort s;
        msg "@]"

and format_decl = function
    DLet (_,b) ->
      msg "@[<2>let ";
      format_binding b;
      msg "@]"

  | DType(_,xl,x,cl) ->  
      msg "@[<2>type@ ";
      (match xl with 
         | [] -> ()
         | [x] -> msg "%s" (Id.string_of_t x)
         | _ -> 
             msg "(";
             Misc.format_list ",@ " (fun xi -> msg "%s" (Id.string_of_t xi)) xl;
             msg ")");
      msg "@ %s@ =@ " (Qid.string_of_t x);
      Misc.format_list " | "
        (fun (l,s) -> match s with
           | None -> msg "%s" (Id.string_of_t l)
           | Some s -> 
               msg "(%s@ " (Id.string_of_t l);
               format_sort s;
               msg ")")
        cl;
      msg "@]"        

  | DMod (i,m,ds) ->
      format_module (Mod (i,m,[], ds))

  | DTest (_,e,tr) ->
      msg"@[<2>test@ @[";
      format_exp e;
      msg "@ ";
      format_test_result tr;
      msg "@]@]"

and format_module = function
  | Mod (_,m,qs,ds) ->
      msg "@[module %s =@\n  @[" (Id.string_of_t m);
      if qs <> [] then 
        Misc.format_list "@\n" 
          (fun x -> msg "open %s" (Qid.string_of_t x))
          qs;
      Misc.format_list "@\n" format_decl ds;
      msg "@\n@]@\n@]"

(* ----- conversions to string ----- *)  
let string_of_exp e = to_string (fun () -> format_exp e)
let string_of_binding b = to_string (fun () -> format_binding b)
let string_of_decl d = to_string (fun () -> format_decl d)
let string_of_test_result tr = to_string (fun () -> format_test_result tr)
let string_of_op o = to_string (fun () -> format_op o)
let string_of_module m = to_string (fun () -> format_module m)
let string_of_sort s = to_string (fun () -> format_sort s)
let string_of_param p = to_string (fun () -> format_param p)
let string_of_pat p = to_string (fun () -> format_pat p)
