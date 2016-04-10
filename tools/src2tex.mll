(*
 * ML lex script to convert .src files to .tex files.
 *)

{
  let pr = print_string

  type mode = TEX | SRC | BOTH
  type lineMode = TEX | SRC | BOTH | CHARS | NONE

  let mode = ref SRC
  and lineMode = ref NONE
  and newLine = ref true
  and needsVerbatim = ref false
  and inVerbatim = ref false

  let shouldPrint () =
    (!lineMode <> SRC && !lineMode <> NONE) ||
    (!lineMode = NONE && !mode <> SRC)

  let inVerb () = needsVerbatim := true
  let outVerb () = needsVerbatim := false

  let checkVerb () =
    if !inVerbatim then
      if !needsVerbatim then ()
      else (inVerbatim := false; pr "\\end{progeg}\n")
    else
      if !needsVerbatim then (inVerbatim := true; pr "\\begin{progeg}\n")
      else ()

  let basename = ref ""
  let wholename = ref ""
  let count = ref 0
  let runeg() =
     count := !count + 1;
     let old_needsVerbatim = !needsVerbatim in
     needsVerbatim := false; checkVerb();
     pr ("\\showout{" ^ !basename ^ "." ^ (string_of_int !count) ^ "}\n");
     needsVerbatim := old_needsVerbatim

  let pc lexbuf = function
    '@'	->
      if !lineMode = CHARS then
	(lineMode := TEX; pr "}}")
      else if !lineMode = TEX || (!lineMode = NONE && !mode = TEX) then
	(lineMode := CHARS; pr "\\ensuremath{\\itbox{")
      else if shouldPrint() then
	(checkVerb(); pr "@")
      else
	()
  | '\n' ->
      if !lineMode = CHARS then 
        (if !wholename<>"" then
          (prerr_string "File \""; prerr_string !wholename;
           prerr_string "\", ");
         prerr_string "line 1, characters ";
         prerr_int (Lexing.lexeme_start lexbuf); prerr_string "-";
         prerr_int (Lexing.lexeme_end lexbuf);
         prerr_string ":\nNewline within @...@ sequence\n";
         exit 1);
      if shouldPrint() then (checkVerb(); print_char '\n'); 
      newLine := true; lineMode := NONE;
      if !mode <> BOTH then outVerb();
  | c ->
      if !lineMode = CHARS then
	match c with
	  '_' -> pr "{\\char95}"
	| '#' -> pr "{\\char35}"
	| ' ' -> pr "{ }"
	| '$' -> pr "{\\char36}"
	| '%' -> pr "{\\char37}"
	| '&' -> pr "{\\char38}"
	| '~' -> pr "{\\char126}"
	| '^' -> pr "{\\char94}"
	| '\\' -> pr "{\\char92}"
	| '{' -> pr "{\\char123}"
	| '}' -> pr "{\\char125}"
	(* | '\'' -> pr "{ensuremath{'}}" *)
	| _ -> print_char c
      else if shouldPrint() then
	(checkVerb();
	if !inVerbatim then
          match c with
	      '{' -> pr "{\\char123}"
	    | '}' -> pr "{\\char125}"
            | '\\' -> pr "{\\char92}"
	    | '~' -> pr "{\\char126}"
	    | _ -> print_char c
        else print_char c)
      else
	()

  let prifchar lexbuf p = 
    if shouldPrint() then (checkVerb();
                           (if !inVerbatim || !lineMode = CHARS
                            then p ()
                            else pr (Lexing.lexeme lexbuf)))

  let translateIfInCharmode lexbuf s =
    checkVerb();
    if shouldPrint() then
      (if !mode=TEX && (!lineMode=TEX || !lineMode=NONE) then 
        pr (Lexing.lexeme lexbuf)
       else 
         pr s)

  let mark lexbuf s = 
    let oldMode = !lineMode in
    lineMode := CHARS;
    String.iter (fun c -> pc lexbuf c) s;
    lineMode := oldMode
}

rule lex = parse
  eof { }
(* An ugly hack for xfig output files *)
| "reset@font" {
    pr (Lexing.lexeme lexbuf); lex lexbuf
  }

| "#{@}" ' '* '\n' {
    if !newLine then (newLine:=true; mode := TEX; outVerb())
    else (pc lexbuf '#'; pc lexbuf '{'; pc lexbuf '@'; pc lexbuf '}'; pc lexbuf '\n'); lex lexbuf
  }
| "#{#}" ' '* '\n' {
    if !newLine then (newLine:=true; mode := SRC)
    else (pc lexbuf '#'; pc lexbuf '{'; pc lexbuf '#'; pc lexbuf '}'); lex lexbuf
  }
| "#{*}" ' '* '\n' {
    if !newLine then (newLine:=true; mode := BOTH; inVerb())
    else (pc lexbuf '#'; pc lexbuf '{'; pc lexbuf '*'; pc lexbuf '}'; pc lexbuf '\n'); lex lexbuf
  }
| "#{&}" ' '* '\n' {
    if !newLine then (newLine:=true; mode := BOTH; inVerb())
    else (pc lexbuf '#'; pc lexbuf '{'; pc lexbuf '&'; pc lexbuf '}'; pc lexbuf '\n'); lex lexbuf
  }
| "#@" {
    if !newLine then (newLine := false; lineMode := TEX; outVerb())
    else (pc lexbuf '#'; pc lexbuf '@'); lex lexbuf
  }
| "#&" {
    if !newLine then (newLine := false; lineMode := BOTH; inVerb())
    else (pc lexbuf '#'; pc lexbuf '&'); lex lexbuf
  }
| "##" {
    if !newLine then (newLine := false; lineMode := SRC)
    else (pc lexbuf '#'; pc lexbuf '#'); lex lexbuf
  }
| "#<" ' '* '\n' {
    if !newLine then (newLine := false; runeg())
    else (pr "#<\n"); newLine := true; lex lexbuf
  }
| "#>" [^'\n']* "\n" {
    let s = Lexing.lexeme lexbuf in
    pr (String.sub s 2 ((String.length s) - 2));
    newLine := true; lex lexbuf
  }
| "#*" {
    if !newLine then (newLine := false; lineMode := BOTH; inVerb())
    else (pc lexbuf '#'; pc lexbuf '*'); lex lexbuf
  }
| "[|" [^'|']* "|]" {
    let s = Lexing.lexeme lexbuf in
    pr "\\shade{\\ensuremath{\\itbox{";
    mark lexbuf (String.sub s 2 ((String.length s) - 4));
    pr "}}}"; 
    lex lexbuf
    }
| "{|" [^'|']* "|}" {
    let s = Lexing.lexeme lexbuf in     
    pr "\\highlight{\\ensuremath{\\itbox{";
    mark lexbuf (String.sub s 2 ((String.length s) - 4));
    pr "}}}"; 
    lex lexbuf
  }
| "[v|" [^'|']* "|]" {
    let s = Lexing.lexeme lexbuf in     
    pr "\\lowershade{\\ensuremath{\\itbox{";
    mark lexbuf (String.sub s 3 ((String.length s) - 5));
    pr "}}}"; 
    lex lexbuf
  }
| "{v|" [^'|']* "|}" {
    let s = Lexing.lexeme lexbuf in     
    pr "\\lowerhighlight{\\ensuremath{\\itbox{";
    mark lexbuf (String.sub s 3 ((String.length s) - 5));
    pr "}}}"; 
    lex lexbuf
  }
(*
  | "<->" {
      checkVerb(); if shouldPrint() then pr "\\(\\Def\\)"; lex lexbuf
    }
  | "->" {
      checkVerb(); if shouldPrint() then pr "\\(\\Arrow\\)"; lex lexbuf
    }
  | "<-" {
      checkVerb(); if shouldPrint() then pr "\\(\\LeftArrow\\)"; lex lexbuf
    }
  | "::" {
      translateIfInCharmode lexbuf "\\Inkind{}";
      lex lexbuf
    }
  | "==" {
      checkVerb(); if shouldPrint() then pr "{\\Defn}"; lex lexbuf
    }
  | "=>" {
      checkVerb(); if shouldPrint() then pr "\\(\\TArrow\\)"; lex lexbuf 
    }
  | "==>" {
      checkVerb(); if shouldPrint() then pr "\\(\\CaseArrow\\)"; lex lexbuf
    }
  | "lambda" [' ' '\t']* {
      translateIfInCharmode lexbuf "\\LAMBDA{}";
      lex lexbuf
    }
  | "All" [' ' '\t']* {
      translateIfInCharmode lexbuf "\\FORALL{}";
      lex lexbuf
    }
  | "Some("  {
      pr "Some(";
      lex lexbuf
    }
  | "Some" [' ' '\t']* {
      translateIfInCharmode lexbuf "\\EXISTS{}";
      lex lexbuf
    }
  | [' ' '\t']* "knownas" [' ' '\t']* {
      translateIfInCharmode lexbuf "\\KNOWNAS{}";
      lex lexbuf
    }
  | "Struct" [' ' '\t']* {
      translateIfInCharmode lexbuf "\\STRUCT{}";
      lex lexbuf
    }
  | "Pi" [' ' '\t']* {
      translateIfInCharmode lexbuf "\\PI{}";
      lex lexbuf
    }
  | "{|" {
      checkVerb(); if shouldPrint() then pr "\\(\\LCurlyBar\\)"; lex lexbuf
    }
  | "|}" {
      checkVerb(); if shouldPrint() then pr "\\(\\BarRCurly\\)"; lex lexbuf
    }
  | "**" {
      checkVerb(); prifchar lexbuf (fun ()-> pr "\\(\\TIMES\\)"); lex lexbuf
    }
  | "$" {
      checkVerb(); prifchar lexbuf (fun ()-> pr "\\(\\Triangle\\)"); lex lexbuf
    }
  | "Some" ' '+ {
      checkVerb(); prifchar lexbuf (fun ()-> pr "\\(\\exists\\)"); lex lexbuf
    }
  | "Rec" ' '+ {
      checkVerb(); prifchar lexbuf (fun ()-> pr "\\(\\mu\\)"); lex lexbuf
    }
  | "|]" ' '* "->" {
      checkVerb(); prifchar lexbuf (fun ()-> pr "]"); lex lexbuf
  } 
  | "All[" ['a'-'z'] | "[|" ['a'-'z'] {
      (* variable beginning with lowercase letters is \Pi *)
      checkVerb(); 
      prifchar lexbuf (fun ()-> pr "\\(\\Pi\\)[";
                         let b = Lexing.lexeme lexbuf in
                         print_char (String.get b (String.length b -1)));
      lex lexbuf
    }
  | "All[" ['A'-'Z'] | "[|" ['A'-'Z'] {   
      (* variable beginning with uppercase letters is \forall *)
      checkVerb(); 
      prifchar lexbuf (fun ()-> pr "\\(\\forall\\)[";
                         let b = Lexing.lexeme lexbuf in
                         print_char (String.get b (String.length b - 1)));
      lex lexbuf
    }
*)
| "@@" {
    (*(if !lineMode = CHARS then print_char '@' else (pc lexbuf '@'; pc lexbuf '@'));*)
    print_char '@';
    lex lexbuf
  }
| "@@@" {
    if !lineMode = CHARS then
      (print_char '@'; pc lexbuf '@'; lex lexbuf)
    else if !lineMode = TEX || (!lineMode = NONE && !mode = TEX) then
      (lineMode := CHARS; pr "\\ensuremath{\\itbox{@"; lex lexbuf)
    else
      (pc lexbuf '@'; pc lexbuf '@'; pc lexbuf '@'; lex lexbuf)
  }
(*
  | '_'['a'-'z' 'A'-'Z' '0'-'9']+ '\''*
  | '_' '{' [^ '}' '@' ]* '}' '\''* {
      let s = Lexing.lexeme lexbuf in
      let l = String.length s in
      let numprimes =
        let rec loop n = if n<l && s.[l-n-1] = '\'' then loop(n+1) else n
        in loop 0 in
      let subscr = String.sub s 1 (l-numprimes-1) in
      let primes = String.make numprimes '\'' in
      if !lineMode = CHARS then
        (pr "}"; pr primes; pr "_{"; pr subscr; pr "}\\itbox{")
      else if shouldPrint() then
        (checkVerb();
         if !lineMode=SRC || !lineMode = BOTH
            || (!lineMode=NONE && (!mode=SRC || !mode=BOTH)) then
           (pr "\\progegsubscr{"; pr subscr; pr "}"; pr primes)
         else
           pr s);
      lex lexbuf
    }
*)
| '\''+ {
    let s = Lexing.lexeme lexbuf in
    if !lineMode = CHARS then
      (pr "}"; pr s; pr "\\itbox{")
    else if shouldPrint() then
      (checkVerb(); pr s);
    lex lexbuf
  }
| '~'['a'-'z' 'A'-'Z' '0'-'9']+ {
    if !lineMode = CHARS then
      (let s = Lexing.lexeme lexbuf in
       pr "}\\overline{\\itbox{"; pr (String.sub s 1 (String.length s - 1));
       pr "}}\\itbox{"; lex lexbuf)
    else if shouldPrint() then
      (checkVerb(); pr (Lexing.lexeme lexbuf); lex lexbuf)
    else
      lex lexbuf
  }
| '\n' ([' ' '\t']* as w) '|' { 
    newLine := false; pc lexbuf (Lexing.lexeme_char lexbuf 0); 
      let n = String.length w in 
        for i=0 to n-1 do 
          pc lexbuf w.[i]
        done;
        pc lexbuf ' ';
      lex lexbuf
  }

| _ {
    newLine := false; pc lexbuf (Lexing.lexeme_char lexbuf 0); lex lexbuf
  }

{
  let () =
    (pr "%% AUTOMATICALLY GENERATED: DO NOT MODIFY!\n";
     let finish() =
       if !inVerbatim then pr "\\end{progeg}\n";
       exit 0
     in 

     Arg.parse [
       "-basename", Arg.String (fun n -> basename := n),
          "Process the input file as if it had been called <name>.src";
     ] (fun s ->
          match !wholename with
              "" -> wholename := s
            | _ -> (print_string "Only one input file expected\n"; exit 1)) "";

     if !basename = "" then
       if !wholename = "" then
         basename := "f"
       else
         basename := (if   (String.length !wholename) >= 4
                        & ((String.sub !wholename
                              ((String.length !wholename) - 4) 4) = ".src")
                      then String.sub !wholename 0 
                             ((String.length !wholename) - 4)
                      else !wholename);

     let lexchannel = 
       match !wholename with
         "" -> stdin
       | n -> open_in n
     in 

     lex (Lexing.from_channel lexchannel); finish())
}
