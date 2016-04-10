(*
 * ML lex script to convert .src files to .pi files.
 *)

{
  type mode = SRC | TEX
  type lineMode = SRC | TEX | NONE

  let mode = ref SRC
  and lineMode = ref NONE
  and newLine = ref true

  let buffer_size = 240
  let current = Buffer.create buffer_size
  let old = Buffer.create buffer_size

  let suffix = ref "f" 
  let terminator = ref ";" 
  let created_files = ref ""
  let basename = ref ""
  let wholename = ref ""
  let count = ref 0

  let emit s = Buffer.add_string current s

  let reset () = 
    mode := SRC;
    lineMode := NONE;
    newLine := true;
    Buffer.reset current;
    Buffer.reset old;
    count := 0

  let dump() =
     count := !count + 1;
     let outname = !basename (* ^ "." ^ (string_of_int !count) ^ "." ^ !suffix *) in
     created_files := (outname
                       ^ (if !created_files <> "" then " " else "")
                       ^ !created_files);
     let o = open_out_bin outname in
       if (Buffer.length old) <> 0 then
         (output_string o ("DO printingoff" ^ !terminator ^ "\n");
          Buffer.output_buffer o old;
          output_string o ("DO printingon" ^ !terminator ^ "\n"));
       Buffer.output_buffer o current; close_out o;
       Buffer.reset old; Buffer.add_buffer old current;
       Buffer.reset current

  let pr s =
    if (!mode = SRC && !lineMode = NONE) || !lineMode = SRC then
      emit s
    else
      ()
}

rule lex = parse
  eof { }
| "\n" {
    emit "\n"; newLine := true; lineMode := NONE; lex lexbuf
  }
| "#{@}" {
    if !newLine then (newLine := false; mode := TEX)
    else (pr "#{@}"); lex lexbuf
  }
| "#{#}" {
    if !newLine then (newLine := false; mode := SRC; pr "    ")
    else (pr "#{#}"); lex lexbuf
  }
| "#{*}" {
    if !newLine then (newLine := false; mode := SRC; pr "    ")
    else (pr "#{*}"); lex lexbuf
  }
| "#@" {
    if !newLine then (newLine := false; lineMode := TEX)
    else (pr "#@"); lex lexbuf
  }
| "#&" {
    if !newLine then (newLine := false; lineMode := TEX)
    else (pr "#&"); lex lexbuf
  }
| "##" {
    if !newLine then (newLine := false; lineMode := SRC; pr "  ")
    else (pr "##"); lex lexbuf
  }
| "#<" ' '* '\n' {
    if !newLine then (dump(); emit "\n") else (pr "#<\n");
    newLine := true; lex lexbuf
  }
(* Nuke the next once things stabilize *)
| "%USECHECKER2" [' ']* "\n" {
    let _ = Lexing.lexeme lexbuf in 
    if !newLine then
      (suffix := "ff"; terminator:=";"; emit "\n")
    else
      pr (Lexing.lexeme lexbuf);
    newLine := true; lex lexbuf
  }
| "###" {
    if !newLine then (newLine := false; lineMode := SRC; pr "#")
    else (pr "###"); lex lexbuf
  }
| "#*" {
    if !newLine then (newLine := false; lineMode := SRC; pr "  ")
    else (pr "#*"); lex lexbuf
  }
| "[|" [^'|''\n']* "|]" {
    let s = Lexing.lexeme lexbuf in
    pr (String.sub s 2 ((String.length s) - 4));
    lex lexbuf
  }
| "{|" [^'|''\n']* "|}" {
    let s = Lexing.lexeme lexbuf in
    pr (String.sub s 2 ((String.length s) - 4)); 
    lex lexbuf
  }
| "[v|" [^'|''\n']* "|]" {
    let s = Lexing.lexeme lexbuf in
    pr (String.sub s 3 ((String.length s) - 5)); 
    lex lexbuf
  }
| "{v|" [^'|''\n']* "|}" {
    let s = Lexing.lexeme lexbuf in
    pr (String.sub s 3 ((String.length s) - 5)); 
    lex lexbuf
  }
| _ {
    newLine := false; pr (Lexing.lexeme lexbuf); lex lexbuf
  }

{
  let fcl_of_src_str s = 
    reset ();
    let _ = lex (Lexing.from_string s) in 
    Buffer.contents current

  let fcl_of_src fn = 
    reset ();
    let fchan = open_in_bin fn in
    let _ = lex (Lexing.from_channel fchan) in 
    let _ = close_in fchan in 
    Buffer.contents current
}
