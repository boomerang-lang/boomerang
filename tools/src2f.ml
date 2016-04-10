open Src2fcl

let () =
  if Array.length Sys.argv == 3 then
    (wholename := Array.get Sys.argv 1;
     basename := Array.get Sys.argv 2;
     emit "# 1 \""; emit !wholename;
     emit "\"\n"; lex (Lexing.from_channel (open_in !wholename)))
  else
    (basename := "tex/f"; 
     lex (Lexing.from_channel stdin));
  if Buffer.length current > 0 then dump();
  print_string !created_files; print_string "\n";
  exit 0
