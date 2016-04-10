let first = ref true

let () =
  let inc = 
    if Array.length Sys.argv == 2 then open_in_bin Sys.argv.(1)
    else (Printf.eprintf "usage: read_oracle FILE\n"; exit 1) in
    (try 
       Printf.printf "let data = ref [";
       while true do
       let s = (input_value inc : string) in 
       let b = (input_value inc : bool) in 
	 if !first then first:=false
	 else Printf.printf ";\n";      
	 Printf.printf "%b (* %s *)" b s;
     done
     with End_of_file -> close_in inc);
    Printf.printf "]\n"
