module B = Buffer
module F = Format
module S = String

let desc = ref ""
let cmd = B.create 100

let go () = 
  let red s = "[1;31m" ^ s ^ "[0;29m" in
  let green s = "[1;32m" ^ s ^ "[0;29m" in
  let c_stdout = Unix.open_process_in (B.contents cmd) in
    F.print_string !desc;
    for i=1 to (80-S.length !desc) do F.print_char ' ' done;
    F.print_flush ();
    let c_buf = B.create 100 in
      (try while true do B.add_channel c_buf c_stdout 1 done with End_of_file -> ());
      begin match Unix.close_process_in c_stdout with
          Unix.WEXITED(0) -> F.printf "[   %s   ]@\n" (green "OK") 
        | _               -> F.printf "[ %s ]@\n%s" (red "FAILED") (B.contents c_buf)
      end;
      F.print_flush ();
      B.reset cmd      
    
let _ = 
  let inc = 
    if Array.length (Sys.argv) > 1 then 
      begin 
        let fn = Sys.argv.(1) in
        if Sys.file_exists fn then
          open_in_bin (Sys.argv.(1)) 
        else (Format.eprintf "wrap: %s not found" fn; exit 1)          
      end 
    else stdin in
    try 
      while true do
        let line = input_line inc in        
        let len = S.length line in
          if len <> 0 then
            if line.[len-1] = '\\' then B.add_string cmd (S.sub line 0 (len-1))
            else 
              let hash_pos = S.index line '#' in
                desc := S.sub line (hash_pos+1) (S.length line - (hash_pos+1));
                B.add_string cmd (S.sub line 0 hash_pos);
                B.add_string cmd " 2>&1";
                go ()                  
      done
    with End_of_file -> ()  
