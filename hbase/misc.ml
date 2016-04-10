(****************************************************************)
(* The Harmony Project                                          *)
(* harmony@lists.seas.upenn.edu                                 *)
(*                                                              *)
(* misc.ml - Miscelaneous functions                             *)
(****************************************************************)
(* $Id: misc.ml 4628 2009-08-17 20:39:00Z cretin $ *)

let debug = Trace.debug "misc"

(* ------------- Hashtbl utilities --------------- *)
let safe_hash_add tbl x y =
  if (Hashtbl.mem tbl x) then
    raise (Failure "Misc.hash_safe_add: key already bound")
  else
    Hashtbl.add tbl x y

(* ------------- Safelist utilities --------------- *)

let show_list f l =
  let _, s =
    Safelist.fold_left
      (fun (b, s) a ->
         true,
         s ^ (if b then ";" else "") ^ f a)
      (false, "[")
      l
  in
  s ^ "]"

let enum l =
  Safelist.rev
    (snd (Safelist.fold_left
            (fun (i,acc) k -> (i+1,(i,k)::acc))
            (0,[]) l))

(* map, where the mapped function is 
    (1) provided with the zero-indexed index of each elt and 
    (2) may filter entries by returning None
*)
let map_index_filter f l = 
  let rec aux (n,acc) = function
      [] -> (n,acc)
    | h::t -> let acc' = match f n h with 
          None -> acc
        | Some b -> b::acc in
        aux (n+1,acc') t in
    Safelist.rev (snd (aux (0,[]) l))

let rec dict_cmp cmp l1 l2 = match l1,l2 with
    [],[] -> 0
  | _,[] -> 1
  | [],_ -> -1
  | h1::t1,h2::t2 -> 
      let c = cmp h1 h2 in
        if c <> 0 then c
        else dict_cmp cmp t1 t2 

(* returns true iff elements of l are unique *)
let uniq l =
  let rec loop = function
      [] -> true
    | [x] -> true
    | (x::y::rest) -> x <> y && (loop (y::rest)) in
  loop (Safelist.sort compare l)

let rec union l1 l2 =
  match l1 with
    [] -> l2
  | h::t -> if Safelist.mem h l2 then union t l2 else h :: (union t l2)

let rec remove n l =
  match l with
    [] -> l
  | h::t -> if h=n then t else h::(remove n t)

let safeheadtail = function
    h::t -> Some h, t
  | [] -> None, []

let safetail l = try
  Safelist.tl l
with (Failure "tl") -> []

let rec fold_left2 f c xs ys = match xs,ys with
  [],[] -> c
| (x::xs),[] -> fold_left2 f (f c (Some x) None) xs []
| [],(y::ys) -> fold_left2 f (f c None (Some y)) [] ys
| (x::xs),(y::ys) -> fold_left2 f (f c (Some x) (Some y)) xs ys

let rec fold_left3 f c xs ys zs = match xs,ys,zs with
  [],[],[] -> c
| (x::xs),[],[] -> fold_left3 f (f c (Some x) None None) xs [] []
| [],(y::ys), [] -> fold_left3 f (f c None (Some y) None) [] ys []
| [],[], (z::zs) -> fold_left3 f (f c None None (Some z)) [] [] zs
| (x::xs),(y::ys), [] -> fold_left3 f (f c (Some x) (Some y) None) xs ys []
| (x::xs),[], (z::zs) -> fold_left3 f (f c (Some x) None (Some z)) xs [] zs
| [], (y::ys), (z::zs) -> fold_left3 f (f c None (Some y) (Some z)) [] ys zs
| (x::xs), (y::ys), (z::zs) -> fold_left3 f (f c (Some x) (Some y) (Some z)) xs ys zs

let fold_left2_with_pad f c xs ys padx pady =
  let rec loop acc = function
      [],[] -> acc
    | (x::xs),[] -> loop (f acc x pady) (xs,[])
    | [],(y::ys) -> loop (f acc padx y) ([],ys)
    | (x::xs),(y::ys) -> loop (f acc x y) (xs,ys) in
  loop c (xs,ys)

let map2_with_pad f xs ys padx pady =
  Safelist.rev
    (fold_left2_with_pad
       (fun acc x y -> (f x y)::acc)
       [] xs ys padx pady)

let zip_with_pad pad1 pad2 l1 l2 =
  let rec loop acc = function
      [],[] -> Safelist.rev acc
    | x::xs,[] -> loop ((x,pad2)::acc) (xs,[])
    | [],y::ys -> loop ((pad1,y)::acc) ([],ys)
    | x::xs,y::ys -> loop ((x,y)::acc) (xs,ys) in
  loop [] (l1,l2)

let rec iter_with_sep f fsep = function
    [] -> ()
  | [x] -> f x
  | (x::xs) -> f x; fsep (); iter_with_sep f fsep xs

let rev_and_flatten ll =
  let rec help acc = function
      [] -> acc
    | (l::ls) -> help (Safelist.append l acc) ls in
  help [] ll

let safeassoc alist =
  fun k ->
    (try
      Safelist.assoc k alist
    with Not_found -> k)

(* partition n l divides the list l into two segments, where the first*)
(* segment has length n. if there are not n elements in l, all the *)
(* elements go into the first segment and the second segment is empty *)
let partition n l =
  let rec help n l acc =
    match n,l with
      0,_ -> (Safelist.rev acc,l)
    | n,(x::xs) -> help (n-1) xs (x::acc)
    | _,_ -> assert false in
  if ((Safelist.length l) <= n) then (l,[]) else
  help n l []

let take n l =
  let rec take_aux n l acc =
    match n,l with
    0,_ | _,[]           -> Safelist.rev acc
      | n,(x::xs) when n > 0 -> take_aux (n-1) xs (x::acc)
      | _                    -> assert false
  in
  take_aux n l []

(* composel : ('a -> 'a) list -> ('a -> 'a) *)
let composel (l : ('a -> 'a) list) =
  Safelist.fold_left (fun f acc -> (fun s -> f (acc s))) (fun x -> x) l

(* ------------- Option utilities --------------- *)

let map_option f = function
    None -> None
  | Some x -> Some (f x)

let map2_option f o1 o2 = 
  match o1,o2 with
    | Some x1,Some x2 -> Some (f x1 x2)
    | _ -> None

let map2opt f xs ys =
  let rec loop acc = function
      [],[] -> Safelist.rev acc
    | x::xs,[] -> loop ((f (Some x) None)::acc) (xs,[])
    | [],y::ys -> loop ((f None (Some y))::acc) ([],ys)
    | x::xs,y::ys -> loop ((f (Some x) (Some y))::acc) (xs,ys) in
  loop [] (xs,ys)

(* ------------- Alternative utilities --------------- *)
type ('a,'b) alternative = Left of 'a | Right of 'b

let map_left alt f = match alt with 
  | Left l -> f l
  | r -> r
      
let map_right alt1 f = match alt1 with
  | Right r -> f r
  | l -> l

(* ------------- String/Char utilities --------------- *)

(* Based on String.escape 
 *)
let escape (escapeChar: char -> string) s =
  debug (fun () -> Util.format "escape: %s@\n" s);
  let n = ref 0 in
    for i = 0 to String. length s - 1 do
      let l = String.length (escapeChar (String.get s i)) in
      assert (l>0);
      n := !n + l;
    done;
  let result = 
    if !n = String.length s then s else 
    begin
      let s' = String.create !n in   
      n := 0;
      for i = 0 to String.length s - 1 do 
        let c = String.get s i in
        let cEscaped = escapeChar c in
        if String.length cEscaped = 1 then begin
          String.set s' !n c; incr n
        end else
          for i = 0 to String.length cEscaped - 1 do begin
            String.set s' !n (String.get cEscaped i);
            incr n;
          end done
      done;
      s'
    end
  in
  debug(fun () -> Util.format "escape returns %s@\n" result);
  result

(* \ -> \\; x -> \x (for all x in escapedchars) *)
let generic_escape_char escapedchars c = 
  if c = '\\' then 
    "\\\\"
  else if String.contains escapedchars c then
    let str = String.create 2 in
    String.set str 0 '\\';
    String.set str 1 c;
    str
  else
    "-"

(* \x -> x *)
let generic_unescape s =
  if not (String.contains s '\\') then
    s
  else
    let rec loop i n =
      if i >= String.length s then
        n
      else if s.[i] = '\\' then
        loop (i + 2) (n + 1)
      else
        loop (i + 1) (n + 1)
    in
    let s' = String.create (loop 0 0) in
    let rec loop i i' = 
      if i >= String.length s then
        ()
      else if s.[i] = '\\' then begin
        s'.[i'] <- s.[i+1]; (* assumes that '\' always followed by a char *)
        loop (i + 2) (i' + 1)
      end else begin
        s'.[i'] <- s.[i];
        loop (i + 1) (i' + 1)
      end
    in
    loop 0 0;
    s'

(* find c, skipping all the escaped characters, e.g., "\;" *)
let rec index_rec_nonescape s i c = 
  if i >= String.length s then 
    raise Not_found 
  else if String.unsafe_get s i = c then 
    i
  else if String.unsafe_get s i = '\\' then
    index_rec_nonescape s (i+2) c
  else
    index_rec_nonescape s (i+1) c

let split_nonescape c text = 
  let rec split start =
    try 
      let pos = index_rec_nonescape text start c in
      String.sub text start (pos-start) :: split (pos+1)
    with Not_found ->
      [String.sub text start (String.length text - start)]
  in
  split 0

let is_blank s = 
  let l = String.length s in
  let rec loop i =
    if i = l then true
    else String.contains "\n \t" s.[i] && loop (i + 1)
  in
  loop 0

let bounded_index_from buf pos len char =
  let endpos = pos + len in
  let rec loop i =
    if i >= endpos then raise Not_found
    else if buf.[i] = char then i
    else loop (i+1)
  in
  loop pos

let findsubstring s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let rec loop i =
    if i+l1 > l2 then None
    else if s1 = String.sub s2 i l1 then Some(i)
    else loop (i+1)
  in loop 0

let trimLeadingSpaces s = 
  let l = String.length s in
  let rec loop i = 
    if i < l && s.[i] = ' ' then loop (i + 1)
    else String.sub s i (l - i)
  in
  loop 0

let rec replace_substring s fromstring tostring =
  match findsubstring fromstring s with
    None -> s
  | Some(i) ->
      let before = String.sub s 0 i in
      let afterpos = i + (String.length fromstring) in
      let after = String.sub s afterpos ((String.length s) - afterpos) in
      before ^ tostring ^ (replace_substring after fromstring tostring)

let rec replace_suffix s froms tos = 
  let s_len = String.length s in 
  let froms_len = String.length froms in 
  let pref_len = s_len - froms_len in 
    if pref_len >= 0 then
      (String.sub s 0 pref_len) ^ tos
    else
      raise Not_found
        
let rec replace_substrings s l =
  match l with
    [] -> s
  | (sub,sub')::rest -> replace_substrings (replace_substring s sub sub') rest

let whack_chars s cl reverse =
  let esc_s = 
    Safelist.fold_right 
      (fun (p,r) s -> Str.global_replace (Str.regexp p) r s)   
      [("\n","\\n");("\"","\\\"");("\\", "\\\\\\\\")]
      s in
    if reverse then
      try
        String.iter
          (fun c -> if not (List.mem c cl) then raise Not_found)
          esc_s;
        esc_s
      with Not_found -> Printf.sprintf "\"%s\"" esc_s
    else
        if (Safelist.exists (fun c -> String.contains s c) cl) then 
          Printf.sprintf "\"%s\"" esc_s 
        else 
          esc_s

let whack s =   
  if s = "" then "\"\"" 
  else whack_chars s [' '; '\n';'"'] false

let whack_ident s = 
  let s' =
    whack_chars s
      ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'o';
       'n'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'; 'A'; 'B';
       'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'O'; 'N'; 'P';
       'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; '0'; '1'; '2'; '3';
       '4'; '5'; '6'; '7'; '8'; '9'; '0'; '\''; '_'; '-'; '@']
      true in
  if s' = "" then "\"\"" else s'

let unescaped s =
  let buf = Buffer.create (String.length s) in
  let rec loop i =
    if (i >= (String.length s)) then () else
    let c = s.[i] in
    if (c = '\\') then
      let c2 = s.[i+1] in
      match c2 with
        '\\' | '"' | '\'' -> (Buffer.add_char buf c2; loop (i+2))
      | 'n' -> (Buffer.add_char buf '\n'; loop (i+2))
      | 'r' -> (Buffer.add_char buf '\r'; loop (i+2))
      | 't' -> (Buffer.add_char buf '\t'; loop (i+2))
      | 'b' -> (Buffer.add_char buf '\b'; loop (i+2))
      | _ ->
          let c3 = s.[i+2] in
          let c4 = s.[i+3] in
          let i2 = int_of_char(c2) - int_of_char('0') in
          let i3 = int_of_char(c3) - int_of_char('0') in
          let i4 = int_of_char(c4) - int_of_char('0') in
          if (i2 < 0 || i2 > 9 || i3 < 0 || i3 > 9 || i4 < 0 || i4 > 9) then
            raise (Error.Harmony_error (fun () -> Util.format "Bad escape sequence in %s" (whack s)))
          else
            (Buffer.add_char buf 
              (char_of_int (i2 * 100 + i3 * 10 + i4)); loop (i+4))
    else
      (Buffer.add_char buf c; loop (i+1)) in
  loop 0;
  Buffer.contents buf

let unwhack s =
  let candidate =
    if s.[0] <> '"' then s
    else String.sub s 1 ((String.length s) - 2) in
  unescaped candidate

let rec hexify_string s =  (* inefficient *)
  if s = "" then ""
  else (Printf.sprintf "%02x" (int_of_char s.[0]))
     ^ (hexify_string (String.sub s 1 (String.length s - 1)))

let splitIntoWords (s:string) (c:char) = 
  let rec inword acc start pos =
    if pos >= String.length(s) || s.[pos] = c then
      betweenwords ((String.sub s start (pos-start)) :: acc) pos
    else inword acc start (pos+1)
  and betweenwords acc pos =
    if pos >= (String.length s) then (Safelist.rev acc)
    else if s.[pos]=c then betweenwords acc (pos+1)
    else inword acc pos pos
  in betweenwords [] 0

let filename_extension (s:string) =
  Safelist.nth (Safelist.rev (splitIntoWords s '.')) 0

let colorize = Prefs.colorizePref

type color = Black | Red | Green | Yellow | Blue | Pink | Cyan | White
let colorcode = function
    Black   -> "30m"
  | Red     -> "31m"
  | Green   -> "32m"
  | Yellow  -> "33m"
  | Blue    -> "34m"
  | Pink    -> "35m"
  | Cyan    -> "36m"
  | White   -> "37m"

(* val color : string -> ~bold:bool -> color -> string *)
let color s ?(bold=false) c =
  if not (Prefs.read colorize) then s else
    let prefix = if bold then "1;" else "0;" in
    "[" ^ prefix ^ (colorcode c) ^ s ^ "[0;29m" 

(* ------------- File/Directory utilities --------------- *)

(* is_dir : string -> bool *)
let is_dir f =
  try (Unix.stat f).Unix.st_kind = Unix.S_DIR with _ -> false

let is_file f =
  try (Unix.stat f).Unix.st_kind = Unix.S_REG with _ -> false

(* mkdir_forsure : string -> unit *)
let mkdir_forsure d = if not (is_dir d) then Unix.mkdir d 0o0755

(* read_dir : string -> string list *)
(* NOTE: CVS directories ignored! *)
let read_dir d =
  let ignored = ["."; ".."; "CVS"] in
  let d = Unix.opendir d in
  let rec do_read acc =
    try
      (match (Unix.readdir d) with
       | s when Safelist.mem s ignored -> do_read acc
       | f -> do_read (f :: acc))
    with End_of_file -> acc
  in
  let files = do_read [] in
  Unix.closedir d;
  files

(* in_dir : string -> (unit -> 'a) -> 'a *)
let in_dir d f =
  let cwd = Unix.getcwd () in
  Unix.chdir d;
  let res = f () in
  Unix.chdir cwd;
  res

let rec remove_file_or_dir d =
  if not (Sys.file_exists d) then () else
  if (Unix.stat d).Unix.st_kind = Unix.S_DIR then begin
    let handle = Unix.opendir d in
    let rec loop () =
      let r = try Some(Unix.readdir handle)
      with End_of_file -> None
        | Sys_error s -> raise (Error.Harmony_error(fun () -> Util.format "Error reading %s (%s)" d s)) in
        match r with
            Some f ->
              if f="." || f=".." then loop ()
              else begin
                remove_file_or_dir (d^"/"^f);
                loop ()
              end  
          | None ->
              Unix.closedir handle;
              Unix.rmdir d
    in loop ()
  end else 
    Sys.remove d

let read_chan chan =
  let nbytes = in_channel_length chan in
  let string = String.create nbytes in
  really_input chan string 0 nbytes;
  string

let read file =
  if file = "-" then
    read_chan stdin
  else 
    let chan = open_in_bin file in
    try
      let r = read_chan chan in
      close_in chan;
      r
    with exn ->
      close_in chan;
      raise exn

let write file s =
  if file = "-" then
    output_string stdout s
  else 
    let chan = open_out_bin file in
    try
      output_string chan s;
      close_out chan
    with exn ->
      close_out chan;
      raise exn

let tempFileName tag =
  let rec loop i = 
    let pid = Unix.getpid() in
    let now = Unix.time() in
    let name = Printf.sprintf "%s-%d-%f%d.tmp" tag pid now i in
    if Sys.file_exists name then loop (i+1)
    else name
  in loop 0

let backup_suffix = ".bak"

let backup_file_name path =
  let time_as_compact_str () =
    let time = Unix.localtime (Unix.time()) in
      Printf.sprintf
        "%4d%02d%02d+%02d%02d" 
        (time.Unix.tm_year + 1900)
        (time.Unix.tm_mon + 1)
        time.Unix.tm_mday
        time.Unix.tm_hour
        time.Unix.tm_min
  in
  let t = time_as_compact_str () in
  let rec f i =
    let suf =
      if i=0 then backup_suffix
      else Printf.sprintf ".%d%s" i backup_suffix in
    let s = Printf.sprintf "%s-%s%s" path t suf in
    if Sys.file_exists s then f (i+1) else s in
  f 0

let rec cp_dash_r fromhere tohere =
  if is_dir fromhere then
    begin
      mkdir_forsure tohere;
      Safelist.iter (fun f -> cp_dash_r (fromhere ^ "/" ^ f) (tohere ^ "/" ^ f))
                (read_dir fromhere)
    end
  else
    write tohere (read fromhere)

(* ### Should trap unix errors and do something sensible... *)
let backup path =
  let newpath = backup_file_name path in
  if is_dir path or Sys.file_exists path then cp_dash_r path newpath

(* exec : string -> string *)
let exec s = 
  let buf = Buffer.create 17 in 
  let p = Unix.open_process_in s in
  let _ = 
    try 
      while true do
        Buffer.add_char buf (input_char p)
      done
    with End_of_file -> () in 
  let _ = 
    match Unix.close_process_in p with
      | Unix.WEXITED 0 -> ()
      | _ -> invalid_arg ("Misc.exec: " ^ s) in 
  Buffer.contents buf

(* read_char : unit -> char *)
let read_char () =
  let term = Unix.tcgetattr Unix.stdin in
  try
    Unix.tcsetattr
      Unix.stdin
      Unix.TCSANOW
      {term with Unix.c_icanon = false;
                 Unix.c_echo = false;
                 Unix.c_vmin = 1;};
    let c = input_char stdin in
    (* restore the terminal in its initial state *)
    Unix.tcsetattr Unix.stdin Unix.TCSANOW term;
    c
  with
  | e -> Unix.tcsetattr Unix.stdin Unix.TCSANOW term; raise e

(* ------------- Mutable variable utilities --------------- *)

let dynamic_var i = ref i

let dynamic_lookup d = !d

let dynamic_bind d v f =
  let cur = !d in
  d := v;
  try
    let res = f() in
    d := cur;
    res
  with e -> begin
    d := cur;
    raise e
  end 

(* ------------- Numeric utilities ------------------*)
let divide num den = if den = 0 then 0.0 else (float_of_int num /. float_of_int den)
let percent num den = 100.0 *. (divide num den)
  
(* ------------- Pretty printing utilities --------------- *)

let concat fold sep is_empty empty pretty structure = 
  fold 
    (fun acc h -> 
       if is_empty acc then pretty h
       else sep acc (pretty h))
    empty 
    structure 

let concat_list sep l = 
  concat
    Safelist.fold_left
    (fun x y -> Printf.sprintf "%s%s%s" x sep y)
    (fun x -> String.length x = 0)
    ""
    (fun x -> x)
    l

let concat_f_list sep f l = concat_list sep (Safelist.map f l)

let format_list sep f l =
  let extract_thk = function 
      Some thk -> thk
    | None -> (fun () -> ()) in
  let thko =
    concat
      Safelist.fold_left
      (fun x y -> 
         Some (fun () -> 
                 extract_thk x ();
                 Util.format sep;
                 extract_thk y ()))
      (fun x -> x = None)
      None
      (fun x -> Some (fun () -> f x))
      l
  in
    extract_thk thko ()

(* SHOULD BE NUKED SOON *)
let fformat_list fmtr sep f l =
  let extract_thk = function 
      Some thk -> thk
    | None -> (fun () -> ()) in
  let thko =
    concat
      Safelist.fold_left
      (fun x y -> 
         Some (fun () -> 
                 extract_thk x ();
                 Format.fprintf fmtr sep;
                 extract_thk y ()))
      (fun x -> x = None)
      None
      (fun x -> Some (fun () -> f x))
      l
  in
    extract_thk thko ()


