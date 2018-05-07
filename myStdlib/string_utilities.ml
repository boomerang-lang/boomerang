open Core
open Util

let paren (s:string) : string = "(" ^ s ^ ")"

let bracket (s:string) : string = "[" ^ s ^ "]"

let undelimit_string : string -> string =
    (Str.global_replace (Str.regexp "\\\\\\\\") "\\\\")
  % (Str.global_replace (Str.regexp "\\\\n") "\n")
  % (Str.global_replace (Str.regexp "\\\\t") "\t")
  % (Str.global_replace (Str.regexp "\\\\\"") "\"")

      
let delimit_string : string -> string =
  Str.global_replace (Str.regexp "\"") "\\\\\""
  % (Str.global_replace (Str.regexp "\n") "\\\\n")
  % (Str.global_replace (Str.regexp "\t") "\\\\t")
  % (Str.global_replace (Str.regexp "\\\\") "\\\\\\\\")

let delimit_tabs : string -> string =
  Str.global_replace (Str.regexp "\t") "\\t"

let delimit_newlines : string -> string =
  Str.global_replace (Str.regexp "\n") "\\n"

let delimit_slashes : string -> string =
  Str.global_replace (Str.regexp "\\") "\\\\"

let delimit_commas : string -> string =
  Str.global_replace (Str.regexp ",") "\\,"

let undelimit_commas : string -> string =
  Str.global_replace (Str.regexp "\\,") ","

let string_of_option (inner_converter:'a -> string) (ao:'a option) : string =
  begin match ao with
    | None -> "None"
    | Some a -> "Some " ^ inner_converter a
  end

let string_of_either
    (left_to_string:'a -> string)
    (right_to_string:'b -> string)
    (x:('a,'b) either)
  : string =
  begin match x with
    | Left l -> "Left" ^ paren (left_to_string l)
    | Right r -> "Right" ^ paren (right_to_string r)
  end

let string_of_list (inner_converter:'a -> string) (al:'a list) : string =
  bracket
    (String.concat
       ~sep:";"
       (List.map ~f:inner_converter al))

let string_of_pair
    (first_converter:'a -> string)
    (second_converter:'b -> string)
    ((a,b):('a*'b))
  : string =
  paren
    ((first_converter a) ^ "," ^ (second_converter b))

let string_of_triple
    (fst_to_string:'a -> string)
    (snd_to_string:'b -> string)
    (trd_to_string:'c -> string)
    ((a,b,c):('a * 'b * 'c))
  : string =
  paren
    ((fst_to_string a) ^ "," ^ (snd_to_string b) ^ "," ^ (trd_to_string c))

let string_of_quadruple
    (fst_to_string:'a -> string)
    (snd_to_string:'b -> string)
    (trd_to_string:'c -> string)
    (rth_to_string:'d -> string)
    ((a,b,c,d):('a * 'b * 'c * 'd))
  : string =
  paren
    ((fst_to_string a)
     ^ "," ^ (snd_to_string b)
     ^ "," ^ (trd_to_string c)
     ^ "," ^ (rth_to_string d))

let string_of_quintuple
    (fst_to_string:'a -> string)
    (snd_to_string:'b -> string)
    (trd_to_string:'c -> string)
    (rth_to_string:'d -> string)
    (fth_to_string:'e -> string)
    ((a,b,c,d,e):('a * 'b * 'c * 'd * 'e))
  : string =
  paren
    ((fst_to_string a)
     ^ "," ^ (snd_to_string b)
     ^ "," ^ (trd_to_string c)
     ^ "," ^ (rth_to_string d)
     ^ "," ^ (fth_to_string e))

let string_of_sextuple
    (fst_to_string:'a -> string)
    (snd_to_string:'b -> string)
    (trd_to_string:'c -> string)
    (rth_to_string:'d -> string)
    (fth_to_string:'e -> string)
    (sth_to_string:'e -> string)
    ((a,b,c,d,e,f):('a * 'b * 'c * 'd * 'e * 'f))
  : string =
  paren
    ((fst_to_string a)
     ^ "," ^ (snd_to_string b)
     ^ "," ^ (trd_to_string c)
     ^ "," ^ (rth_to_string d)
     ^ "," ^ (fth_to_string e)
     ^ "," ^ (sth_to_string f))
    
let string_of_int_list : int list -> string =
  string_of_list string_of_int

let string_of_int_list_list : int list list -> string =
  string_of_list string_of_int_list

let string_of_char : char -> string = Char.to_string

let string_of_char_list : char list -> string =
  string_of_list Char.to_string

let string_of_char_list_list : char list list -> string =
  string_of_list string_of_char_list

let string_of_ref (location_to_string:'a -> string) (r:'a ref) : string =
  (location_to_string !r) ^ " ref"
