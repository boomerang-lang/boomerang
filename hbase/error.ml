(*****************************************************)
(* The Harmony Project                               *)
(* harmony@lists.seas.upenn.edu                      *)
(*                                                   *)
(* error.ml - run-time exceptions                    *)
(*****************************************************)
(* $Id: error.ml 3437 2007-12-09 22:41:31Z jnfoster $ *)

(* run-time errors carry a (unit -> unit) function that can be used to
   print a message using the Format functions *)
exception Harmony_error of (unit -> unit)

let simple_error s = raise (Harmony_error (fun () -> Util.format "%s" s))

(* fail_on_error : (unit -> 'a) -> 'a 
 *    simple error handling: print and exit. Used in the text UI *)
let exit_on_error f = 
  try 
    f ()
  with Harmony_error msg -> 
    msg ();
    Util.format "@\n";
    exit 2
