(* Unison file synchronizer: src/ubase/trace.ml *)
(* Copyright 1999-2007 (see COPYING for details) *)

(* ---------------------------------------------------------------------- *)
(* Choosing where messages go *)

type trace_printer_choices = [`Stdout | `Stderr | `FormatStdout]

let traceprinter = ref (`Stderr : trace_printer_choices)

let redirect x = (traceprinter := x)

(* ---------------------------------------------------------------------- *)
(* Debugging messages *)

let debugmods = Prefs.debugPref
let debugtimes = Prefs.debugtimesPref

let runningasserver = ref false

let debugging() = (Prefs.read debugmods) <> []

let enabled modname =
  let m = Prefs.read debugmods in
  let en = 
    m <> [] && (   (* tracing labeled "" is enabled if anything is *)
                   (modname = "")
                || (* '-debug verbose' enables everything *)
                   (Safelist.mem "verbose" m)
                || (* '-debug all+' likewise *)
                   (Safelist.mem "all+" m)
                || (* '-debug all' enables all tracing not marked + *)
                   (Safelist.mem "all" m && not (Util.endswith modname "+"))
                || (* '-debug m' enables m and '-debug m+' enables m+ *)
                   (Safelist.mem modname m)
                || (* '-debug m+' also enables m *)
                   (Safelist.mem (modname ^ "+") m)
               ) in
  en
    
let enable modname onoff =
  let m = Prefs.read debugmods in
  let m' = if onoff then (modname::m) else (Safelist.remove modname m) in
  Prefs.set debugmods m'

let debug modname thunk =
  if enabled modname then begin
    let s = if !runningasserver then "server: " else "" in
    let time =
      if Prefs.read debugtimes then
        let tm = Util.localtime (Util.time()) in
        Printf.sprintf "%02d:%02d:%02d"
          tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      else "" in
    if time<>"" || s<>"" || modname<>"" then begin
      let time = if time="" || (s=""&&modname="") then time else time^": " in
      match !traceprinter with
      | `Stdout -> Printf.printf "[%s%s%s] " time s modname
      | `Stderr -> Printf.eprintf "[%s%s%s] " time s modname
      | `FormatStdout -> Format.printf "[%s%s%s] " time s modname
      end;
    thunk();
    flush stderr
  end

(* We set the debugPrinter variable in the Util module so that other modules
   lower down in the module dependency graph (so that they can't just
   import Trace) can also print debugging messages. *)
let _ = Util.debugPrinter := Some(debug)


(* ---------------------------------------------------------------------- *)
(* Logging *)

let logging = Prefs.logPref
let logfile = Prefs.logfilePref

let logch = ref None

let rec getLogch() =
  Util.convertUnixErrorsToFatal "getLogch" (fun() ->
  match !logch with
    None ->
      let file = Prefs.read logfile in
      let ch =
        open_out_gen [Open_wronly; Open_append; Open_creat] 0o600 file in
      logch := Some (ch, file);
      ch
  | Some(ch, file) ->
      if Prefs.read logfile = file then ch else begin
        close_out ch;
        logch := None; getLogch ()
      end)

let sendLogMsgsToStderr = ref true

let writeLog s =
  if !sendLogMsgsToStderr then begin
      match !traceprinter with
      | `Stdout -> Printf.printf "%s" s
      | `Stderr -> Util.msg "%s" s
      | `FormatStdout -> Format.printf "%s " s
  end else debug "" (fun() -> 
      match !traceprinter with
      | `Stdout -> Printf.printf "%s" s
      | `Stderr -> Util.msg "%s" s
      | `FormatStdout -> Format.printf "%s " s);
  if Prefs.read logging then begin
    let ch = getLogch() in
    output_string ch s;
    flush ch
  end

(* ---------------------------------------------------------------------- *)
(* Formatting and displaying messages *)

let terse = Prefs.tersePref

type msgtype = Msg | StatusMajor | StatusMinor | Log
type msg = msgtype * string

let defaultMessageDisplayer s =
  if not (Prefs.read terse) then begin
    let show() = if s<>"" then Util.msg "%s\n" s in
    if enabled "" then debug "" show
    else if not !runningasserver then show()
  end 

let messageDisplayer = ref defaultMessageDisplayer

let defaultStatusFormatter s1 s2 = s1 ^ " " ^ s2

let statusFormatter = ref defaultStatusFormatter

let statusMsgMajor = ref ""
let statusMsgMinor = ref ""

let displayMessageLocally (mt,s) = 
  let display = !messageDisplayer in
  let displayStatus() =
    display (!statusFormatter !statusMsgMajor !statusMsgMinor) in
  match mt with
    Msg -> display s
  | StatusMajor -> statusMsgMajor := s; statusMsgMinor := ""; displayStatus()
  | StatusMinor -> statusMsgMinor := s; displayStatus()
  | Log -> writeLog s

let messageForwarder = ref None

let displayMessage m =
  match !messageForwarder with
    None -> displayMessageLocally m
  | Some(f) -> f m

(* ---------------------------------------------------------------------- *)
(* Convenience functions for displaying various kinds of messages *)

let message s = displayMessage (Msg, s)

let status s =
  displayMessage (StatusMajor, s)

let statusMinor s = displayMessage (StatusMinor, s)
                      
let statusDetail s =
  let ss = if not !runningasserver then s else (Util.padto 30 s) ^ " [server]" in
  displayMessage (StatusMinor, ss)

let log s = displayMessage (Log, s)

let logverbose s =
  let temp = !sendLogMsgsToStderr in
  sendLogMsgsToStderr := !sendLogMsgsToStderr && not (Prefs.read terse);
  displayMessage (Log, s);
  sendLogMsgsToStderr := temp 

(* ---------------------------------------------------------------------- *)
(* Timing *)
    
let printTimers = Prefs.timersPref
    
type timer = string * float

let gettime () = Unix.gettimeofday()

let startTimer desc =
  if Prefs.read(printTimers) then
    (message (desc ^ "..."); (desc, gettime()))
  else
    (desc,0.0)

let startTimerQuietly desc =
  if Prefs.read(printTimers) then
    (desc, gettime())
  else
    (desc,0.0)

let showTimer (desc, t1) =
  (* Showing timer values from the server process does not work at the moment:
     it confuses the RPC mechanism *)
  if not !runningasserver then
    if Prefs.read(printTimers) then
      let t2 = gettime() in
      message (Printf.sprintf "%s (%.2f seconds)" desc (t2 -. t1))

