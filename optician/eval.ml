open Core
open Lang
open Normalized_lang
open Regexcontext

let rec to_empty_exampled_regex (r:Regex.t) : exampled_regex =
  begin match r with
  | Regex.RegExEmpty -> ERegExEmpty
  | Regex.RegExBase s -> ERegExBase (s,[])
  | Regex.RegExConcat (r1,r2) ->
      ERegExConcat (to_empty_exampled_regex r1,to_empty_exampled_regex r2,[])
  | Regex.RegExOr (r1,r2) ->
      ERegExOr
        ((to_empty_exampled_regex r1),
         (to_empty_exampled_regex r2),
         [])
  | Regex.RegExStar r' -> ERegExStar (to_empty_exampled_regex r',[])
  | Regex.RegExVariable t -> ERegExVariable (t,[],[])
  end

type data = string * exampled_regex *
            (string -> exampled_regex -> exampled_regex) list
            * int list * string option

type state =
  | State of (data -> ((state ref * data) list))
  | QAccept

type dfa = (state ref) * (state ref)

let rec regex_to_dfa (c:RegexContext.t) (r:Regex.t) (inside_var:bool) : dfa =
  begin match r with
  | Regex.RegExEmpty ->
      let final = ref QAccept in
      (ref (State (fun _ -> [])), final)
  | Regex.RegExBase s ->
      let final = ref QAccept in
      (ref (State (fun (str,er,recombiners,is,so) ->
        begin match String.chop_prefix ~prefix:s str with
        | None -> []
        | Some str' ->
            if not inside_var then
              begin match er with
              | ERegExBase (b,il) -> [(final,(str',ERegExBase (b,is::il),recombiners,is,so))]
              | _ -> failwith "bad";
              end
            else
              [(final,(str',er,recombiners,is,so))]
        end)), final)
  | Regex.RegExConcat (r1,r2) ->
      let (r1_start_ref,r1_end_ref) = regex_to_dfa c r1 inside_var in
      let (r2_start_ref,r2_end_ref) = regex_to_dfa c r2 inside_var in
      let new_start_fun = (fun (s,er,rc,is,so) ->
        if not inside_var then
          begin match er with
          | ERegExConcat (er1,er2,_) ->
            let rc_swapsecond = (fun _ _ -> er2) in
            [r1_start_ref, (s,er1,rc_swapsecond::rc,is,so)]
          | _ -> failwith "i am a bad programmer"
          end
        else
          [r1_start_ref, (s,er,rc,is,so)]) in
      let new_start = State new_start_fun in
      let new_middle_fun = (fun (s,er,rc,is,so) ->
        if not inside_var then
          begin match rc with
          | h::t -> let rc_rememberfirst = (fun _ er' -> ERegExConcat (er,er',
          extract_example_list er')) in
            [r2_start_ref, (s,h s er,rc_rememberfirst::t,is,so)]
          | [] -> failwith "stupid bad"
          end
        else
          [r2_start_ref, (s,er,rc,is,so)])
      in
      let middle_state = State new_middle_fun in
      r1_end_ref := middle_state;
      let new_end_ref = ref QAccept in
      let new_r2_end =
        if not inside_var then
          State
            (fun (s,er,rc,is,so) ->
              begin match rc with
              | h::t -> [(new_end_ref,(s,h s er, t, is,so))]
              | _ -> failwith "bad coder"
              end)
        else
          State (fun x -> [(new_end_ref,x)])
      in
      r2_end_ref := new_r2_end;
      (ref new_start,new_end_ref)
  | Regex.RegExOr (r1,r2) ->
      let (r1_start_ref,r1_end_ref) = regex_to_dfa c r1 inside_var in
      let (r2_start_ref,r2_end_ref) = regex_to_dfa c r2 inside_var in
      let new_start_fun = (fun (s,er,rc,is,so) ->
        if not inside_var then
          begin match er with
          | ERegExOr (er1,er2,il) ->
              let rc_left = 
                (fun _ er1' ->
                  ERegExOr (er1',er2,is::il)) in
              let rc_right =
                (fun _ er2' ->
                  ERegExOr (er1,er2',is::il)) in
              [(r1_start_ref, (s,er1,rc_left::rc,is,so))
              ;(r2_start_ref, (s,er2,rc_right::rc,is,so))]
          | _ -> failwith "bad programming error" 
          end
        else
          [(r1_start_ref, (s,er,rc,is,so))
          ;(r2_start_ref, (s,er,rc,is,so))]
        ) in
      let new_start = State (new_start_fun) in
      let new_end_ref = ref QAccept in
      let new_inner_end =
        if not inside_var then
          State
            (fun (s,er,rc,is,so) ->
              begin match rc with
              | h::t -> [(new_end_ref,(s,h s er, t, is,so))]
              | _ -> failwith "bad coder1"
              end)
        else
          State (fun x -> [(new_end_ref,x)])
      in
      r1_end_ref := new_inner_end;
      r2_end_ref := new_inner_end;
      (ref new_start,new_end_ref)
  | Regex.RegExStar (inner_r) ->
      let (inner_start_ref,inner_end_ref) = regex_to_dfa c inner_r inside_var in
      let new_end_ref = ref QAccept in
      let new_inner_end_fun =
        if not inside_var then
          (fun (s,er,rc,is,so) ->
            begin match (is,rc) with
            | (i::it,h::t) -> (inner_start_ref, (s,er,rc,(i+1)::it,so))::
              [(new_end_ref,(s,h s er,t,it,so))]
            | _ -> failwith "bad coder2"
            end)
        else
          (fun x -> [(inner_start_ref, x);(new_end_ref, x)])
      in
      inner_end_ref := State new_inner_end_fun;
      let new_start_fun = 
        if not inside_var then
          (fun (s,er,rc,is,so) ->
            begin match er with
            | ERegExStar (er',il) ->
              let rc_add_star =
                (fun _ er'' ->
                  ERegExStar (er'',is::il)) in
                [(inner_end_ref,(s,er',rc_add_star::rc,-1::is,so))]
            | _ -> failwith "error between keyboard and chair"
            end)
        else
          (fun x -> [(inner_end_ref,x)])
      in
      let new_start = State new_start_fun in
      (ref new_start, new_end_ref)
  | Regex.RegExVariable t ->
      let rex = RegexContext.lookup_exn c t in
      let (inner_start_ref,inner_end_ref) = regex_to_dfa c rex true in
      let new_end_ref = ref QAccept in
      let new_start_fun =
        if not inside_var then
          (fun ((s,er,rc,is,_):data) ->
            [(inner_start_ref,(s,er,
              (fun s' er' ->
                begin match er' with
                | ERegExVariable (t,l,il) -> ERegExVariable
                    (t,(String.chop_suffix_exn ~suffix:s' s)::l,is::il)
                | _ -> failwith "programmer < dumpster"
                end)::rc,is,Some s))]
          )
        else
          (fun x -> [(inner_start_ref,x)])
      in
      let new_start_state = State new_start_fun in
      let new_inner_end_fun =
        (fun (s,er,rc,is,so) ->
          if not inside_var then
            begin match rc with
            | [] -> failwith "bad coding2"
            | h::t -> [(new_end_ref,(s,h s er,t,is,so))]
            end
          else
            [(new_end_ref,(s,er,rc,is,so))]) in
      inner_end_ref := (State new_inner_end_fun);
      (ref new_start_state,new_end_ref)
  end

let rec eval_dfa (st:state) ((s,er,recombiners,is,so):data) :
  exampled_regex option =
  begin match st with
  | State (f) ->
        let state_string_list = f (s,er,recombiners,is,so) in
        List.fold_left
        ~f:(fun acc (st',(s',er',rc,is,so)) ->
          begin match acc with
          | None ->  eval_dfa (!st') (s',er',rc,is,so)
          | _ -> acc
          end)
        ~init:None
        state_string_list
  | QAccept ->
      if s = "" then
        Some er
      else
        None
  end

let fast_eval (c:RegexContext.t) (r:Regex.t) (s:string) : bool =
  let (dfa_start,_) = regex_to_dfa c r false in
  not
    (Option.is_empty
       (eval_dfa
          !dfa_start
          (s,(to_empty_exampled_regex r),[],[0],None)))

let regex_to_exampled_regex (rc:RegexContext.t) (r:Regex.t) (es:string list)
                                 : exampled_regex option =
  let (dfa_start,_) = regex_to_dfa rc r false in
  let start_state = !dfa_start in
  List.foldi
  ~f:(fun i er e ->
    begin match er with
    | None -> None
    | Some er' -> eval_dfa start_state (e,er',[],[i],None)
    end)
  ~init:(Some (to_empty_exampled_regex r))
  es
