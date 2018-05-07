open Core
open Lang
open Normalized_lang

let rec to_empty_exampled_regex (r:StochasticRegex.t) : exampled_regex =
  begin match StochasticRegex.node r with
  | Empty -> ERegExEmpty
  | Base s -> ERegExBase (s,empty_parsing_example_data)
  | Concat (r1,r2) ->
      ERegExConcat (to_empty_exampled_regex r1,to_empty_exampled_regex r2,empty_parsing_example_data)
  | Or (r1,r2,p) ->
      ERegExOr
        ((to_empty_exampled_regex r1),
         (to_empty_exampled_regex r2),
         empty_parsing_example_data,
         p)
  | Star (r',p) -> ERegExStar (to_empty_exampled_regex r',empty_parsing_example_data,p)
  | Closed r' -> ERegExClosed (r',empty_string_example_data,empty_parsing_example_data)
  end

type data = run_mode * string * exampled_regex *
            (string -> exampled_regex -> exampled_regex) list
            * int list * string option

type state =
  | State of (data -> ((state ref * data) list))
  | QAccept

let add_data
    (m:run_mode)
    (ed:('a list) example_data)
    (p:'a)
  : ('a list) example_data =
  begin match m with
    | Arg1 -> {ed with arg1_data = p::ed.arg1_data }
    | Arg2 -> {ed with arg2_data = p::ed.arg2_data }
    | Output -> {ed with output_data = p::ed.output_data }
  end

type dfa = (state ref) * (state ref)

let rec regex_to_dfa
    (r:StochasticRegex.t)
    (inside_var:bool)
  : dfa =
  begin match StochasticRegex.node r with
  | Empty ->
      let final = ref QAccept in
      (ref (State (fun _ -> [])), final)
  | Base s ->
      let final = ref QAccept in
      (ref (State (fun (m,str,er,recombiners,is,so) ->
        begin match String.chop_prefix ~prefix:s str with
        | None -> []
        | Some str' ->
            if not inside_var then
              begin match er with
              | ERegExBase (b,il) -> [(final,(m,str',ERegExBase (b,add_data m il is),recombiners,is,so))]
              | _ -> failwith "bad";
              end
            else
              [(final,(m,str',er,recombiners,is,so))]
        end)), final)
  | Concat (r1,r2) ->
      let (r1_start_ref,r1_end_ref) = regex_to_dfa r1 inside_var in
      let (r2_start_ref,r2_end_ref) = regex_to_dfa r2 inside_var in
      let new_start_fun = (fun (m,s,er,rc,is,so) ->
        if not inside_var then
          begin match er with
          | ERegExConcat (er1,er2,_) ->
            let rc_swapsecond = (fun _ _ -> er2) in
            [r1_start_ref, (m,s,er1,rc_swapsecond::rc,is,so)]
          | _ -> failwith "i am a bad programmer"
          end
        else
          [r1_start_ref, (m,s,er,rc,is,so)]) in
      let new_start = State new_start_fun in
      let new_middle_fun = (fun (m,s,er,rc,is,so) ->
        if not inside_var then
          begin match rc with
          | h::t -> let rc_rememberfirst = (fun _ er' -> ERegExConcat (er,er',
          extract_example_data er')) in
            [r2_start_ref, (m,s,h s er,rc_rememberfirst::t,is,so)]
          | [] -> failwith "stupid bad"
          end
        else
          [r2_start_ref, (m,s,er,rc,is,so)])
      in
      let middle_state = State new_middle_fun in
      r1_end_ref := middle_state;
      let new_end_ref = ref QAccept in
      let new_r2_end =
        if not inside_var then
          State
            (fun (m,s,er,rc,is,so) ->
              begin match rc with
              | h::t -> [(new_end_ref,(m,s,h s er, t, is,so))]
              | _ -> failwith "bad coder"
              end)
        else
          State (fun x -> [(new_end_ref,x)])
      in
      r2_end_ref := new_r2_end;
      (ref new_start,new_end_ref)
  | Or (r1,r2,p) ->
      let (r1_start_ref,r1_end_ref) = regex_to_dfa r1 inside_var in
      let (r2_start_ref,r2_end_ref) = regex_to_dfa r2 inside_var in
      let new_start_fun = (fun (m,s,er,rc,is,so) ->
        if not inside_var then
          begin match er with
          | ERegExOr (er1,er2,il,p) ->
              let rc_left = 
                (fun _ er1' ->
                  ERegExOr (er1',er2,add_data m il is,p)) in
              let rc_right =
                (fun _ er2' ->
                  ERegExOr (er1,er2',add_data m il is,p)) in
              [(r1_start_ref, (m,s,er1,rc_left::rc,is,so))
              ;(r2_start_ref, (m,s,er2,rc_right::rc,is,so))]
          | _ -> failwith "bad programming error" 
          end
        else
          [(r1_start_ref, (m,s,er,rc,is,so))
          ;(r2_start_ref, (m,s,er,rc,is,so))]
        ) in
      let new_start = State (new_start_fun) in
      let new_end_ref = ref QAccept in
      let new_inner_end =
        if not inside_var then
          State
            (fun (m,s,er,rc,is,so) ->
              begin match rc with
              | h::t -> [(new_end_ref,(m,s,h s er, t, is,so))]
              | _ -> failwith "bad coder1"
              end)
        else
          State (fun x -> [(new_end_ref,x)])
      in
      r1_end_ref := new_inner_end;
      r2_end_ref := new_inner_end;
      (ref new_start,new_end_ref)
  | Star (inner_r,p) ->
      let (inner_start_ref,inner_end_ref) = regex_to_dfa inner_r inside_var in
      let new_end_ref = ref QAccept in
      let new_inner_end_fun =
        if not inside_var then
          (fun (m,s,er,rc,is,so) ->
            begin match (is,rc) with
            | (i::it,h::t) -> (inner_start_ref, (m,s,er,rc,(i+1)::it,so))::
              [(new_end_ref,(m,s,h s er,t,it,so))]
            | _ -> failwith "bad coder2"
            end)
        else
          (fun x -> [(inner_start_ref, x);(new_end_ref, x)])
      in
      inner_end_ref := State new_inner_end_fun;
      let new_start_fun = 
        if not inside_var then
          (fun (m,s,er,rc,is,so) ->
            begin match er with
            | ERegExStar (er',il,p) ->
              let rc_add_star =
                (fun _ er'' ->
                  ERegExStar (er'',add_data m il is,p)) in
                [(inner_end_ref,(m,s,er',rc_add_star::rc,-1::is,so))]
            | _ -> failwith "error between keyboard and chair"
            end)
        else
          (fun x -> [(inner_end_ref,x)])
      in
      let new_start = State new_start_fun in
      (ref new_start, new_end_ref)
  | Closed r' ->
      let (inner_start_ref,inner_end_ref) = regex_to_dfa r' true in
      let new_end_ref = ref QAccept in
      let new_start_fun =
        if not inside_var then
          (fun ((m,s,er,rc,is,_):data) ->
            [(inner_start_ref,(m,s,er,
              (fun s' er' ->
                begin match er' with
                | ERegExClosed (r',l,il) -> ERegExClosed
                    (r',add_data m l (String.chop_suffix_exn ~suffix:s' s),add_data m il is)
                | _ -> failwith "programmer < dumpster"
                end)::rc,is,Some s))]
          )
        else
          (fun x -> [(inner_start_ref,x)])
      in
      let new_start_state = State new_start_fun in
      let new_inner_end_fun =
        (fun (m,s,er,rc,is,so) ->
          if not inside_var then
            begin match rc with
            | [] -> failwith "bad coding2"
            | h::t -> [(new_end_ref,(m,s,h s er,t,is,so))]
            end
          else
            [(new_end_ref,(m,s,er,rc,is,so))]) in
      inner_end_ref := (State new_inner_end_fun);
      (ref new_start_state,new_end_ref)
  end

let rec eval_dfa (st:state) ((m,s,er,recombiners,is,so):data) :
  exampled_regex option =
  begin match st with
  | State (f) ->
        let state_string_list = f (m,s,er,recombiners,is,so) in
        List.fold_left
        ~f:(fun acc (st',(m,s',er',rc,is,so)) ->
          begin match acc with
          | None ->  eval_dfa (!st') (m,s',er',rc,is,so)
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

let fast_eval (r:StochasticRegex.t) (s:string) : bool =
  let (dfa_start,_) = regex_to_dfa r false in
  not
    (Option.is_empty
       (eval_dfa
          !dfa_start
          (Arg1,s,(to_empty_exampled_regex r),[],[0],None)))

let regex_to_exampled_regex
    (r:StochasticRegex.t)
    (s_ex_data:(int * string) list example_data)
  : exampled_regex option =
  let (dfa_start,_) = regex_to_dfa r false in
  let start_state = !dfa_start in
  List.fold_left
    ~f:(fun er (m,es) ->
        List.fold_left
          ~f:(fun er (i,e) ->
              begin match er with
                | None -> None
                | Some er' -> eval_dfa start_state (m,e,er',[],[i],None)
              end)
          ~init:er
          es)
    ~init:(Some (to_empty_exampled_regex r))
    [(Arg1,s_ex_data.arg1_data)
    ;(Arg2,s_ex_data.arg2_data)
    ;(Output,s_ex_data.output_data)]
