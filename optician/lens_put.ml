open MyStdlib
open Lang
open Typing

(*let rec lens_createl_internal
    (l:Lens.t)
    (er:exampled_regex)
    (iteration:int list)
  : string =
  begin match (l,er) with
    | (Lens.Disconnect (r1,r2,s1,s2), ERegExBase (s2',_)) ->
      s1
    | (Lens.Closed (_,l'), _) ->
      let relevant_string = extract_string Arg1 er iteration in
      Lens.get_left_create_closed l' relevant_string
    | (Lens.Concat (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_createl_internal l1 er1 iteration) ^
        (lens_createl_internal l2 er2 iteration)
    | (Lens.Swap (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_createl_internal l1 er2 iteration) ^
        (lens_createl_internal l2 er1 iteration)
    | (Lens.Union (l1,l2), ERegExOr (er1,er2,_,_)) ->
        if took_regex Arg1 er1 iteration then
          lens_createl_internal l1 er1 iteration
        else
          lens_createl_internal l2 er2 iteration
    | (Lens.Compose (l1,l2),_) ->
      let intermediary_string = lens_createl_internal l1 er iteration in
      let (_,intermediary_regex) = type_lens l2 in
      let intermediary_er_o =
        regex_to_exampled_regex
          (StochasticRegex.from_regex intermediary_regex)
          (make_example_data
             ~arg1_data:[(0,intermediary_string)]
             ~arg2_data:[]
             ~output_data:[])
      in
      begin match intermediary_er_o with
        | None -> failwith "bad input to lens"
        | Some intermediary_er -> lens_createl_internal l2 intermediary_er [0]
      end
    | (Lens.Iterate l', ERegExStar (er',_,_)) ->
        let valid_iterations =
          List.rev
            (List.filter
              ~f:(fun it -> List.tl_exn it = iteration)
              (extract_iterations_consumed Arg1 er')) in
        String.concat
          (List.map
            ~f:(lens_createl_internal l' er')
            valid_iterations)
    | (Lens.Identity _, _) ->
      extract_string Arg1 er iteration
    | (Lens.Inverse l', _) ->
      lens_creater_internal l' er iteration
    | (Lens.Permute (p,ls), _) ->
      let rec extract_reversed_concat_list
          (er:exampled_regex)
          (n:int)
        : exampled_regex list =
        if n = 0 then
          []
        else if n = 1 then
          [er]
        else
          begin match er with
            | ERegExConcat(er1,er2,_) ->
              er2::(extract_reversed_concat_list er1 (n-1))
            | _ -> failwith ("bad typecheck disagreeing types1" ^ Lens.show l)
          end
      in
      let concat_list =
        List.rev
          (extract_reversed_concat_list er (List.length ls))
      in
      let permed_concat_list =
        Permutation.apply_inverse_to_list_exn
          p
          concat_list
      in
      let er_l_list = List.zip_exn permed_concat_list ls in
      List.fold
        ~f:(fun s (er,l) ->
            s ^ (lens_createl_internal l er iteration))
        ~init:""
        er_l_list
    | _ -> failwith ("bad typecheck disagreeing types2" ^ Lens.show l)
  end

and lens_creater_internal
    (l:Lens.t)
    (er:exampled_regex)
    (iteration:int list)
  : string =
  begin match (l,er) with
    | (Lens.Disconnect (r1,r2,s1,s2), ERegExBase (s2',_)) ->
      s2
    | (Lens.Closed (_,l'), _) ->
      let relevant_string = extract_string Arg1 er iteration in
      Lens.get_right_create_closed l' relevant_string
    | (Lens.Concat (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_creater_internal l1 er1 iteration) ^
        (lens_creater_internal l2 er2 iteration)
    | (Lens.Swap (l1,l2), ERegExConcat (er1,er2,_)) ->
        (lens_creater_internal l2 er2 iteration) ^
        (lens_creater_internal l1 er1 iteration)
    | (Lens.Union (l1,l2), ERegExOr (er1,er2,_,_)) ->
        if took_regex Arg1 er1 iteration then
          lens_creater_internal l1 er1 iteration
        else
          lens_creater_internal l2 er2 iteration
    | (Lens.Compose (l1,l2),_) ->
      let intermediary_string = lens_creater_internal l2 er iteration in
      let (intermediary_regex,_) = type_lens l1 in
      let intermediary_er_o = regex_to_exampled_regex
          (StochasticRegex.from_regex intermediary_regex)
          (make_example_data
             ~arg1_data:[(0,intermediary_string)]
             ~arg2_data:[]
             ~output_data:[])
      in
      begin match intermediary_er_o with
        | None -> failwith "bad input to lens"
        | Some intermediary_er -> lens_creater_internal l1 intermediary_er [0]
      end
    | (Lens.Iterate l', ERegExStar (er',_,_)) ->
        let valid_iterations =
          List.rev
            (List.filter
              ~f:(fun it -> List.tl_exn it = iteration)
              (extract_iterations_consumed Arg1 er')) in
        String.concat
          (List.map
            ~f:(lens_creater_internal l' er')
            valid_iterations)
    | (Lens.Identity _, _) ->
      extract_string Arg1 er iteration
    | (Lens.Inverse l', _) ->
      lens_createl_internal l' er iteration
    | (Lens.Permute (p,ls), _) ->
      let rec extract_reversed_concat_list
          (er:exampled_regex)
          (n:int)
        : exampled_regex list =
        if n = 0 then
          []
        else if n = 1 then
          [er]
        else
          begin match er with
            | ERegExConcat(er1,er2,_) ->
              er2::(extract_reversed_concat_list er1 (n-1))
            | _ -> failwith ("bad typecheck disagreeing types3" ^ Lens.show l)
          end
      in
      let concat_list =
        List.rev
          (extract_reversed_concat_list er (List.length ls))
      in
      let er_l_list = List.zip_exn concat_list ls in
      let permed_er_l_list =
        Permutation.apply_to_list_exn
          p
          er_l_list
      in
      List.fold
        ~f:(fun s (er,l) ->
            s ^ (lens_createl_internal l er iteration))
        ~init:""
        permed_er_l_list
    | _ -> failwith ("bad typecheck disagreeing types4" ^ Lens.show l)
  end

let lens_creater
    (l:Lens.t)
    (s:string)
  : string =
  let (sr,_) = type_lens l in
  let exampled_sr_o = regex_to_exampled_regex (StochasticRegex.from_regex sr)
      (make_example_data
         ~arg1_data:[(0,s)]
         ~arg2_data:[]
         ~output_data:[])
  in
  begin match exampled_sr_o with
    | None -> failwith ("bad input to lens~: " ^ s ^ " " ^ (Regex.show sr))
    | Some exampled_sr -> lens_creater_internal l exampled_sr [0]
  end

let lens_createl
    (l:Lens.t)
    (s:string)
  : string =
  let (_,tr) = type_lens l in
  let exampled_sr_o = regex_to_exampled_regex
      (StochasticRegex.from_regex tr)
      (make_example_data
         ~arg1_data:[(0,s)]
         ~arg2_data:[]
         ~output_data:[])
  in
  begin match exampled_sr_o with
    | None -> failwith ("bad input to lens: " ^ s)
    | Some exampled_sr -> lens_createl_internal l exampled_sr [0]
  end
*)

let rec lens_creater
    (l:Lens.t)
    (s:string)
  : string =
  begin match l with
    | Identity _ -> s
    | Closed (_,fn_container) ->
      fn_container.creater s
    | Compose (l1,l2) ->
      let s' = lens_creater l1 s in
      lens_creater l2 s'
    | Inverse l ->
      lens_createl l s
    | _ -> failwith ("shouldn't have to perform" ^ Lens.show l)
  end

and lens_createl
    (l:Lens.t)
  : string -> string =
  lens_creater (Lens.invert l)

let rec lens_putr
    (l:Lens.t)
    (s:string)
    (t:string)
  : string =
  begin match l with
    | Identity _ -> s
    | Closed (_,fn_container) ->
      fn_container.putr s t
    | Compose (l1,l2) ->
      let u = lens_createl l2 t in
      let u' = lens_putr l1 s u in
      lens_putr l2 u' t
    | Inverse l ->
      lens_putl l s t
    | _ -> failwith ("shouldn't have to perform" ^ Lens.show l)
  end

and lens_putl
    (l:Lens.t)
    (s:string)
    (t:string)
  : string =
  lens_putr (Lens.invert l) s t

let lens_putr_or_creater
    (l:Lens.t)
    (sin:string)
    (sin2o:string option)
  : string =
  begin match sin2o with
    | None -> lens_creater l sin
    | Some sin2 -> lens_putr l sin sin2
  end

let lens_putl_or_createl
    (l:Lens.t)
    (sin:string)
    (sin2o:string option)
  : string =
  lens_putr_or_creater
    (Lens.invert l)
    sin
    sin2o
