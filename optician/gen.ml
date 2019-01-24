open MyStdlib
open Lenscontext
open Converter
open Lang
open Regex_utilities
open Lens_utilities
open Normalized_lang
open Consts
open Synth_structs
open Expand
open Language_equivalences

module PQ = PriorityQueueOf(QueueElement)
module SPQ = PriorityQueueOf(SymmetricQueueElement)

module type LENS_SYNTHESIZER =
sig
  val gen_lens :
    LensContext.t ->
    Regex.t ->
    Regex.t ->
    create_examples ->
    Lens.t option
end

module type LENSSYNTH_PRIORITY_QUEUE =
sig
  type queue
  type element = QueueElement.t

  val empty : queue
  val from_list : element list -> queue
  val push : queue -> element -> queue
  val push_all : queue -> element list -> queue
  val pop : queue -> (element * int * queue) option
  val length : queue -> int
  val compare : queue -> queue -> comparison
  val to_string : queue -> string
end

module DNFSynth =
struct
  type synthesis_info =
    {
      l                    : dnf_lens                   ;
      oedr1                : ordered_exampled_dnf_regex ;
      specs_visited        : int                        ;
      expansions_performed : int                        ;
      expansions_inferred  : int                        ;
      expansions_forced    : int                        ;
      r1                   : Regex.t                    ;
      r2                   : Regex.t                    ;
    }
    [@@deriving show]

  let rec gen_atom_zipper (lc:LensContext.t)
      (atom1:ordered_exampled_atom)
      (atom2:ordered_exampled_atom)
    : atom_lens =
    begin match (atom1,atom2) with
      | (OEAClosed (_,sorig1,_,_),OEAClosed (_,sorig2,_,_)) ->
        AtomLensVariable (LensContext.shortest_path_exn lc (StochasticRegex.to_regex sorig1) (StochasticRegex.to_regex sorig2))
      | (OEAStar r1, OEAStar r2) ->
        AtomLensIterate (gen_dnf_lens_zipper_internal lc r1 r2)
      | _ -> failwith "invalid"
    end

  and gen_clause_zipper (lc:LensContext.t)
      ((atoms_partitions1,strs1,_):ordered_exampled_clause)
      ((atoms_partitions2,strs2,_):ordered_exampled_clause)
    : clause_lens =
    let zipped_equivs = List.zip_exn atoms_partitions1 atoms_partitions2 in
    let atom_lens_perm_part_list_list =
      List.map
        ~f:(fun (a_list1,a_list2) ->
            let thingy = List.zip_exn a_list1 a_list2 in
            List.map
              ~f:(fun ((a1,i1),(a2,i2)) ->
                  (gen_atom_zipper lc a1 a2,(i1,i2)))
              thingy
          )
        zipped_equivs in
    let atom_lens_perm_part_list = List.concat atom_lens_perm_part_list_list in
    let atom_lens_perm_part_list_by_left_atom =
      List.sort
        ~compare:(fun (_,(x,_)) (_,(y,_)) -> Int.compare x y)
        atom_lens_perm_part_list in
    let (atom_lenses,perm_parts) = List.unzip
        atom_lens_perm_part_list_by_left_atom in
    (atom_lenses,Permutation.create_from_pairs perm_parts,strs1,strs2)


  and gen_dnf_lens_zipper_internal
      (lc:LensContext.t)
      (r1:ordered_exampled_dnf_regex)
      (r2:ordered_exampled_dnf_regex)
    : dnf_lens =
    let zipped_equivs = List.zip_exn r1 r2 in
    let clause_lens_perm_part_list_list =
      List.map
        ~f:(fun (cl_list1,cl_list2) ->
            let thingy = List.zip_exn cl_list1 cl_list2 in
            List.map
              ~f:(fun ((cl1,i1),(cl2,i2)) ->
                  (gen_clause_zipper lc cl1 cl2,(i1,i2)))
              thingy
          )
        zipped_equivs in
    let clause_lens_perm_part_list = List.concat clause_lens_perm_part_list_list in
    let clause_lens_perm_part_list_by_left_clause =
      List.sort
        ~compare:(fun (_,(x,_)) (_,(y,_)) -> Int.compare x y)
        clause_lens_perm_part_list in
    let (clause_lenses,perm_parts) = List.unzip
        clause_lens_perm_part_list_by_left_clause in
    (clause_lenses,Permutation.create_from_pairs perm_parts)

  let kinda_rigid_synth
      (lc:LensContext.t)
      (r1:StochasticRegex.t)
      (r2:StochasticRegex.t)
      (creater_exs:create_examples)
      (createl_exs:create_examples)
      (putr_exs:put_examples)
      (putl_exs:put_examples)
    : (Lens.t * float) option =
    (*print_endline (Regex.show_with_closed (StochasticRegex.to_regex r1));
    print_endline (Regex.show_with_closed (StochasticRegex.to_regex r2));*)
    (*print_endline "wait this should happen";
    if (LensContext.size lc = 4) then
      (print_endline "R1:"; print_endline @$ Regex.show @$ StochasticRegex.to_regex r1;
       print_endline "R2:"; print_endline @$ Regex.show @$ StochasticRegex.to_regex r2;
       print_endline "\n\n\n");*)
    let (i,creater_exs) =
      List.fold_left
        ~f:(fun (i,exs) (lex,rex) ->
            (i+1
            ,((i,lex),(i,rex))::exs))
        ~init:(0,[])
        creater_exs
    in
    let (i,createl_exs) =
      List.fold_left
        ~f:(fun (i,exs) (rex,lex) ->
            (i+1
            ,((i,rex),(i,lex))::exs))
        ~init:(i,[])
        createl_exs
    in
    let (i,putr_exs) =
      List.fold_left
        ~f:(fun (i,exs) (lex,rex,rex') ->
            (i+1
            ,((i,lex),(i,rex),(i,rex'))::exs))
        ~init:(i,[])
        putr_exs
    in
    let (_,putl_exs) =
      List.fold_left
        ~f:(fun (i,exs) (rex,lex,lex') ->
            (i+1
            ,((i,rex),(i,lex),(i,lex'))::exs))
        ~init:(i,[])
        putl_exs
    in
    let (lcreater,rcreater) = List.unzip creater_exs in
    let (rcreatel,lcreatel) = List.unzip createl_exs in
    let (lputr,mputr,rputr) = List.unzip3 putr_exs in
    let (rputl,mputl,lputl) = List.unzip3 putl_exs in
    let left_example_data =
      make_example_data
        ~arg1_data:(lcreater@lputr)
        ~arg2_data:(mputl)
        ~output_data:(lcreatel@lputl)
    in
    let right_example_data =
      make_example_data
        ~arg1_data:(rcreatel@rputl)
        ~arg2_data:(mputr)
        ~output_data:(rcreater@rputr)
    in
    let exampled_r1_opt =
      regex_to_exampled_dnf_regex
        lc
        r1
        left_example_data
    in
    let exampled_r2_opt =
      regex_to_exampled_dnf_regex
        lc
        r2
        right_example_data
    in
    begin match (exampled_r1_opt,exampled_r2_opt) with
      | (Some exampled_r1, Some exampled_r2) ->
        let exampled_r1_tree =
          StarSemiringTreeRep.exampled_dnf_regex_to_tree
            lc
            exampled_r1
        in
        let exampled_r2_tree =
          StarSemiringTreeRep.exampled_dnf_regex_to_tree
            lc
            exampled_r2
        in
        if !optimal then
          let (alignment_opt,cost) =
            StarSemiringTreeRep.OptimalAlignment.get_minimal_alignment_and_cost
              exampled_r1_tree
              exampled_r2_tree
          in
          Option.map
            ~f:(fun al ->
                (Option.value_exn
                   (StarSemiringTreeRep.alignment_to_lens al))
              ,cost)
            alignment_opt
        else
          let (alignment_opt,cost) =
            StarSemiringTreeRep.GreedyAlignment.get_minimal_alignment_and_cost
              exampled_r1_tree
              exampled_r2_tree
          in
          Option.map
            ~f:(fun al ->
                (Option.value_exn
                   (StarSemiringTreeRep.alignment_to_lens_greedy al))
              ,cost)
            alignment_opt
      | _ -> failwith "bad examples"
    end

  let rigid_synth
      (lc:LensContext.t)
      (qe:QueueElement.t)
      (exs:create_examples)
      (count:int)
    : synthesis_info option =
    let r1 = QueueElement.get_r1 qe in
    let r2 = QueueElement.get_r2 qe in
    if (get_dnf_size r1 <> get_dnf_size r2) then
      None
    else
      let (lexs,rexs) = List.unzip exs in
      let lexs =
        make_example_data
          ~arg1_data:(List.mapi ~f:(fun i lex -> (i,lex)) lexs)
          ~arg2_data:[]
          ~output_data:[]
      in
      let rexs =
        make_example_data
          ~arg1_data:(List.mapi ~f:(fun i rex -> (i,rex)) rexs)
          ~arg2_data:[]
          ~output_data:[]
      in
      let exampled_r1_opt = regex_to_exampled_dnf_regex lc (StochasticRegex.from_regex r1) lexs in
      let exampled_r2_opt = regex_to_exampled_dnf_regex lc (StochasticRegex.from_regex r2) rexs in
      begin match (exampled_r1_opt,exampled_r2_opt) with
        | (Some exampled_r1,Some exampled_r2) ->
          let e_o_r1 = to_ordered_exampled_dnf_regex lc exampled_r1 in
          let e_o_r2 = to_ordered_exampled_dnf_regex lc exampled_r2 in
          begin match make_matchable (compare_ordered_exampled_dnf_regexs e_o_r1 e_o_r2) with
            | EQ ->
              Some (
                {
                  l = gen_dnf_lens_zipper_internal lc e_o_r1 e_o_r2;
                  oedr1 = e_o_r1;
                  specs_visited = count;
                  expansions_performed = (QueueElement.get_expansions_performed qe);
                  expansions_inferred = (QueueElement.get_expansions_inferred qe);
                  expansions_forced = (QueueElement.get_expansions_forced qe);
                  r1 = r1;
                  r2 = r2;
                })
            | _ -> None
          end
        | _ -> failwith "bad examples"
      end

  let gen_dnf_lens_and_info_zipper
      (lc:LensContext.t)
      (r1:Regex.t)
      (r2:Regex.t)
      (exs:create_examples)
    : synthesis_info option =
    let count = ref 0 in
    let rec gen_dnf_lens_zipper_queueing
        (queue:PQ.t)
      : synthesis_info option =
      begin match PQ.pop_until_new_priority queue with
        | None -> None
        | Some (f,qes,q) ->
          count := !count + (List.length qes);
          if !verbose then
            (print_endline "popped";
             (*print_endline ("r1: " ^ Regex.show (QueueElement.get_r1 qe));
             print_endline "\n\n";
             print_endline ("r2: " ^ Regex.show (QueueElement.get_r2 qe));
             print_endline "\n\n";*)
             print_endline ("count: " ^ (string_of_int !count));
             print_endline "\n\n";
             print_endline ("priority: " ^ (QueueElement.Priority.show f));
             print_endline "\n\n";
             (*print_endline ("exps_perfed: " ^ (string_of_int (QueueElement.get_expansions_performed qe)));
             print_endline "\n\n";
             print_endline ("exps_inferred: " ^ (string_of_int (QueueElement.get_expansions_inferred qe)));
             print_endline "\n\n";
             print_endline ("exps_forced: " ^ (string_of_int (QueueElement.get_expansions_forced qe)));*)
             print_endline ("\n\n\n"));
          let result_o =
            List.find_map
              ~f:(fun qe ->
                  rigid_synth
                    lc
                    qe
                    exs
                    !count)
              qes
          in
          begin match result_o with
            | Some _ -> result_o
            | None ->
              let queue_elements =
                List.concat_map
                  ~f:(fun qe ->
                      expand
                        lc
                        qe)
                  (List.dedup_and_sort ~compare:QueueElement.compare qes)
              in
              let pq' = PQ.push_all
                   q
                   queue_elements in
              gen_dnf_lens_zipper_queueing
                pq'
          end
      end
    in
      gen_dnf_lens_zipper_queueing
      (PQ.from_list
         [
           QueueElement.make
             ~r1:r1
             ~r2:r2
             ~expansions_performed:0
             ~expansions_inferred:0
             ~expansions_forced:0
           ])


  let expansions_performed_for_gen
      (lc:LensContext.t)
      (r1:Regex.t)
      (r2:Regex.t)
      (exs:create_examples)
    : int option =
    Option.map ~f:(fun x -> x.expansions_performed) (gen_dnf_lens_and_info_zipper lc r1 r2 exs)

  let specs_visited_for_gen
      (lc:LensContext.t)
      (r1:Regex.t)
      (r2:Regex.t)
      (exs:create_examples)
    : int option =
    Option.map ~f:(fun x -> x.specs_visited) (gen_dnf_lens_and_info_zipper lc r1 r2 exs)

  let expansions_inferred_for_gen
      (lc:LensContext.t)
      (r1:Regex.t)
      (r2:Regex.t)
      (exs:create_examples)
    : int option =
    Option.map ~f:(fun x -> x.expansions_inferred) (gen_dnf_lens_and_info_zipper lc r1 r2 exs)

  let expansions_forced_for_gen
      (lc:LensContext.t)
      (r1:Regex.t)
      (r2:Regex.t)
      (exs:create_examples)
    : int option =
    Option.map ~f:(fun x -> x.expansions_forced) (gen_dnf_lens_and_info_zipper lc r1 r2 exs)

  let gen_symmetric_lens
      (keep_going:Float.t)
      (lc:LensContext.t)
      (r1:StochasticRegex.t)
      (r2:StochasticRegex.t)
      (creater_exs:create_examples)
      (createl_exs:create_examples)
      (putr_exs:put_examples)
      (putl_exs:put_examples)
    : Lens.t option =
    let attempts = ref 0 in
    let rec gen_symmetric_lens_queuing
        (pq:SPQ.t)
        (best:Lens.t option)
        (best_cost:float)
        (best_r1:StochasticRegex.t)
        (best_r2:StochasticRegex.t)
      : Lens.t option =
      let calc_comparison_cost
          (distance:Float.t)
          (choice_cost:Float.t)
        : Float.t =
        Float.max
          (distance +. choice_cost -. keep_going)
          0.
      in
      let satisfies_from_no_termination
          (f:Float.t)
        : bool =
        f < Float.max_finite_value && !no_termination
      in
      begin match SPQ.pop_until_new_priority pq with
        | Some (f,qes,pq) ->
          let popped_count = List.length qes in
          attempts := !attempts+popped_count;
          let choice_cost = Math.log2 (Float.of_int popped_count) in
          if !verbose then
            (print_endline "popped";
             print_endline ("attempt #" ^ (string_of_int !attempts));
             (*print_endline ("r1: " ^ StochasticRegex.representative_exn (SymmetricQueueElement.get_r1 qe));
             print_endline "\n\n";
             print_endline ("r2: " ^ StochasticRegex.representative_exn (SymmetricQueueElement.get_r2 qe));
             print_endline "\n\n";*)
             print_endline ("priority: " ^ (SymmetricQueueElement.Priority.show f));
             print_endline ("choice_cost: " ^ (Float.to_string choice_cost));
             print_endline "\n\n";
             print_endline ("bestpriority: " ^ (SymmetricQueueElement.Priority.show best_cost));
             print_endline "\n\n";
             (*print_endline ("exps_perfed: " ^ (string_of_int (SymmetricQueueElement.get_expansions_performed qe)));
             print_endline "\n\n";
             print_endline ("exps_inferred: " ^ (string_of_int (SymmetricQueueElement.get_expansions_inferred qe)));
             print_endline "\n\n";
               print_endline ("exps_forced: " ^ (string_of_int (SymmetricQueueElement.get_expansions_forced qe)))*));
          let comparison_cost = calc_comparison_cost f choice_cost in
          let sorted_qes =
            List.sort
              ~compare:(fun qe1 qe2 ->
                  Int.compare
                    (SymmetricQueueElement.dnf_distance qe1)
                    (SymmetricQueueElement.dnf_distance qe2))
              qes
          in
          if comparison_cost >=. best_cost || satisfies_from_no_termination best_cost then
            (if !verbose then print_endline @$ string_of_float best_cost;
             if !test_dumb_cost_at_correct_pair then
               (no_intelligent_cost := true;
  StarSemiringTreeRep.clear_alignments ();
                assert (Lens.is_eq
                         (Option.value_exn best)
                         (fst @$ Option.value_exn (kinda_rigid_synth
                                              lc
                                              best_r1
                                              best_r2
                                              creater_exs
                                              createl_exs
                                              putr_exs
                                              putl_exs)));
                no_intelligent_cost := false );
             if !test_constants_cost_at_correct_pair then
               (constants_cost := true;
  StarSemiringTreeRep.clear_alignments ();
                assert (Lens.is_eq
                         (Option.value_exn best)
                         (fst @$ Option.value_exn (kinda_rigid_synth
                                              lc
                                              best_r1
                                              best_r2
                                              creater_exs
                                              createl_exs
                                              putr_exs
                                              putl_exs)));
                constants_cost := false );
             best)
          else
            let (best,best_cost,best_r1,best_r2) =
              List.fold_left
                ~f:(fun (best,best_cost,best_r1,best_r2) qe ->
                    if comparison_cost >= best_cost || satisfies_from_no_termination best_cost then
                      (best,best_cost,best_r1,best_r2)
                    else
                      let lco =
                        kinda_rigid_synth
                          lc
                          qe.r1
                          qe.r2
                          creater_exs
                          createl_exs
                          putr_exs
                          putl_exs
                      in
                      begin match lco with
                        | None -> (best,best_cost,best_r1,best_r2)
                        | Some (l,c) ->
                          let cost = c in
                          if cost <=. best_cost then
                            (Some l,cost,qe.r1,qe.r2)
                          else
                            (best,best_cost,best_r1,best_r2)
                      end)
                ~init:(best,best_cost,best_r1,best_r2)
                sorted_qes
            in
            if comparison_cost >=. best_cost || satisfies_from_no_termination best_cost then
              (if !verbose then print_endline @$ string_of_float best_cost;
               if !test_dumb_cost_at_correct_pair then
               (no_intelligent_cost := true;
  StarSemiringTreeRep.clear_alignments ();
                assert (Lens.is_eq
                         (Option.value_exn best)
                         (fst @$ Option.value_exn (kinda_rigid_synth
                                              lc
                                              best_r1
                                              best_r2
                                              creater_exs
                                              createl_exs
                                              putr_exs
                                              putl_exs)));
                no_intelligent_cost := false );
             if !test_constants_cost_at_correct_pair then
               (constants_cost := true;
  StarSemiringTreeRep.clear_alignments ();
                assert (Lens.is_eq
                         (Option.value_exn best)
                         (fst @$ Option.value_exn (kinda_rigid_synth
                                              lc
                                              best_r1
                                              best_r2
                                              creater_exs
                                              createl_exs
                                              putr_exs
                                              putl_exs)));
                constants_cost := false );
             best)
            else
              let new_elements =
                List.concat_map
                  ~f:(fun qe -> Symmetric_expand.expand lc qe)
                  qes
              in
              let pq = SPQ.push_all pq new_elements in
              gen_symmetric_lens_queuing
                pq
                best
                best_cost
                best_r1
                best_r2
        | None -> best
      end
    in
    gen_symmetric_lens_queuing
      (SPQ.singleton
         (SymmetricQueueElement.init r1 r2))
      None
      Float.max_finite_value
      StochasticRegex.empty
      StochasticRegex.empty

  let gen_dnf_lens
      (lc:LensContext.t)
      (r1:Regex.t)
      (r2:Regex.t)
      (exs:create_examples)
    : dnf_lens option =
    Option.map
      ~f:(fun si ->
          if !verbose then
            print_endline @$
            show_synthesis_info
              { si with oedr1 = []; l = ([],Permutation.create_from_pairs []); r1 = Regex.empty; r2 = Regex.empty };
          si.l)
      (gen_dnf_lens_and_info_zipper lc r1 r2 exs)

  let gen_lens
      (lc:LensContext.t)
      (r1:Regex.t)
      (r2:Regex.t)
      (exs:create_examples)
    : Lens.t option =
    let dnf_lens_option = gen_dnf_lens lc r1 r2 exs in
    Option.map
      ~f:dnf_lens_to_lens
      dnf_lens_option

  let num_possible_choices
      (lc:LensContext.t)
      (r1:Regex.t)
      (r2:Regex.t)
      (exs:create_examples)
    : float option =
    let rec get_possibible_lenses_oedr
        (oedr:ordered_exampled_dnf_regex)
      : float =
      List.fold_left
        ~f:(fun acc oecl ->
            acc *.
            (Math.factorial (List.length oecl)) *.
            (get_possible_lenses_oec (fst (List.hd_exn oecl))))
        ~init:1.0
        oedr
    and get_possible_lenses_oec
        ((oec,_,_):ordered_exampled_clause)
      : float =
      List.fold_left
        ~f:(fun acc oeal ->
            acc *.
            (Math.factorial (List.length oeal)) *.
            (get_possible_lenses_oea (fst (List.hd_exn oeal))))
        ~init:1.0
        oec
    and get_possible_lenses_oea
        (oea:ordered_exampled_atom)
      : float =
      begin match oea with
      | OEAClosed _ -> 1.0
      | OEAStar dr ->
        get_possibible_lenses_oedr dr
      | OEASkip _ -> 1.0
      end
    in
    Option.map
      ~f:(fun x -> get_possibible_lenses_oedr x.oedr1)
      (gen_dnf_lens_and_info_zipper lc r1 r2 exs)
end

let expansions_performed_for_gen = DNFSynth.expansions_performed_for_gen
let specs_visited_for_gen = DNFSynth.specs_visited_for_gen
let expansions_inferred_for_gen = DNFSynth.expansions_inferred_for_gen
let expansions_forced_for_gen = DNFSynth.expansions_forced_for_gen
let num_possible_choices = DNFSynth.num_possible_choices

let gen_symmetric_lens
    (keep_going:Float.t)
    (existing_lenses:(Lens.t * Regex.t * Regex.t) list)
    (r1:Regex.t)
    (r2:Regex.t)
    (creater_exs:create_examples)
    (createl_exs:create_examples)
    (putr_exs:put_examples)
    (putl_exs:put_examples)
  : Lens.t option =
  let existing_lenses =
    List.map
      ~f:(fun (l,r1,r2) -> (l,iteratively_deepen r1 [], iteratively_deepen r2 []))
      existing_lenses
  in
  let (lc,ss) = LensContext.insert_list LensContext.empty existing_lenses in
  Star_semiring_alignment_greedy.lc := lc;
  Star_semiring_tree_alignment_optimal.lc := lc;
  StarSemiringTreeRep.clear_alignments ();
  (*print_endline "lenscontext!";
  print_endline @$ LensContext.show lc;*)
  let r1 = iteratively_deepen r1 ss in
  let r2 = iteratively_deepen r2 ss in
  let r1 = StochasticRegex.from_regex r1 in
  let r2 = StochasticRegex.from_regex r2 in
  if !verbose then
    print_endline "Synthesis Start";
  let lens_option =
    DNFSynth.gen_symmetric_lens
      keep_going
      lc
      r1
      r2
      creater_exs
      createl_exs
      putr_exs
      putl_exs
  in
  if !verbose then
    print_endline "Synthesis End";
  if !simplify_generated_lens then
    Option.map
      ~f:simplify_lens
      lens_option
  else
    lens_option

let gen_lens
    (keep_going:Float.t)
    (existing_lenses:(Lens.t * Regex.t * Regex.t) list)
    (r1:Regex.t)
    (r2:Regex.t)
    (creater_exs:create_examples)
    (createl_exs:create_examples)
    (putr_exs:put_examples)
    (putl_exs:put_examples)
  : Lens.t option =
  if !gen_symmetric then
    let lo =
      gen_symmetric_lens
        keep_going
        existing_lenses
        r1
        r2
        creater_exs
        createl_exs
        putr_exs
        putl_exs
    in
    if !simplify_generated_lens then
      Option.map
        ~f:(simplify_lens)
        lo
    else
      lo
  else
    let existing_lenses =
      List.map
        ~f:(fun (l,r1,r2) -> (l,iteratively_deepen r1 [], iteratively_deepen r2 []))
        existing_lenses
    in
    let (lc,ss) = LensContext.insert_list LensContext.empty existing_lenses in
    let r1 = iteratively_deepen r1 ss in
    let r2 = iteratively_deepen r2 ss in
    if !verbose then
      print_endline "Synthesis Start";
    let lens_option = DNFSynth.gen_lens lc r1 r2 creater_exs in
    if !verbose then
      print_endline "Synthesis End";
    if !simplify_generated_lens then
      Option.map
        ~f:(simplify_lens)
        lens_option
    else
      lens_option

