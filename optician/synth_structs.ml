open MyStdlib
open Lang
open Regex_utilities
open Normalized_lang
open Lenscontext
open Lens_put

module QueueElement = struct
  type t =
    {
      r1 : Regex.t;
      r2 : Regex.t;
      expansions_performed : int;
      expansions_inferred : int;
      expansions_forced : int;
    }
  [@@deriving ord, show, hash, make]

  let get_r1
      (q:t)
    : Regex.t =
    q.r1

  let get_r2
      (q:t)
    : Regex.t =
    q.r2

  let get_expansions_performed
      (q:t)
    : int =
    q.expansions_performed

  let get_expansions_inferred
      (q:t)
    : int =
    q.expansions_inferred

  let get_expansions_forced
      (q:t)
    : int =
    q.expansions_forced

  let nqe_to_tuple
      (q:t)
    : Regex.t * Regex.t * int * int * int =
    (q.r1,
     q.r2,
     q.expansions_performed,
     q.expansions_inferred,
     q.expansions_forced)

  let compare
      (q1:t)
      (q2:t)
    : comparison =
    quint_compare
      Regex.compare
      Regex.compare
      (fun _ _ -> 0)
      (fun _ _ -> 0)
      (fun _ _ -> 0)
      (nqe_to_tuple q1)
      (nqe_to_tuple q2)

  module Priority = struct
    type t = float
    [@@deriving show, hash]

    let compare
        (e1:t)
        (e2:t) 
      : int =
      Float.compare e1 e2
  end

  let priority
      (qe : t)
    : Priority.t =
    Float.of_int qe.expansions_performed
end

module SymmetricQueueElement = struct
  type t = 
    {
      r1 : StochasticRegex.t;
      r2 : StochasticRegex.t;
      expansion_choices : int list;
      expansions_performed : int;
      expansions_inferred : int;
      expansions_forced : int;
    }
  [@@deriving ord, show, hash, make]

  let dnf_distance
      (q:t)
    : int =
    IntModule.distance
      (StochasticRegex.dnf_size q.r1)
      (StochasticRegex.dnf_size q.r2)

  let get_r1
      (q:t)
    : StochasticRegex.t =
    q.r1

  let get_r2
      (q:t)
    : StochasticRegex.t =
    q.r2

  let get_expansions_choices
      (q:t)
    : int list =
    q.expansion_choices

  let get_expansions_performed
      (q:t)
    : int =
    q.expansions_performed

  let get_expansions_inferred
      (q:t)
    : int =
    q.expansions_inferred

  let get_expansions_forced
      (q:t)
    : int =
    q.expansions_forced

  let init
      (r1:StochasticRegex.t)
      (r2:StochasticRegex.t)
    : t =
    make
      ~r1:r1
      ~r2:r2
      ~expansion_choices:[]
      ~expansions_performed:0
      ~expansions_inferred:0
      ~expansions_forced:0
      ()

  let nqe_to_tuple
      (q:t)
    : StochasticRegex.t * StochasticRegex.t * int list * int * int * int =
    (q.r1,
     q.r2,
     q.expansion_choices,
     q.expansions_performed,
     q.expansions_inferred,
     q.expansions_forced)

  let compare
      (q1:t)
      (q2:t)
    : comparison =
    sext_compare
      StochasticRegex.compare
      StochasticRegex.compare
      (fun _ _ -> 0)
      (fun _ _ -> 0)
      (fun _ _ -> 0)
      (fun _ _ -> 0)
      (nqe_to_tuple q1)
      (nqe_to_tuple q2)

  module Priority = struct
    type t = float
    [@@deriving show, hash]

    let compare
        (e1:t)
        (e2:t)
      : int =
      Float.compare e1 e2
  end

  let priority
      (qe : t)
    : Priority.t =
    (*let information_content_of_choice
        (n:int)
      : float =
      Math.log2 (Float.of_int n)
    in
    let ps_of_choices =
      List.map
        ~f:information_content_of_choice
        qe.expansion_choices
    in*)
    Float.of_int qe.expansions_performed
    (*+. List.fold_left
      ~f:( +. )
      ~init:0.
      ps_of_choices
    +. 20.*)
end



module StarSemiringTreeRep =
struct
  module PD =
  struct
    type t =
      {
        parsings_strings : (int list * string) list example_data ;
      }
    [@@deriving ord, show, hash, make]

    let are_compatible
        (v1:t)
        (v2:t)
      : bool =
      (*print_endline @$ show_parsings_strings_example_data v1.parsings_strings;
      print_endline @$ show_parsings_strings_example_data v2.parsings_strings;*)
      let r_arg1_parsings = List.map ~f:fst v1.parsings_strings.arg1_data in
      let r_result_parsings = List.map ~f:fst v2.parsings_strings.output_data in
      let l_arg1_parsings = List.map ~f:fst v2.parsings_strings.arg1_data in
      let l_result_parsings = List.map ~f:fst v1.parsings_strings.output_data in
      let is_compat_r =
        is_submultiset
          ~cmp:(compare_list ~cmp:Int.compare)
          r_arg1_parsings
          r_result_parsings
      in
      let is_compat_l =
        is_submultiset
          ~cmp:(compare_list ~cmp:Int.compare)
          l_arg1_parsings
          l_result_parsings
      in
      (*print_endline (string_of_bool (is_compat_r && is_compat_l));*)
      is_compat_r && is_compat_l

    let requires_mapping
        (v:t)
      : bool =
      let arg2_data = v.parsings_strings.arg2_data in
      let output_data = v.parsings_strings.output_data in
      let arg2_data_keys = List.map ~f:fst arg2_data in
      let create_parsings =
        minus_keys_lose_order
          (compare_list ~cmp:Int.compare)
          output_data
          arg2_data_keys
      in
      let valid_creates =
        begin match create_parsings with
          | [] -> true
          | (_,s)::t ->
            let default = s in
            List.fold_left
              ~f:(fun acc (_,s) -> acc && s = default)
              ~init:true
              t
        end
      in
      valid_creates &&
      not
        (List.for_all
           ~f:(fun p ->
               List.mem
                 ~equal:(is_equal %% (compare_list ~cmp:Int.compare))
                 (List.map ~f:fst v.parsings_strings.output_data)
                 p)
           (List.map ~f:fst (v.parsings_strings.arg2_data)))

    module Default = IntModule
    let extract_default _ = failwith "ah"
  end

  module TD =
  struct
    type t =
      {
        parsings_strings : (int list * string) list example_data ;
        strings          : string list                           ;
        atoms            : StochasticRegex.t list                ;
      }
    [@@deriving hash]

    let compare
        (td1:t)
        (td2:t)
      : int =
      compare_parsings_strings_example_data
        td1.parsings_strings
        td2.parsings_strings

    let pp
        (f:Format.formatter)
        (td:t)
      : unit =
      pp_parsings_strings_example_data f td.parsings_strings

    let show
        (x:t)
      : string
      = pp Format.str_formatter x; Format.flush_str_formatter ()

    let hash_fold_t
        (s:Base__Hash.state)
        (td:t)
      : Base__Hash.state =
      hash_fold_parsings_strings_example_data s td.parsings_strings

    let hash
        (td:t)
      : int =
      hash_parsings_strings_example_data td.parsings_strings

    let make
        (parsings_strings:(int list * string) list example_data)
        (strings:string list)
        (atoms:StochasticRegex.t list)
      : t =
      {
        parsings_strings = parsings_strings ;
        strings          = strings          ;
        atoms            = atoms            ;
      }

    let get_strings
        (v:t)
      : string list =
      v.strings

    let get_atoms
        (v:t)
      : StochasticRegex.t list =
      v.atoms

    let are_compatible
        (v1:t)
        (v2:t)
      : bool =
      let r_arg1_parsings = List.map ~f:fst v1.parsings_strings.arg1_data in
      let r_result_parsings = List.map ~f:fst v2.parsings_strings.output_data in
      let l_arg1_parsings = List.map ~f:fst v2.parsings_strings.arg1_data in
      let l_result_parsings = List.map ~f:fst v1.parsings_strings.output_data in
      (*print_endline @$ show_parsings_strings_example_data v1.parsings_strings;
      print_endline @$ show_parsings_strings_example_data v2.parsings_strings;*)
      let is_compat_r =
        is_submultiset
          ~cmp:(compare_list ~cmp:Int.compare)
          r_arg1_parsings
          r_result_parsings
      in
      let is_compat_l =
        is_submultiset
          ~cmp:(compare_list ~cmp:Int.compare)
          l_arg1_parsings
          l_result_parsings
      in
      is_compat_r && is_compat_l

    let requires_mapping
        (v:t)
      : bool =
      let arg2_data = v.parsings_strings.arg2_data in
      let output_data = v.parsings_strings.output_data in
      let arg2_data_keys = List.map ~f:fst arg2_data in
      let create_parsings =
        minus_keys_lose_order
          (compare_list ~cmp:Int.compare)
          output_data
          arg2_data_keys
      in
      let valid_creates =
        begin match create_parsings with
          | [] -> true
          | (_,s)::t ->
            let default = s in
            List.fold_left
              ~f:(fun acc (_,s) -> acc && s = default)
              ~init:true
              t
        end
      in
      valid_creates &&
      let parsings_output_data =
        List.map
          ~f:fst
          v.parsings_strings.output_data
      in
      not
        (List.for_all
           ~f:(fun p ->
               List.mem
                 ~equal:(is_equal %% (compare_list ~cmp:Int.compare))
                 parsings_output_data
                 (fst p))
           v.parsings_strings.arg2_data)

    module Default = IntModule
    let extract_default _ = failwith "ah"
  end

  module SD =
  struct
    type t =
      {
        parsings_strings : (int list * string) list example_data ;
      }
    [@@deriving ord, show, hash, make]

    let are_compatible
        (v1:t)
        (v2:t)
      : bool =
      let r_arg1_parsings = List.map ~f:fst v1.parsings_strings.arg1_data in
      let r_result_parsings = List.map ~f:fst v2.parsings_strings.output_data in
      let l_arg1_parsings = List.map ~f:fst v2.parsings_strings.arg1_data in
      let l_result_parsings = List.map ~f:fst v1.parsings_strings.output_data in
      let is_compat_r =
        is_submultiset
          ~cmp:(compare_list ~cmp:Int.compare)
          r_arg1_parsings
          r_result_parsings
      in
      let is_compat_l =
        is_submultiset
          ~cmp:(compare_list ~cmp:Int.compare)
          l_arg1_parsings
          l_result_parsings
      in
      is_compat_r && is_compat_l

    let requires_mapping
        (v:t)
      : bool =
      let parsings_output_data = List.map ~f:fst v.parsings_strings.output_data in
      let arg2_data = v.parsings_strings.arg2_data in
      let output_data = v.parsings_strings.output_data in
      let arg2_data_keys = List.map ~f:fst arg2_data in
      let create_parsings =
        minus_keys_lose_order
          (compare_list ~cmp:Int.compare)
          output_data
          arg2_data_keys
      in
      let valid_creates =
        begin match create_parsings with
          | [] -> true
          | (_,s)::t ->
            let default = s in
            List.fold_left
              ~f:(fun acc (_,s) -> acc && s = default)
              ~init:true
              t
        end
      in
      valid_creates &&
      not
        (List.for_all
           ~f:(fun p ->
               List.mem
                 ~equal:(is_equal %% (compare_list ~cmp:Int.compare))
                 parsings_output_data
                 p)
           (List.map ~f:fst v.parsings_strings.arg2_data))

    module Default = IntModule
    let extract_default _ = failwith "ah"
  end

  module BD =
  struct
    type parsings_strings_example_data =
      (int list * string) list example_data
    [@@deriving ord, show, hash]

    type hashable_t = parsings_strings_example_data * StochasticRegex.t
    [@@deriving hash,show]

    type t =
      {
        parsings_strings : parsings_strings_example_data ;
        stochastic_regex : StochasticRegex.t             ;
      }

    let to_hashable_t
        (v:t)
      : hashable_t =
      (v.parsings_strings,v.stochastic_regex)

    let hash_fold_t
        (s:Base__Hash.state)
        (v:t)
      = hash_fold_hashable_t s (to_hashable_t v)

    let pp
        (f:Format.formatter)
        (x:t)
      : unit =
      pp_parsings_strings_example_data f x.parsings_strings;
      Format.fprintf f "%s" (StochasticRegex.representative_exn x.stochastic_regex)

    let show
        (x:t)
      : string
      = pp Format.str_formatter x; Format.flush_str_formatter ()


    let hash : t -> int = Base__Hash.run hash_fold_t

    let compare
        (bd1:t)
        (bd2:t)
      : int =
      let bt1 = (bd1.parsings_strings,bd1.stochastic_regex) in
      let bt2 = (bd2.parsings_strings,bd2.stochastic_regex) in
      pair_compare
        compare_parsings_strings_example_data
        StochasticRegex.compare
        bt1
        bt2

    let make
        (stochastic_regex:StochasticRegex.t)
        (ilsl:((int list * string) list example_data))
      : t =
      {
        parsings_strings = ilsl             ;
        stochastic_regex = stochastic_regex ;
      }

    module Alignment =
    struct
      include Lens
      let hash _ = 1
      let hash_fold_t s _ = s
      let cost _ = 0.
    end

    let get_alignment
        (lc:LensContext.t)
        (v1:t)
        (v2:t)
      : Lens.t option =
      let rep_v1 = LensContext.rep_elt lc (StochasticRegex.to_regex v1.stochastic_regex) in
      let rep_v2 = LensContext.rep_elt lc (StochasticRegex.to_regex v2.stochastic_regex) in
      if Regex.compare rep_v1 rep_v2 <> 0 then
        None
      else
        let path =
          LensContext.shortest_path_exn
            lc
            (StochasticRegex.to_regex v1.stochastic_regex)
            (StochasticRegex.to_regex v2.stochastic_regex)
        in
        let compatible_with_fun
            (put_fun:string -> string option -> string)
            (psed1:parsings_strings_example_data)
            (psed2:parsings_strings_example_data)
          : bool =
          let merged_arg_result_parsings_o =
            option_bind
              ~f:(fun merged_arg_result_parsings ->
                  distribute_option
                    (List.map
                       ~f:(fun ((arg1parse,arg1str),(outputparse,outputstr)) ->
                           if IntList.compare arg1parse outputparse = 0 then
                             Some (arg1parse,arg1str,outputstr)
                           else
                             None)
                       merged_arg_result_parsings))
              (List.zip
                 psed1.arg1_data
                 psed2.output_data)
          in
          let rec merge_ordered_examples
              (l1:(int list * string * string) list)
              (l2:(int list * string) list)
            : (string * string option * string) list =
            begin match (l1,l2) with
              | (_,[]) ->
                List.map
                  ~f:(fun (il,s,o) -> (s,None,o))
                  l1
              | ([],_) -> failwith "shouldn't happen"
              | ((il1,sin1,sout)::t1,(il2,sin2)::t2) ->
                begin match make_matchable (IntList.compare il1 il2) with
                  | EQ -> (sin1,Some sin2,sout)::(merge_ordered_examples t1 t2)
                  | LT -> (sin1,None,sout)::(merge_ordered_examples t1 l2)
                  | GT -> failwith "shouldn't happen"
                end
            end
          in
          begin match merged_arg_result_parsings_o with
            | None -> false
            | Some merged_arg_result_parsings ->
              let ordered_l1 =
                List.sort
                  ~compare:(fun (il1,_,_) (il2,_,_) -> IntList.compare il1 il2)
                  merged_arg_result_parsings
              in
              let ordered_l2 =
                List.sort
                  ~compare:(fun (il1,_) (il2,_) -> IntList.compare il1 il2)
                  v2.parsings_strings.arg2_data
              in
              let ssosl =
                merge_ordered_examples
                  ordered_l1
                  ordered_l2
              in
              (*print_endline
              @$
              (string_of_list
                 (string_of_triple
                    ident
                    (string_of_option ident)
                    ident)
                 ssosl);*)
              List.for_all
                ~f:(fun (sin,soin,sout) ->
                    is_equal (String.compare (put_fun sin soin) sout))
                ssosl
          end
        in
        (*print_endline (show_parsings_strings_example_data v1.parsings_strings);
        print_endline (show_parsings_strings_example_data v2.parsings_strings);
        print_endline (StochasticRegex.representative_exn v1.stochastic_regex);
        print_endline (StochasticRegex.representative_exn v2.stochastic_regex);
        print_endline (string_of_int (StochasticRegex.to_regex v1.stochastic_regex).tag);
        print_endline (string_of_int (StochasticRegex.to_regex v2.stochastic_regex).tag);*)
        let lr_compat =
          compatible_with_fun
            (lens_putr_or_creater path)
            v1.parsings_strings
            v2.parsings_strings
        in
        let rl_compat =
          compatible_with_fun
            (lens_putl_or_createl path)
            v2.parsings_strings
            v1.parsings_strings
        in
        (*print_endline (string_of_bool (lr_compat && rl_compat));*)
        if lr_compat && rl_compat then
          Some path
        else
          None

    let cost
        (_:unit)
      : float =
      0.

    let requires_mapping
        (v:t)
      : bool =
      let arg2_data = v.parsings_strings.arg2_data in
      let output_data = v.parsings_strings.output_data in
      let arg2_data_keys = List.map ~f:fst arg2_data in
      let create_parsings =
        minus_keys_lose_order
          (compare_list ~cmp:Int.compare)
          output_data
          arg2_data_keys
      in
      let valid_creates =
        begin match create_parsings with
          | [] -> true
          | (_,s)::t ->
            let default = s in
            List.fold_left
              ~f:(fun acc (_,s) -> acc && s = default)
              ~init:true
              t
        end
      in
      not valid_creates ||
      not
        (List.for_all
           ~f:(fun p ->
               List.mem
                 ~equal:(is_equal
                         %% (pair_compare
                               (compare_list ~cmp:Int.compare)
                               String.compare))
                 v.parsings_strings.output_data
                 p)
           v.parsings_strings.arg2_data)

    module Default = IntModule
    let extract_default _ = failwith "ah"

    let information_content
        (v:t)
      : float =
      StochasticRegex.information_content v.stochastic_regex
  end

  module OptimalAlignment = Star_semiring_tree_alignment_optimal.PlusTimesStarTreeAlignmentOf(PD)(TD)(SD)(BD)
  module Tree = OptimalAlignment.Tree

  module GreedyAlignment =  Star_semiring_alignment_greedy.PlusTimesStarTreeAlignmentOf(PD)(TD)(SD)(BD)

  let clear_alignments () : unit =
    GreedyAlignment.NonemptyNormalizedPlusStarTreeAlignment.clear_dict ();
    OptimalAlignment.NonemptyNormalizedPlusStarTreeAlignment.clear_dict ()

  let exampled_dnf_regex_to_tree
      (lc:LensContext.t)
      (ed:ExampledDNFRegex.t)
    : Tree.t =
    let rec exampled_dnf_regex_to_nonempty_tree
        ((cs,ill,_):ExampledDNFRegex.t)
      : Tree.nonempty_t =
      let children =
        List.map
          ~f:(fun (c,p,i) -> (exampled_clause_to_nonempty_tree c,p,i))
          cs
      in
      Tree.mk_plus (PD.make ill) children
    and exampled_clause_to_nonempty_tree
        ((acts,ss,ill):ExampledDNFRegex.exampled_clause)
      : Tree.nonempty_t =
      let (children,regexps) =
        List.unzip
          (List.map
             ~f:(fun (ec,b) ->
                 ((exampled_atom_to_nonempty_tree ec,b)
                 ,ExampledDNFRegex.get_atom_regex ec))
             acts)
      in
      Tree.mk_times (TD.make ill ss regexps) children
    and exampled_atom_to_nonempty_tree
        (a:ExampledDNFRegex.exampled_atom)
      : Tree.nonempty_t =
      begin match a with
        | EAClosed (rs,ilss) ->
          Tree.mk_base (BD.make rs ilss)
        | EASkip (_,rs) ->
          Tree.mk_base (BD.make rs (ExampledDNFRegex.extract_atom_parsing_data a))
        | EAStar (d,p,ill,_) ->
          let child = exampled_dnf_regex_to_nonempty_tree d in
          Tree.mk_star (SD.make ill) p child
      end
    in
    Tree.mk_nonempty (exampled_dnf_regex_to_nonempty_tree ed)

  module CreateDict = DictOf(IntModule)(IntModule)
  module MatchDict = DictOf(PairOf(IntModule)(IntModule))(Lens)

  let alignment_to_lens
      (a:OptimalAlignment.t)
    : Lens.t option =
    let rec nonempty_alignment_to_lens
        (a:OptimalAlignment.Nonempty.t)
      : Lens.t =
      begin match a with
        | OptimalAlignment.Nonempty.Base l -> l
        | OptimalAlignment.Nonempty.Plus (_,_,matches,createls,creaters) ->
          let cdl =
            CreateDict.from_kvp_list
              (List.mapi ~f:(curry ident) createls)
          in
          let cdr =
            CreateDict.from_kvp_list
              (List.mapi ~f:(curry ident) creaters)
          in
          let md =
            MatchDict.from_kvp_list
              (List.map
                 ~f:(fun (i,j,a) -> ((i,j),nonempty_alignment_to_lens a))
                 matches)
          in
          let (ll,cdl,cdr,md) =
            fold_until_completion
              ~f:(fun (ll,cdl,cdr,md) ->
                  let addable
                      (cd:CreateDict.t)
                      (i:int)
                      (j:int)
                    : bool =
                    begin match CreateDict.lookup cd i with
                      | None -> true
                      | Some j' -> is_equal (Int.compare j j')
                    end
                  in
                  let first_possible_option =
                    MatchDict.first
                      ~f:(fun (i,j) _ ->
                          addable cdl i j
                          && addable cdr j i)
                      md
                  in
                  begin match first_possible_option with
                    | None -> Right (ll,cdl,cdr,md)
                    | Some ((i,j),l) ->
                      let cdl = CreateDict.remove cdl i in
                      let cdr = CreateDict.remove cdr i in
                      let md = MatchDict.remove md (i,j) in
                      Left (l::ll,cdl,cdr,md)
                  end)
              ([],cdl,cdr,md)
          in
          let l_handled =
            fold_on_head_with_default
              ~f:Lens.make_or
              ~default:Lens.zero
              (List.rev ll)
          in
          if MatchDict.is_empty md then
            l_handled
          else
            let mapping_triples =
              List.map
                ~f:(fun ((i,j),l) ->
                    let (stype,vtype) = Typing.type_lens l in
                    let istring = Int.to_string i in
                    let jstring = Int.to_string j in
                    let l_left =
                      Lens.make_concat
                        (Lens.make_const "" jstring)
                        (Lens.make_ident stype)
                    in
                    let l_right =
                      Lens.make_concat
                        (Lens.make_const istring "")
                        (Lens.make_ident vtype)
                    in
                    let l_middle =
                      Lens.make_concat
                        (Lens.make_const (Int.to_string j) (Int.to_string i))
                        l
                    in
                    ((i,j,l_left)
                    ,l_middle
                    ,(j,i,l_right)))
                (MatchDict.as_kvp_list md)
            in
            let (lij_list_l,l_list_m,lij_list_r) =
              List.unzip3
                mapping_triples
            in
            let lens_sort_compare_by_createdict
                (cd:CreateDict.t)
                ((i1,j1,_):int * int * Lens.t)
                ((i2,j2,_):int * int * Lens.t)
              : int =
              let c = Int.compare i1 i2 in
              if is_equal c then
                let jo = CreateDict.lookup cd i1 in
                begin match jo with
                  | None -> 0
                  | Some j ->
                    if is_equal (Int.compare j1 j) then
                      -1
                    else if is_equal (Int.compare j2 j) then
                      1
                    else
                      0
                end
              else
                c
            in
            let lij_list_sorted_l =
              List.sort
                ~compare:(lens_sort_compare_by_createdict cdl)
                lij_list_l
            in
            let lij_list_sorted_r =
              List.sort
                ~compare:(lens_sort_compare_by_createdict cdr)
                lij_list_r
            in
            let l_list_l = List.map ~f:trd3 lij_list_sorted_l in
            let l_list_r = List.map ~f:trd3 lij_list_sorted_r in
            let l_l =
              fold_on_head_exn
                ~f:Lens.make_or
                l_list_l
            in
            let l_m =
              fold_on_head_exn
                ~f:Lens.make_or
                l_list_m
            in
            let l_r =
              fold_on_head_exn
                ~f:Lens.make_or
                l_list_r
            in
            Lens.make_or
              l_handled
              (Lens.make_compose
                 (Lens.make_compose l_l l_m)
                 l_r)
        | OptimalAlignment.Nonempty.Star (_,_,a) ->
          Lens.make_star (nonempty_alignment_to_lens a)
        | OptimalAlignment.Nonempty.Times (tll,tlr,aligns,projl,projr) ->
          let projl = List.sort ~compare:Int.compare projl in
          let projr = List.sort ~compare:Int.compare projr in
          let rec combine_scct_and_sub_lenses
              (sub_lenses:Lens.t list)
              (scct:Permutation.swap_concat_compose_tree)
            : (Lens.t * Lens.t list) =
            begin match scct with
              | SCCTSwap (s1,s2) ->
                let (l1,remaining_left) =
                  combine_scct_and_sub_lenses
                    sub_lenses
                    s1 in
                let (l2,remaining_total) =
                  combine_scct_and_sub_lenses
                    remaining_left
                    s2 in
                (Lens.Swap(l1,l2),remaining_total)
              | SCCTConcat (s1,s2) ->
                let (l1,remaining_left) =
                  combine_scct_and_sub_lenses
                    sub_lenses
                    s1 in
                let (l2,remaining_total) =
                  combine_scct_and_sub_lenses
                    remaining_left
                    s2 in
                (Lens.Concat(l1,l2),remaining_total)
              | SCCTCompose _ ->
                failwith "compose is too ugly, should have failed faster"
              (*let s2size = size_scct s2 in
                let identity_copies = duplicate (LensIdentity (RegExBase "TODO")) s2size in
                let (l1,_) =
                combine_scct_and_sub_lenses
                  identity_copies
                  s1 in
                let (l2,remaining_total) =
                combine_scct_and_sub_lenses
                  sub_lenses
                  s2 in
                (LensCompose(l1,l2),remaining_total)*)
              | SCCTLeaf -> split_by_first_exn sub_lenses
            end
          in
          let left_strings = TD.get_strings tll in
          let right_strings = TD.get_strings tlr in
          let left_atoms = TD.get_atoms tll in
          let right_atoms = TD.get_atoms tlr in
          let last_index_left = List.length left_atoms in
          let last_index_right = List.length right_atoms in
          let (left_string_h,left_strings_t) =
            split_by_first_exn
              left_strings
          in
          let (right_string_h,right_strings_t) =
            split_by_first_exn
              right_strings
          in
          let left_projections =
            List.mapi
              ~f:(fun right_offset index ->
                  let rx_left = List.nth_exn left_atoms index in
                  ((index
                   ,last_index_right+right_offset)
                  ,Lens.make_disconnect
                      (StochasticRegex.to_regex rx_left)
                      Regex.one
                      (StochasticRegex.representative_exn rx_left)
                      ""))
              projl
          in
          let right_projections =
            List.mapi
              ~f:(fun left_offset index ->
                  let rx_right = List.nth_exn right_atoms index in
                  ((last_index_left+left_offset
                   ,index)
                  ,Lens.make_disconnect
                      (Regex.one)
                      (StochasticRegex.to_regex rx_right)
                      ""
                      (StochasticRegex.representative_exn rx_right)))
              projr
          in
          let left_strings_t_full =
            left_strings_t
            @ (List.init
                 (List.length right_projections)
                 (fun _ -> ""))
          in
          let right_strings_t_full =
            right_strings_t
            @ (List.init
                 (List.length left_projections)
                 (fun _ -> ""))
          in
          let aligns =
            List.map
              ~f:(fun (i,j,a) -> ((i,j),nonempty_alignment_to_lens a))
              aligns
          in
          let all_aligns =
            left_projections
            @ right_projections
            @ aligns
          in
          let all_aligns_ordered =
            List.sort
              ~compare:(fun ((i1,_),_) ((i2,_),_) -> Int.compare i1 i2)
              all_aligns
          in
          let (index_pairs,alignments) =
            List.unzip
              all_aligns_ordered
          in
          let perm =
            Permutation.create_from_pairs
              index_pairs
          in
          let right_strings_t_invperm =
            Permutation.apply_inverse_to_list_exn
              perm
              right_strings_t_full
          in
          let string_lss_hd =
            Lens.make_disconnect
               (Regex.make_base left_string_h)
               (Regex.make_base right_string_h)
               left_string_h
               right_string_h
          in
          let string_tl_combos =
            List.zip_exn
              left_strings_t_full
              right_strings_t_invperm
          in
          let aligns_consts_zips =
            List.zip_exn
              alignments
              string_tl_combos
          in
          let atom_string_concats =
            List.map
              ~f:(fun (x,(s1,s2)) ->
                  Lens.make_times
                    x
                    (Lens.make_const s1 s2))
              aligns_consts_zips
          in
          begin match aligns_consts_zips with
            | [] -> string_lss_hd
            | _ ->
              let permutation_scct =
                Permutation.to_swap_concat_compose_tree
                  perm
              in
              if Permutation.has_compose permutation_scct then
                Lens.make_times
                  string_lss_hd
                  (Lens.make_permute perm atom_string_concats)
              else
                Lens.make_times
                  string_lss_hd
                  (fst (combine_scct_and_sub_lenses
                          atom_string_concats
                          permutation_scct))
          end
      end
    in
    begin match a with
      | Empty -> None
      | NonemptyTree nt -> Some (nonempty_alignment_to_lens nt)
    end

  let alignment_to_lens_greedy
      (al:GreedyAlignment.t)
    : Lens.t option =
    let rec nonempty_alignment_to_lens
        (a:GreedyAlignment.Nonempty.t)
      : Lens.t =
      begin match a with
        | GreedyAlignment.Nonempty.Base l -> l
        | GreedyAlignment.Nonempty.Plus (_,_,matches,createls,creaters) ->
          let cdl =
            CreateDict.from_kvp_list
              (List.mapi ~f:(curry ident) createls)
          in
          let cdr =
            CreateDict.from_kvp_list
              (List.mapi ~f:(curry ident) creaters)
          in
          let md =
            MatchDict.from_kvp_list
              (List.map
                 ~f:(fun (i,j,a) -> ((i,j),nonempty_alignment_to_lens a))
                 matches)
          in
          let (ll,cdl,cdr,md) =
            fold_until_completion
              ~f:(fun (ll,cdl,cdr,md) ->
                  let addable
                      (cd:CreateDict.t)
                      (i:int)
                      (j:int)
                    : bool =
                    begin match CreateDict.lookup cd i with
                      | None -> true
                      | Some j' -> is_equal (Int.compare j j')
                    end
                  in
                  let first_possible_option =
                    MatchDict.first
                      ~f:(fun (i,j) _ ->
                          addable cdl i j
                          && addable cdr j i)
                      md
                  in
                  begin match first_possible_option with
                    | None -> Right (ll,cdl,cdr,md)
                    | Some ((i,j),l) ->
                      let cdl = CreateDict.remove cdl i in
                      let cdr = CreateDict.remove cdr i in
                      let md = MatchDict.remove md (i,j) in
                      Left (l::ll,cdl,cdr,md)
                  end)
              ([],cdl,cdr,md)
          in
          let l_handled =
            fold_on_head_with_default
              ~f:Lens.make_or
              ~default:Lens.zero
              (List.rev ll)
          in
          if MatchDict.is_empty md then
            l_handled
          else
            let mapping_triples =
              List.map
                ~f:(fun ((i,j),l) ->
                    let (stype,vtype) = Typing.type_lens l in
                    let istring = Int.to_string i in
                    let jstring = Int.to_string j in
                    let l_left =
                      Lens.make_concat
                        (Lens.make_const "" jstring)
                        (Lens.make_ident stype)
                    in
                    let l_right =
                      Lens.make_concat
                        (Lens.make_const istring "")
                        (Lens.make_ident vtype)
                    in
                    let l_middle =
                      Lens.make_concat
                        (Lens.make_const (Int.to_string j) (Int.to_string i))
                        l
                    in
                    ((i,j,l_left)
                    ,l_middle
                    ,(j,i,l_right)))
                (MatchDict.as_kvp_list md)
            in
            let (lij_list_l,l_list_m,lij_list_r) =
              List.unzip3
                mapping_triples
            in
            let lens_sort_compare_by_createdict
                (cd:CreateDict.t)
                ((i1,j1,_):int * int * Lens.t)
                ((i2,j2,_):int * int * Lens.t)
              : int =
              let c = Int.compare i1 i2 in
              if is_equal c then
                let jo = CreateDict.lookup cd i1 in
                begin match jo with
                  | None -> 0
                  | Some j ->
                    if is_equal (Int.compare j1 j) then
                      -1
                    else if is_equal (Int.compare j2 j) then
                      1
                    else
                      0
                end
              else
                c
            in
            let lij_list_sorted_l =
              List.sort
                ~compare:(lens_sort_compare_by_createdict cdl)
                lij_list_l
            in
            let lij_list_sorted_r =
              List.sort
                ~compare:(lens_sort_compare_by_createdict cdr)
                lij_list_r
            in
            let l_list_l = List.map ~f:trd3 lij_list_sorted_l in
            let l_list_r = List.map ~f:trd3 lij_list_sorted_r in
            let l_l =
              fold_on_head_exn
                ~f:Lens.make_or
                l_list_l
            in
            let l_m =
              fold_on_head_exn
                ~f:Lens.make_or
                l_list_m
            in
            let l_r =
              fold_on_head_exn
                ~f:Lens.make_or
                l_list_r
            in
            Lens.make_or
              l_handled
              (Lens.make_compose
                 (Lens.make_compose l_l l_m)
                 l_r)
        | GreedyAlignment.Nonempty.Star (_,_,a) ->
          Lens.make_star (nonempty_alignment_to_lens a)
        | GreedyAlignment.Nonempty.Times (tll,tlr,aligns,projl,projr) ->
          let projl = List.sort ~compare:Int.compare projl in
          let projr = List.sort ~compare:Int.compare projr in
          let rec combine_scct_and_sub_lenses
              (sub_lenses:Lens.t list)
              (scct:Permutation.swap_concat_compose_tree)
            : (Lens.t * Lens.t list) =
            begin match scct with
              | SCCTSwap (s1,s2) ->
                let (l1,remaining_left) =
                  combine_scct_and_sub_lenses
                    sub_lenses
                    s1 in
                let (l2,remaining_total) =
                  combine_scct_and_sub_lenses
                    remaining_left
                    s2 in
                (Lens.Swap(l1,l2),remaining_total)
              | SCCTConcat (s1,s2) ->
                let (l1,remaining_left) =
                  combine_scct_and_sub_lenses
                    sub_lenses
                    s1 in
                let (l2,remaining_total) =
                  combine_scct_and_sub_lenses
                    remaining_left
                    s2 in
                (Lens.Concat(l1,l2),remaining_total)
              | SCCTCompose _ ->
                failwith "compose is too ugly, should have failed faster"
              (*let s2size = size_scct s2 in
                let identity_copies = duplicate (LensIdentity (RegExBase "TODO")) s2size in
                let (l1,_) =
                combine_scct_and_sub_lenses
                  identity_copies
                  s1 in
                let (l2,remaining_total) =
                combine_scct_and_sub_lenses
                  sub_lenses
                  s2 in
                (LensCompose(l1,l2),remaining_total)*)
              | SCCTLeaf -> split_by_first_exn sub_lenses
            end
          in
          let left_strings = TD.get_strings tll in
          let right_strings = TD.get_strings tlr in
          let left_atoms = TD.get_atoms tll in
          let right_atoms = TD.get_atoms tlr in
          let last_index_left = List.length left_atoms in
          let last_index_right = List.length right_atoms in
          let (left_string_h,left_strings_t) =
            split_by_first_exn
              left_strings
          in
          let (right_string_h,right_strings_t) =
            split_by_first_exn
              right_strings
          in
          let left_projections =
            List.mapi
              ~f:(fun right_offset index ->
                  let rx_left = List.nth_exn left_atoms index in
                  ((index
                   ,last_index_right+right_offset)
                  ,Lens.make_disconnect
                      (StochasticRegex.to_regex rx_left)
                      Regex.one
                      (StochasticRegex.representative_exn rx_left)
                      ""))
              projl
          in
          let right_projections =
            List.mapi
              ~f:(fun left_offset index ->
                  let rx_right = List.nth_exn right_atoms index in
                  ((last_index_left+left_offset
                   ,index)
                  ,Lens.make_disconnect
                      (Regex.one)
                      (StochasticRegex.to_regex rx_right)
                      ""
                      (StochasticRegex.representative_exn rx_right)))
              projr
          in
          let left_strings_t_full =
            left_strings_t
            @ (List.init
                 (List.length right_projections)
                 (fun _ -> ""))
          in
          let right_strings_t_full =
            right_strings_t
            @ (List.init
                 (List.length left_projections)
                 (fun _ -> ""))
          in
          let aligns =
            List.map
              ~f:(fun (i,j,a) -> ((i,j),nonempty_alignment_to_lens a))
              aligns
          in
          let all_aligns =
            left_projections
            @ right_projections
            @ aligns
          in
          let all_aligns_ordered =
            List.sort
              ~compare:(fun ((i1,_),_) ((i2,_),_) -> Int.compare i1 i2)
              all_aligns
          in
          let (index_pairs,alignments) =
            List.unzip
              all_aligns_ordered
          in
          let perm =
            Permutation.create_from_pairs
              index_pairs
          in
          let right_strings_t_invperm =
            Permutation.apply_inverse_to_list_exn
              perm
              right_strings_t_full
          in
          let string_lss_hd =
            Lens.make_disconnect
               (Regex.make_base left_string_h)
               (Regex.make_base right_string_h)
               left_string_h
               right_string_h
          in
          let string_tl_combos =
            List.zip_exn
              left_strings_t_full
              right_strings_t_invperm
          in
          let aligns_consts_zips =
            List.zip_exn
              alignments
              string_tl_combos
          in
          let atom_string_concats =
            List.map
              ~f:(fun (x,(s1,s2)) ->
                  Lens.make_times
                    x
                    (Lens.make_const s1 s2))
              aligns_consts_zips
          in
          begin match aligns_consts_zips with
            | [] -> string_lss_hd
            | _ ->
              let permutation_scct =
                Permutation.to_swap_concat_compose_tree
                  perm
              in
              if Permutation.has_compose permutation_scct then
                Lens.make_times
                  string_lss_hd
                  (Lens.make_permute perm atom_string_concats)
              else
                Lens.make_times
                  string_lss_hd
                  (fst (combine_scct_and_sub_lenses
                          atom_string_concats
                          permutation_scct))
          end
      end
    in
    begin match al with
      | Empty -> None
      | NonemptyTree nt -> Some (nonempty_alignment_to_lens nt)
    end
end
