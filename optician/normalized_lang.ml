open MyStdlib
open Lang
open Lenscontext
open Consts
open Lens_put


(**** Exampled Regex {{{ *****)
type 'a example_data =
  {
    arg1_data : 'a ;
    arg2_data : 'a ;
    output_data : 'a ;
  }
[@@deriving ord, show, hash, make]

let map_example_data
    (f:'a -> 'b)
    (ed:'a example_data)
  : 'b example_data =
  make_example_data
    ~arg1_data:(f ed.arg1_data)
    ~arg2_data:(f ed.arg2_data)
    ~output_data:(f ed.output_data)

let unzip_example_data
    (ed:('a * 'b) example_data)
  : 'a example_data * 'b example_data =
  (map_example_data fst ed
  ,map_example_data snd ed)

let merge_example_data
    (f:'a -> 'b -> 'c)
    (ed1:'a example_data)
    (ed2:'b example_data)
  : 'c example_data =
  make_example_data
    ~arg1_data:(f ed1.arg1_data ed2.arg1_data)
    ~arg2_data:(f ed1.arg2_data ed2.arg2_data)
    ~output_data:(f ed1.output_data ed2.output_data)

let merge_example_data_list
    (f:'a list -> 'b)
    (eds:('a example_data) list)
  : 'b example_data =
  make_example_data
    ~arg1_data:(f (List.map ~f:(fun ed -> ed.arg1_data) eds))
    ~arg2_data:(f (List.map ~f:(fun ed -> ed.arg2_data) eds))
    ~output_data:(f (List.map ~f:(fun ed -> ed.output_data) eds))

type parsing_example_data = (int list list) example_data
[@@deriving ord, show, hash]

type example_string_data = (string list) example_data
[@@deriving ord, show, hash]

type parsings_strings_example_data = (int list * string) list example_data
[@@deriving ord, show, hash]

let extract_parsing
    (psed:parsings_strings_example_data)
  : parsing_example_data =
  map_example_data
    (List.map ~f:fst)
    psed

let extract_strings
    (psed:parsings_strings_example_data)
  : example_string_data =
  map_example_data
    (List.map ~f:snd)
    psed

let empty_parsing_example_data =
  make_example_data
    ~arg1_data:[]
    ~arg2_data:[]
    ~output_data:[]

let empty_string_example_data =
  make_example_data
    ~arg1_data:[]
    ~arg2_data:[]
    ~output_data:[]

type run_mode = Arg1 | Arg2 | Output

module ExampledRegex =
struct
  type t = t_node hash_consed
  and t_node =
    | ERegExEmpty
    | ERegExBase of string * (int list list) example_data
    | ERegExSkip of t
    | ERegExRequire of t
    | ERegExConcat of t * t * (int list list) example_data
    | ERegExOr of t * t * (int list list) example_data * Probability.t
    | ERegExStar of t * (int list list) example_data * Probability.t
    | ERegExClosed of StochasticRegex.t * example_string_data * (int list list) example_data
  [@@deriving show, ord, hash]

  let table = HashConsTable.create 100000
  let hashcons = HashConsTable.hashcons hash_t_node compare_t_node table
  let make_t = hashcons

  let empty = make_t ERegExEmpty

  let mk_base
      (s:string)
      (ill:(int list list) example_data)
    : t =
    make_t (ERegExBase (s,ill))

  let mk_concat
      (er1:t)
      (er2:t)
      (ill:(int list list) example_data)
    : t =
    make_t (ERegExConcat (er1,er2,ill))

  let mk_or
      (er1:t)
      (er2:t)
      (ill:(int list list) example_data)
      (p:Probability.t)
    : t =
    make_t (ERegExOr (er1,er2,ill,p))

  let mk_star
      (er:t)
      (ill:(int list list) example_data)
      (p:Probability.t)
    : t =
    make_t (ERegExStar (er,ill,p))

  let mk_closed
      (sr:StochasticRegex.t)
      (es:example_string_data)
      (ill:(int list list) example_data)
    : t =
    make_t (ERegExClosed (sr,es,ill))

  let mk_skip
      (er:t)
    : t =
    make_t (ERegExSkip er)

  let mk_require
      (er:t)
    : t =
    make_t (ERegExRequire er)

  let uid
      (er:t)
    : int =
    er.tag

  let table = HashConsTable.create 10000
  let hashcons = HashConsTable.hashcons hash_t_node compare_t_node table

  let node er = er.node

  let rec extract_example_data (er:t) : parsing_example_data =
    begin match er.node with
      | ERegExEmpty -> empty_parsing_example_data
      | ERegExBase (_,pd) -> pd
      | ERegExConcat (_,_,pd) -> pd
      | ERegExOr (_,_,pd,_) -> pd
      | ERegExStar (_,pd,_) -> pd
      | ERegExClosed (_,_,pd) -> pd
      | ERegExSkip (er) -> extract_example_data er
      | ERegExRequire (er) -> extract_example_data er
    end

  let extract_iterations_consumed
      (m:run_mode)
      (er:t)
    : int list list =
    begin match m with
      | Arg1 -> (extract_example_data er).arg1_data
      | Arg2 -> (extract_example_data er).arg2_data
      | Output -> (extract_example_data er).output_data
    end

  let took_regex
      (m:run_mode)
      (er:t)
      (iteration:int list) : bool =
    let ill = extract_iterations_consumed m er in
    List.mem ~equal:(=) ill iteration
end

let extract_data
    (m:run_mode)
    (d:'a example_data)
  : 'a =
  begin match m with
    | Arg1 -> d.arg1_data
    | Arg2 -> d.arg2_data
    | Output -> d.output_data
  end

let rec extract_string
    (m:run_mode)
    (er:ExampledRegex.t)
    (iteration:int list)
  : string =
  begin match er.node with
    | ExampledRegex.ERegExEmpty -> failwith "no string"
    | ExampledRegex.ERegExBase (s,_) -> s
    | ExampledRegex.ERegExConcat (er1,er2,_) ->
      (extract_string m er1 iteration) ^
      (extract_string m er2 iteration)
    | ExampledRegex.ERegExOr (er1,er2,_,_) ->
      if ExampledRegex.took_regex m er1 iteration then
        extract_string m er1 iteration
      else
        extract_string m er2 iteration
    | ExampledRegex.ERegExStar (er',_,_) ->
      let valid_iterations =
        List.rev
          (List.filter
             ~f:(fun it -> List.tl_exn it = iteration)
             (ExampledRegex.extract_iterations_consumed m er')) in
      String.concat
        (List.map
           ~f:(extract_string m er')
           valid_iterations)
    | ERegExSkip er ->
      extract_string m er iteration
    | ERegExRequire er ->
      extract_string m er iteration
    | ERegExClosed (_,sl,ill) ->
      let dat_opt = List.findi
          ~f:(fun _ il -> il = iteration)
          (extract_data m ill) in
      begin match dat_opt with
        | None -> failwith "im horrible"
        | Some (i,_) ->
          List.nth_exn (extract_data m sl) i
      end
  end

(*let rec exampled_regex_to_string (r:exampled_regex) : string =
  begin match r with
  | ERegExBase (s,ill) -> paren (s ^ string_of_int_list_list ill)
  | ERegExConcat (r1,r2,ill) ->
    paren
      ((exampled_regex_to_string r1)
       ^ (exampled_regex_to_string r2)
       ^ string_of_int_list_list ill)
  | ERegExOr (r1,r2,ill) ->
    paren(
        paren (exampled_regex_to_string r1)
      ^ "+"
      ^ paren (exampled_regex_to_string r2)
      ^ string_of_int_list_list ill)
  | ERegExStar (r',ill) ->
    paren (paren (exampled_regex_to_string r') ^ "*" ^ string_of_int_list_list ill)
  | ERegExClosed (s,ss,ill) -> paren ((Regex.show s) ^ (bracket (
      String.concat
        ~sep:";"
        ss) ^ string_of_int_list_list ill))
  | ERegExEmpty -> "{}"
  end*)


(***** }}} *****)


(**** Exampled DNF Regex {{{ *****)

module ExampledDNFRegex =
struct
  type exampled_atom =
    | EAClosed of
        StochasticRegex.t *
        ((int list * string) list) example_data
    | EASkip of t * StochasticRegex.t
    | EAStar of t * Probability.t * ((int list * string) list) example_data * StochasticRegex.t

  and exampled_clause =
    (exampled_atom * bool) list
    * string list
    * ((int list) * string) list example_data

  and t =
    (exampled_clause * Probability.t * int) list
    * (int list * string) list example_data
    * int
  [@@deriving ord, show, hash]

  let rec make_required
      ((cps,ils,_):t)
    : t =
    (List.map
       ~f:(fun (c,p,_) -> (make_required_clause c,p,0))
       cps
    ,ils
    ,1)

  and make_required_clause
      ((atoms,ss,eds):exampled_clause)
    : exampled_clause =
    (List.map ~f:(fun (a,_) -> (make_required_atom a,true)) atoms
    ,ss
    ,eds)

  and make_required_atom
      (ea:exampled_atom)
    : exampled_atom =
    begin match ea with
      | EAClosed (sr,ed) -> EAClosed (sr,ed)
      | EASkip (d,sr) -> EASkip (make_required d, sr)
      | EAStar (d,p,ed,sr) -> EAStar (make_required d,p,ed,sr)
    end

  let get_atom_regex (ea:exampled_atom) : StochasticRegex.t =
    begin match ea with
      | EAClosed (r,_) ->
        StochasticRegex.make_closed r
      | EAStar (_,_,_,r) -> r
      | EASkip (_,r) -> r
    end

  let extract_example_data
      ((_,ed,_):t)
    : (int list * string) list example_data =
    ed

  let extract_atom_parsing_data
      (ea:exampled_atom)
    : parsings_strings_example_data =
    begin match ea with
      | EAClosed (_,ilss) -> ilss
      | EAStar (_,_,ilss,_) -> ilss
      | EASkip ((_,ilss,_),_) -> ilss
    end
end

(*let rec exampled_dnf_regex_to_string ((r,ill):exampled_dnf_regex) : string =
  paren ((String.concat
  ~sep:" + "
  (List.map ~f:exampled_clause_to_string r)) ^ "," ^ (string_of_int_list_list ill))

and exampled_clause_to_string ((atoms,strings,examples):exampled_clause) : string =
  paren (bracket (
    String.concat
    ~sep:";"
    (List.map ~f:exampled_atom_to_string atoms)))
  ^ "," ^
  (bracket (
    String.concat
    ~sep:";"
    strings))
  ^ "," ^
  (bracket (
    String.concat
    ~sep:";"

    (List.map ~f:
          (fun il -> bracket (String.concat ~sep:";"
          (List.map ~f:string_of_int il)))
          examples)))

and exampled_atom_to_string (a:exampled_atom) : string =
  begin match a with
  | EAClosed (s,_,_,sl,ill,_) -> paren (
      (Regex.show s) ^ "," ^
      bracket (
        String.concat
        ~sep:";"
        sl) ^ "," ^ string_of_int_list_list ill
      )
  | EAStar (r,ill,_) -> (paren ((exampled_dnf_regex_to_string r) ^ (string_of_int_list_list ill))) ^ "*"
  end*)

(***** }}} *****)

type ordered_exampled_atom =
  | OEAClosed of Regex.t * StochasticRegex.t * Lens.t * string list example_data
  | OEASkip of ordered_exampled_dnf_regex
  | OEAStar of ordered_exampled_dnf_regex

and ordered_exampled_clause = ((ordered_exampled_atom * int) list) list * string
list * (int list list) example_data

and ordered_exampled_dnf_regex = (ordered_exampled_clause * int) list list

[@@deriving show]

(*let rec compare_exampled_atoms (a1:exampled_atom) (a2:exampled_atom) :
  comparison =
    begin match (a1,a2) with
      | (EAClosed (s1,_,_,el1,_,_), EAClosed (s2,_,_,el2,_,_)) ->
        let cmp = Regex.compare s1 s2 in
        if (is_equal cmp) then
          ordered_partition_order
            String.compare
            el1
            el2
        else
          cmp
    | (EAStar (r1,_,_), EAStar (r2,_,_)) -> compare_exampled_dnf_regexs r1 r2
    | _ -> 0
    end 

and compare_exampled_clauses ((atoms1,_,ints1):exampled_clause)
                             ((atoms2,_,ints2):exampled_clause)
  : comparison =
  let cmp = ordered_partition_order compare_exampled_atoms atoms1 atoms2 in
  if (is_equal cmp) then
    ordered_partition_order
      (fun x y -> failwith "BREAK PLZ"(*(compare x y)*))
      ints1
      ints2
  else
    cmp

and compare_exampled_dnf_regexs ((r1,_):exampled_dnf_regex) ((r2,_):exampled_dnf_regex) : comparison =
  ordered_partition_order
    compare_exampled_clauses
      r1
      r2*)

let rec compare_ordered_exampled_atoms (a1:ordered_exampled_atom)
                                       (a2:ordered_exampled_atom)
                                     : comparison =
    begin match (a1,a2) with
      | (OEAClosed (s1,_,_,el1), OEAClosed (s2,_,_,el2)) ->
        let cmp = Regex.compare s1 s2 in
        if is_equal cmp then
          compare_example_string_data
            el1
            el2
        else
          cmp
    | (OEAClosed _,         _) -> -1
    | (        _, OEAClosed _) -> 1
    | (OEAStar r1, OEAStar r2) -> compare_ordered_exampled_dnf_regexs r1 r2
    | (OEAStar _,         _) -> -1
    | (        _, OEAStar _) -> 1
    | (OEASkip r1, OEASkip r2) -> compare_ordered_exampled_dnf_regexs r1 r2
    end

and compare_ordered_exampled_clauses
        ((atoms_partitions1,_,ints1):ordered_exampled_clause)
        ((atoms_partitions2,_,ints2):ordered_exampled_clause)
  : comparison =
  let cmp =
    ordered_partition_dictionary_order
      compare_ordered_exampled_atoms
      atoms_partitions1
      atoms_partitions2
  in
  if is_equal cmp then
    compare_parsing_example_data
      ints1
      ints2
  else
    cmp

and compare_ordered_exampled_dnf_regexs (r1:ordered_exampled_dnf_regex)
  (r2:ordered_exampled_dnf_regex) : comparison =
    ordered_partition_dictionary_order
      compare_ordered_exampled_clauses
        r1
        r2

let rec to_ordered_exampled_atom
    (lc:LensContext.t)
    (a:ExampledDNFRegex.exampled_atom) : ordered_exampled_atom =
  begin match a with
  | EAStar (r,_,_,_) -> OEAStar (to_ordered_exampled_dnf_regex lc r)
  | EASkip (r,_) -> OEASkip (to_ordered_exampled_dnf_regex lc r)
  | EAClosed (sorig,ilel) ->
    let (_,el) = unzip_example_data @$ map_example_data List.unzip ilel in
    if !use_lens_context then
      let (rep_type,converter) = LensContext.shortest_path_to_rep_elt lc (StochasticRegex.to_regex sorig) in
      let ss = map_example_data (List.map ~f:(lens_creater converter)) el in
      OEAClosed (rep_type,sorig,converter,ss)
    else
      let rorig = StochasticRegex.to_regex sorig in
      OEAClosed (rorig,sorig,Lens.Identity rorig,el)

  end

and to_ordered_exampled_clause
    (lc:LensContext.t)
    ((atoms_choices,strings,exnums):ExampledDNFRegex.exampled_clause)
  : ordered_exampled_clause =
  let ordered_atoms = List.map ~f:(to_ordered_exampled_atom lc % fst) atoms_choices in
  let ordered_ordered_atoms =
    sort_and_partition_with_indices
      compare_ordered_exampled_atoms
      ordered_atoms in
  (ordered_ordered_atoms
  ,strings
  ,make_example_data
      ~arg1_data:(List.sort
                    ~compare:(compare_list ~cmp:Int.compare)
                    ((List.map ~f:fst) exnums.arg1_data))
      ~arg2_data:(List.sort
                    ~compare:(compare_list ~cmp:Int.compare)
                    ((List.map ~f:fst) exnums.arg2_data))
      ~output_data:(List.sort
                      ~compare:(compare_list ~cmp:Int.compare)
                      ((List.map ~f:fst) exnums.output_data)))

and to_ordered_exampled_dnf_regex
    (lc:LensContext.t)
    ((r,_,_):ExampledDNFRegex.t)
  : ordered_exampled_dnf_regex =
  let ordered_clauses = List.map ~f:((to_ordered_exampled_clause lc) % fst3) r in
  sort_and_partition_with_indices
    compare_ordered_exampled_clauses
    ordered_clauses


(**** DNF Lens {{{ *****)

type atom_lens =
  | AtomLensIterate of dnf_lens
  | AtomLensVariable of Lens.t

and clause_lens = atom_lens list * Permutation.t * string list * string list

and dnf_lens = clause_lens list * Permutation.t
[@@deriving show]

let rec dnf_lens_to_string ((clause_lenses, permutation):dnf_lens) : string =
  let atom_lens_to_string (a:atom_lens) : string =
    begin match a with
    | AtomLensIterate l -> "iterate" ^ (paren (dnf_lens_to_string l))
    | AtomLensVariable lc -> "librarycall(" ^ (Lens.show lc) ^ ")"
    end in
  let clause_lens_to_string ((atomls,permutation,strings1,strings2):clause_lens) : string =
    paren (
      paren (
        String.concat (List.map ~f:atom_lens_to_string atomls) ~sep:","
      )
      ^ " , " ^
      paren (
        Permutation.show permutation
      )
      ^ " , " ^
      paren (
        String.concat (List.map ~f:(fun x -> "'"^x^"'") strings1) ~sep:","

      )
      ^ " , " ^
      paren (
        String.concat (List.map ~f:(fun x -> "'"^x^"'") strings2) ~sep:","
      )
    ) in
  paren (
    String.concat (List.map ~f:clause_lens_to_string clause_lenses) ~sep:","
  )
  ^ " , " ^
  paren (
    Permutation.show permutation
  )

let rec compare_dnf_lens
    (dl1:dnf_lens)
    (dl2:dnf_lens)
  : comparison =
  pair_compare
    (compare_list ~cmp:compare_clause_lens)
    Permutation.compare
    dl1
    dl2

and compare_clause_lens
    (cl1:clause_lens)
    (cl2:clause_lens)
  : comparison =
  quad_compare
    (compare_list ~cmp:compare_atom_lens)
    Permutation.compare
    (compare_list ~cmp:compare_string)
    (compare_list ~cmp:compare_string)
    cl1
    cl2

and compare_atom_lens
    (al1:atom_lens)
    (al2:atom_lens)
  : comparison =
  begin match (al1,al2) with
    | (AtomLensIterate dl1, AtomLensIterate dl2) ->
      compare_dnf_lens dl1 dl2
    | (AtomLensIterate _, _) -> -1
    | (_, AtomLensIterate _) -> 1
    | (AtomLensVariable l1, AtomLensVariable l2) ->
      Lens.compare l1 l2
  end

(***** }}} *****)

(**** DNF Regex {{{ *****)

type atom =
  | AClosed of Regex.t
  | AStar of dnf_regex
  | ASkip of dnf_regex

and clause = atom list * string list

and dnf_regex = clause list
[@@deriving ord, show, hash]

let empty_dnf_string : dnf_regex = [([],[""])]

let concat_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : dnf_regex =
  cartesian_map
    ~f:(fun (a1s,s1s) (a2s,s2s) -> (a1s@a2s,weld_lists (^) s1s s2s))
    r1
    r2

let or_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : dnf_regex =
  r1 @ r2

let concat_clause_dnf_rx (cl:clause) (r:dnf_regex) : dnf_regex =
  concat_dnf_regexs [cl] r

let concat_dnf_rx_clause (r:dnf_regex) (cl:clause) : dnf_regex =
  concat_dnf_regexs r [cl]

let rec exponentiate_dnf (r:dnf_regex) (n:int) : dnf_regex =
  if n < 0 then
    failwith "invalid exponential"
  else if n = 0 then
    empty_dnf_string
  else
    concat_dnf_regexs (exponentiate_dnf r (n-1)) r

let rec quotiented_star_dnf (r:dnf_regex) (n:int) : dnf_regex =
  if n < 1 then
    failwith "invalid modulation"
  else if n = 1 then
    empty_dnf_string
  else
    or_dnf_regexs (quotiented_star_dnf r (n-1)) (exponentiate_dnf r (n-1))

let singleton_atom (a:atom) : dnf_regex =
  [([a],["";""])]


let rec compare_atoms (a1:atom) (a2:atom) : comparison =
  begin match (a1,a2) with
  | (AClosed s1, AClosed s2) -> Regex.compare s1 s2
  | (AClosed  _, _) -> -1
  | (_, AClosed  _) -> 1
  | (AStar        r1, AStar        r2) -> compare_dnf_regexs r1 r2
  | (AStar        r1, _) -> -1
  | (_, AStar        r1) -> 1
  | (ASkip        r1, ASkip        r2) -> compare_dnf_regexs r1 r2
  end

and compare_clauses ((atoms1,_):clause) ((atoms2,_):clause) : comparison =
  ordered_partition_order compare_atoms atoms1 atoms2

and compare_dnf_regexs (r1:dnf_regex) (r2:dnf_regex) : comparison =
  ordered_partition_order compare_clauses r1 r2



let rec dnf_regex_to_string (clauses:dnf_regex) : string =
  (String.concat (List.map ~f:(fun c -> paren (clause_to_string c)) clauses) ~sep:"+")

and clause_to_string ((atoms,strings):clause) : string =
  paren ((paren (String.concat (List.map ~f:atom_to_string atoms) ~sep:","))
    ^ ","
    ^ (paren (String.concat strings ~sep:",")))

and atom_to_string (a:atom) : string =
  begin match a with
  | AStar dnf_rx -> (paren (dnf_regex_to_string dnf_rx)) ^ "*"
  | AClosed s -> Regex.show s
  | ASkip s -> dnf_regex_to_string s
  end

(***** }}} *****)




