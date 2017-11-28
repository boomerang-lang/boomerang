open Stdlib
open Lang


(**** Exampled Regex {{{ *****)

type exampled_regex =
  | ERegExEmpty
  | ERegExBase of string * (int list list)
  | ERegExConcat of exampled_regex * exampled_regex * (int list list)
  | ERegExOr of exampled_regex  * exampled_regex * (int list list)
  | ERegExStar of exampled_regex * (int list list)
  | ERegExVariable of Id.t * string list * (int list list)

let extract_iterations_consumed (er:exampled_regex) : int list list =
  begin match er with
    | ERegExEmpty -> []
    | ERegExBase (_,ill) -> ill
    | ERegExConcat (_,_,ill) -> ill
    | ERegExOr (_,_,ill) -> ill
    | ERegExStar (_,ill) -> ill
    | ERegExVariable (_,_,ill) -> ill
  end

let took_regex (er:exampled_regex)
    (iteration:int list) : bool =
  let ill = extract_iterations_consumed er in
  List.mem ~equal:(=) ill iteration

let rec extract_string (er:exampled_regex) (iteration:int list)
  : string =
  begin match er with
    | ERegExEmpty -> failwith "no string"
    | ERegExBase (s,_) -> s
    | ERegExConcat (er1,er2,_) ->
      (extract_string er1 iteration) ^
      (extract_string er2 iteration)
    | ERegExOr (er1,er2,_) ->
      if took_regex er1 iteration then
        extract_string er1 iteration
      else
        extract_string er2 iteration
    | ERegExStar (er',_) ->
        let valid_iterations =
          List.rev
            (List.filter
              ~f:(fun it -> List.tl_exn it = iteration)
              (extract_iterations_consumed er')) in
        String.concat
          (List.map
            ~f:(extract_string er')
            valid_iterations)
    | ERegExVariable (_,sl,ill) ->
        let dat_opt = List.findi
          ~f:(fun _ il -> il = iteration)
          ill in
        begin match dat_opt with
        | None -> failwith "im horrible"
        | Some (i,_) ->
            List.nth_exn sl i
        end
  end

let extract_example_list (er:exampled_regex) : int list list =
  begin match er with
  | ERegExEmpty -> []
  | ERegExBase (_,ill) -> ill
  | ERegExConcat (_,_,ill) -> ill
  | ERegExOr (_,_,ill) -> ill
  | ERegExStar (_,ill) -> ill
  | ERegExVariable (_,_,ill) -> ill
  end

let rec exampled_regex_to_string (r:exampled_regex) : string =
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
  | ERegExVariable (s,ss,ill) -> paren ((Id.show s) ^ (bracket (
      String.concat
        ~sep:";"
        ss) ^ string_of_int_list_list ill))
  | ERegExEmpty -> "{}"
  end


(***** }}} *****)


(**** Exampled DNF Regex {{{ *****)

type exampled_atom =
  | EAVariable of Id.t * Id.t * Lens.t * string list * int list list
  | EAStar of exampled_dnf_regex * int list list

and exampled_clause = (exampled_atom) list * string list * (int list list)

and exampled_dnf_regex = exampled_clause list * int list list

let rec exampled_dnf_regex_to_string ((r,ill):exampled_dnf_regex) : string =
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
  | EAVariable (s,_,_,sl,ill) -> paren (
      (Id.show s) ^ "," ^
      bracket (
        String.concat
        ~sep:";"
        sl) ^ "," ^ string_of_int_list_list ill
      )
  | EAStar (r,ill) -> (paren ((exampled_dnf_regex_to_string r) ^ (string_of_int_list_list ill))) ^ "*"
  end

(***** }}} *****)

type ordered_exampled_atom =
  | OEAVar of Id.t * Id.t * Lens.t * string list
  | OEAStar of ordered_exampled_dnf_regex

and ordered_exampled_clause = ((ordered_exampled_atom * int) list) list * string
list * (int list list)

and ordered_exampled_dnf_regex = (ordered_exampled_clause * int) list list

let rec compare_exampled_atoms (a1:exampled_atom) (a2:exampled_atom) :
  comparison =
    begin match (a1,a2) with
      | (EAVariable (s1,_,_,el1,_), EAVariable (s2,_,_,el2,_)) ->
        let cmp = Id.compare s1 s2 in
        if (is_equal cmp) then
          ordered_partition_order
            compare
            el1
            el2
        else
          cmp
    | (EAStar (r1,_), EAStar (r2,_)) -> compare_exampled_dnf_regexs r1 r2
    | _ -> 0
    end 

and compare_exampled_clauses ((atoms1,_,ints1):exampled_clause)
                             ((atoms2,_,ints2):exampled_clause)
  : comparison =
  let cmp = ordered_partition_order compare_exampled_atoms atoms1 atoms2 in
  if (is_equal cmp) then
    ordered_partition_order
      (fun x y -> (compare x y))
      ints1
      ints2
  else
    cmp

and compare_exampled_dnf_regexs ((r1,_):exampled_dnf_regex) ((r2,_):exampled_dnf_regex) : comparison =
  ordered_partition_order
    compare_exampled_clauses
      r1
      r2

let rec compare_ordered_exampled_atoms (a1:ordered_exampled_atom)
                                       (a2:ordered_exampled_atom)
                                     : comparison =
    begin match (a1,a2) with
      | (OEAVar (s1,_,_,el1), OEAVar (s2,_,_,el2)) ->
        let cmp = compare s1 s2 in
        if is_equal cmp then
          compare_list
            ~cmp:compare
            el1
            el2
        else
          cmp
    | (OEAStar r1, OEAStar r2) -> compare_ordered_exampled_dnf_regexs r1 r2
    | (OEAStar _, OEAVar _) -> 1
    | (OEAVar _, OEAStar _) -> -1
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
    compare_list
      ~cmp:(compare)
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

let rec to_ordered_exampled_atom (a:exampled_atom) : ordered_exampled_atom =
  begin match a with
  | EAVariable (s,sorig,lmap,el,_) -> OEAVar (s,sorig,lmap,el)
  | EAStar (r,_) -> OEAStar (to_ordered_exampled_dnf_regex r)
  end

and to_ordered_exampled_clause ((atoms,strings,exnums):exampled_clause) : ordered_exampled_clause =
  let ordered_atoms = List.map ~f:to_ordered_exampled_atom atoms in
  let ordered_ordered_atoms =
    sort_and_partition_with_indices
      compare_ordered_exampled_atoms
      ordered_atoms in
  (ordered_ordered_atoms,strings,(List.sort ~cmp:compare exnums))

and to_ordered_exampled_dnf_regex ((r,_):exampled_dnf_regex)
        : ordered_exampled_dnf_regex =
  let ordered_clauses = List.map ~f:to_ordered_exampled_clause r in
  sort_and_partition_with_indices
    compare_ordered_exampled_clauses
    ordered_clauses


(**** DNF Lens {{{ *****)

type atom_lens =
  | AtomLensIterate of dnf_lens
  | AtomLensVariable of Lens.t

and clause_lens = atom_lens list * Permutation.t * string list * string list

and dnf_lens = clause_lens list * Permutation.t

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
  | AVar of Id.t
  | AStar of dnf_regex

and clause = atom list * string list

and dnf_regex = clause list

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
  | (AVar s1, AVar s2) -> compare s1 s2
  | (AVar  _, AStar         _) -> -1
  | (AStar         _, AVar  _) -> 1
  | (AStar        r1, AStar        r2) -> compare_dnf_regexs r1 r2
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
  | AVar s -> Id.show s
  end

(***** }}} *****)




