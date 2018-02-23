open Stdlib
open Lang
open Regex_utilities
open Star_semiring_tree_alignment
open Normalized_lang

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



module StarSemiringTreeRep =
struct
  module PD =
  struct
    type t = Parsings of int list list
    [@@deriving ord, show, hash]

    let make
        (ill:int list list)
      : t =
      Parsings ill

    let are_compatible = is_equal %% compare
  end

  module TD =
  struct
    type t =
      {
        parsings : int list list ;
        strings  : string list   ;
        atoms    : Regex.t list  ;
      }
    [@@deriving ord, show, hash]

    let make
        (parsings:int list list)
        (strings:string list)
        (atoms:Regex.t list)
      : t =
      {
        parsings = parsings ;
        strings  = strings  ;
        atoms    = atoms    ;
      }

    let are_compatible
        (v1:t)
        (v2:t)
      : bool =
      is_equal
        (compare_list
           ~cmp:(compare_list ~cmp:Int.compare)
           v1.parsings
           v2.parsings)

    let get_strings
        (v:t)
      : string list =
      v.strings

    let get_atoms
        (v:t)
      : Regex.t list =
      v.atoms
  end

  module SD =
  struct
    type t = Parsings of int list list
    [@@deriving ord, show, hash]

    let make
        (ill:int list list)
      : t =
      Parsings ill

    let are_compatible = is_equal %% compare
  end

  module BD =
  struct
    type t =
      {
        parsings : int list list ;
        strings  : string list   ;
        regex    : Regex.t       ;
      }
    [@@deriving ord, show, hash]

    let make
        (r:Regex.t)
        (ill:int list list)
        (ss:string list)
      : t =
      {
        parsings = ill ;
        strings  = ss  ;
        regex    = r   ;
      }

    module Alignment =
    struct
      include Lens
      let cost _ = 1.
    end

    let get_alignment
        (v1:t)
        (v2:t)
      : Lens.t option =
      if is_equal (compare v1 v2) then
        Some (Lens.make_ident v1.regex)
      else
        None

    let cost
        (_:unit)
      : float =
      0.
  end

  module Alignment = PlusTimesStarTreeAlignmentOf(PD)(TD)(SD)(BD)
  module Tree = Alignment.Tree

  let exampled_dnf_regex_to_tree
      (ed:exampled_dnf_regex)
    : Tree.t =
    let rec exampled_dnf_regex_to_nonempty_tree
        ((cs,ill):exampled_dnf_regex)
      : Tree.nonempty_t =
      let children =
        List.map
          ~f:exampled_clause_to_nonempty_tree
          cs
      in
      Tree.mk_plus (PD.make ill) children
    and exampled_clause_to_nonempty_tree
        ((ats,ss,ill):exampled_clause)
      : Tree.nonempty_t =
      let (children,regexps) =
        List.unzip
          (List.map
             ~f:(fun ec ->
                 (exampled_atom_to_nonempty_tree ec
                 ,get_atom_regex ec))
             ats)
      in
      Tree.mk_times (TD.make ill ss regexps) children
    and exampled_atom_to_nonempty_tree
        (a:exampled_atom)
      : Tree.nonempty_t =
      begin match a with
        | EAClosed (r,_,_,_,ill,ss) ->
          Tree.mk_base (BD.make r ill ss)
        | EAStar (d,ill,_) ->
          let child = exampled_dnf_regex_to_nonempty_tree d in
          Tree.mk_star (SD.make ill) child
      end
    in
    Tree.mk_nonempty (exampled_dnf_regex_to_nonempty_tree ed)

  let alignment_to_lens
      (a:Alignment.t)
    : Lens.t option =
    let rec nonempty_alignment_to_lens
        (a:Alignment.Nonempty.t)
      : Lens.t =
      begin match a with
        | Alignment.Nonempty.Base l -> l
        | Alignment.Nonempty.Plus _ -> failwith "ah"
        | Alignment.Nonempty.Star _ -> failwith "ah"
        | Alignment.Nonempty.Times (tll,tlr,aligns,projl,projr) ->
          let left_strings = TD.get_strings tll in
          let right_strings = TD.get_strings tlr in
          let last_index_left = List.length left_strings in
          let last_index_right = List.length right_strings in
          let left_atoms = TD.get_atoms tll in
          let right_atoms = TD.get_atoms tlr in
          let left_projections =
            List.mapi
              ~f:(fun right_offset index ->
                  let rx_left = List.nth_exn left_atoms index in
                  ((index
                   ,last_index_right+right_offset)
                  ,Lens.make_disconnect
                      rx_left
                      (Regex.make_base "")
                      (Regex.representative_exn rx_left)
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
                      (Regex.make_base "")
                      rx_right
                      (Regex.representative_exn rx_right)
                      ""))
              projl
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
              ~cmp:(fun ((i1,_),_) ((i2,_),_) -> Int.compare i1 i2)
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
          Lens.make_permute perm alignments
      end
    in
    begin match a with
      | Empty -> None
      | NonemptyTree nt -> Some (nonempty_alignment_to_lens nt)
    end
end
