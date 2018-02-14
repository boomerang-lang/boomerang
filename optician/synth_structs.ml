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
  end

  module TD =
  struct
    type t = Parsings of int list list
    [@@deriving ord, show, hash]

    let make
        (ill:int list list)
      : t =
      Parsings ill
  end

  module SD =
  struct
    type t = Parsings of int list list
    [@@deriving ord, show, hash]

    let make
        (ill:int list list)
      : t =
      Parsings ill
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
      include UnitModule
      let cost _ = 1.
    end

    let get_alignment
        (v1:t)
        (v2:t)
      : unit option =
      if is_equal (compare v1 v2) then
        Some ()
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
        ((ats,_,ill):exampled_clause)
      : Tree.nonempty_t =
      let children =
        List.map
          ~f:exampled_atom_to_nonempty_tree
          ats
      in
      Tree.mk_times (TD.make ill) children
    and exampled_atom_to_nonempty_tree
        (a:exampled_atom)
      : Tree.nonempty_t =
      begin match a with
        | EAClosed (r,_,_,ss,ill) ->
          Tree.mk_base (BD.make r ill ss)
        | EAStar (d,ill) ->
          let child = exampled_dnf_regex_to_nonempty_tree d in
          Tree.mk_star (SD.make ill) child
      end
    in
    Tree.mk_nonempty (exampled_dnf_regex_to_nonempty_tree ed)
end
