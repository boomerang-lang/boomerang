open Stdlib
open Lang

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

    let priority
        (qe : t)
      : float =
      Float.of_int qe.expansions_performed
end
