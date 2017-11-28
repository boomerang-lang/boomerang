open Stdlib
open Printf


(**** General {{{ *****)
exception Internal_error of string
let internal_error f s = raise @@ Internal_error (sprintf "(%s) %s" f s)
(***** }}} *****)


(**** Regex {{{ *****)
module Id =
struct
  type t = Id of string
  [@@deriving ord, show, hash]

  let string_of_id
      (Id v:t)
    : string =
    v

  let make
      (s:string)
    : t =
    Id s
end
(***** }}} *****)



(**** Regex {{{ *****)

module Regex =
struct
  type t =
    | RegExEmpty
    | RegExBase of string
    | RegExConcat of t * t
    | RegExOr of t * t 
    | RegExStar of t
    | RegExVariable of Id.t
  [@@deriving ord, show, hash]

  let one = RegExBase ""

  let zero = RegExEmpty

  let separate_plus
      (r:t)
    : (t * t) option =
    begin match r with
      | RegExOr (r1,r2) -> Some (r1,r2)
      | _ -> None
    end

  let separate_times
      (r:t)
    : (t * t) option =
    begin match r with
      | RegExConcat (r1,r2) -> Some (r1,r2)
      | _ -> None
    end

  let separate_star
      (r:t)
    : t option =
    begin match r with
      | RegExStar r' -> Some r'
      | _ -> None
    end

  let separate_var
      (r:t)
    : Id.t option =
    begin match r with
      | RegExVariable v -> Some v
      | _ -> None
    end

  let make_empty : t = RegExEmpty

  let make_concat
      (r1:t)
      (r2:t)
    : t =
    RegExConcat (r1,r2)

  let make_or
      (r1:t)
      (r2:t)
    : t =
    RegExOr (r1,r2)

  let make_star
      (r:t)
    : t =
    RegExStar r

  let make_var
      (v:Id.t)
    : t =
    RegExVariable v

  let make_var
      (v:Id.t)
    : t =
    RegExVariable v

  let make_plus = make_or

  let make_times = make_concat

  let make_base
      (s:string)
    : t =
    RegExBase s

  let fold_downward_upward
      ~init:(init:'b)
      ~upward_empty:(upward_empty:'b -> 'a)
      ~upward_base:(upward_base:'b -> string -> 'a)
      ~upward_concat:(upward_concat:'b -> 'a -> 'a -> 'a)
      ~upward_or:(upward_or:'b -> 'a -> 'a -> 'a)
      ~upward_star:(upward_star:'b -> 'a -> 'a)
      ~upward_var:(upward_var:'b -> Id.t -> 'a)
      ?downward_concat:(downward_concat:'b -> 'b = ident)
      ?downward_or:(downward_or:'b -> 'b = ident)
      ?downward_star:(downward_star:'b -> 'b = ident)
    : t -> 'a =
    let rec fold_downward_upward_internal
        (downward_acc:'b)
        (r:t)
      : 'a =
      begin match r with
        | RegExEmpty -> upward_empty downward_acc
        | RegExBase s -> upward_base downward_acc s
        | RegExConcat (r1,r2) ->
          let downward_acc' = downward_concat downward_acc in
          upward_concat
            downward_acc
            (fold_downward_upward_internal downward_acc' r1)
            (fold_downward_upward_internal downward_acc' r2)
        | RegExOr (r1,r2) ->
          let downward_acc' = downward_or downward_acc in
          upward_or
            downward_acc
            (fold_downward_upward_internal downward_acc' r1)
            (fold_downward_upward_internal downward_acc' r2)
        | RegExStar r' ->
          let downward_acc' = downward_star downward_acc in
          upward_star
            downward_acc
            (fold_downward_upward_internal downward_acc' r')
        | RegExVariable v ->
          upward_var downward_acc v
      end
    in
    fold_downward_upward_internal init

  let fold
      ~empty_f:(empty_f:'a)
      ~base_f:(base_f:string -> 'a)
      ~concat_f:(concat_f:'a -> 'a -> 'a)
      ~or_f:(or_f:'a -> 'a -> 'a)
      ~star_f:(star_f:'a -> 'a)
      ~var_f:(var_f:Id.t -> 'a)
      (r:t)
    : 'a =
    fold_downward_upward
      ~init:()
      ~upward_empty:(thunk_of empty_f)
      ~upward_base:(thunk_of base_f)
      ~upward_concat:(thunk_of concat_f)
      ~upward_or:(thunk_of or_f)
      ~upward_star:(thunk_of star_f)
      ~upward_var:(thunk_of var_f)
      r

  let rec apply_at_every_level
      (f:t -> t)
      (r:t)
    : t =
    fold
      ~empty_f:(f RegExEmpty)
      ~base_f:(fun s -> f (RegExBase s))
      ~concat_f:(fun r1 r2 -> f (RegExConcat (r1,r2)))
      ~or_f:(fun r1 r2 -> f (RegExOr (r1,r2)))
      ~star_f:(fun r' -> f (RegExStar r'))
      ~var_f:(fun v -> f (RegExVariable v))
      r

  let rec applies_for_every_applicable_level
      (f:t -> t option)
    : t -> t list =
    snd
    %
    fold
      ~empty_f:(
        let empty_r = RegExEmpty in
        let level_contribution = option_to_empty_or_singleton (f empty_r) in
        (empty_r, level_contribution))
      ~base_f:(fun s ->
          let base_r = RegExBase s in
          let level_contribution = option_to_empty_or_singleton (f base_r) in
          (base_r, level_contribution))
      ~concat_f:(fun (r1,r1s) (r2,r2s) ->
          let concat_r = RegExConcat (r1,r2) in
          let level_contribution = option_to_empty_or_singleton (f concat_r) in
          let recursed_lefts =
            List.map
              ~f:(fun r1' -> RegExConcat (r1',r2))
              r1s
          in
          let recursed_rights =
            List.map
              ~f:(fun r2' -> RegExConcat (r1,r2'))
              r2s
          in
          (concat_r, level_contribution@recursed_lefts@recursed_rights))
      ~or_f:(fun (r1,r1s) (r2,r2s) ->
          let or_r = RegExOr (r1,r2) in
          let level_contribution = option_to_empty_or_singleton (f or_r) in
          let recursed_lefts =
            List.map
              ~f:(fun r1' -> RegExOr (r1',r2))
              r1s
          in
          let recursed_rights =
            List.map
              ~f:(fun r2' -> RegExOr (r1,r2'))
              r2s
          in
          (or_r, level_contribution@recursed_lefts@recursed_rights))
      ~star_f:(fun (r',r's) ->
          let star_r = RegExStar r' in
          let level_contribution = option_to_empty_or_singleton (f star_r) in
          let recursed_inner =
            List.map
              ~f:(fun r'' -> RegExStar r'')
              r's
          in
          (star_r, level_contribution@recursed_inner))
      ~var_f:(fun v ->
          let var_r = RegExVariable v in
          let level_contribution = option_to_empty_or_singleton (f var_r) in
          (var_r, level_contribution))

  let rec size
    : t -> int =
    fold
      ~empty_f:1
      ~base_f:(fun _ -> 1)
      ~concat_f:(fun n1 n2 -> 1+n1+n2)
      ~or_f:(fun n1 n2 -> 1+n1+n2)
      ~star_f:(fun n -> 1+n)
      ~var_f:(fun _ -> 1)



  let from_char_set (l : (int * int) list) : t =
	  let charOf (index : int) : char =
		  match Char.of_int index with
		  | None -> failwith "Bad Index Bro"
		  | Some c -> c
	  in let helper ((m, n) : int * int) : t =
		     let rec innerHelper (i : int) (r : t) : t =
			     if i > n then r else
				     innerHelper (i + 1) (RegExOr(r, RegExBase (Char.escaped (charOf i))))
		     in if n < m then failwith "Malformed Character Set" else
		     if n = m then RegExBase (Char.escaped (charOf m)) else
			     innerHelper (m + 1) (RegExBase (Char.escaped (charOf m)))
	  in
	  List.fold_left l ~init: RegExEmpty
		  ~f: (fun r x -> if r = RegExEmpty then helper x else RegExOr (r, (helper x)))

  let iterate_n_times (n : int) (r : t) : t =
	  let rec helper (index : int) (temp : t) : t =
		  if index > n then temp else helper (index + 1) (RegExConcat(r, temp)) in
	  if n < 0 then RegExEmpty else if n = 0 then RegExBase "" else helper 2 r

  let iterate_m_to_n_times (m : int) (n : int) (r : t) : t =
	  let rec helper (index : int) (temp : t) : t =
		  if index > n then temp else
			  helper (index + 1) (RegExOr(temp, iterate_n_times index r)) in
	  if n < m then RegExEmpty else helper (m + 1) (iterate_n_times m r)

end

let regex_semiring = (module Regex : Semiring.Sig with type t = Regex.t)
let regex_star_semiring = (module Regex : StarSemiring.Sig with type t = Regex.t)
(***** }}} *****)




(**** Lens {{{ *****)

module Lens =
struct
  type t =
    | LensConst of string * string
    | LensConcat of t * t
    | LensSwap of t * t
    | LensUnion of t * t
    | LensCompose of t * t
    | LensIterate of t
    | LensIdentity of Regex.t
    | LensInverse of t
    | LensVariable of Id.t
    | LensPermute of (int list) (*Permutation.t*) * (t list)
  [@@deriving ord, show, hash]


  let one = LensIdentity (Regex.one)

  let zero = LensIdentity (Regex.zero)

  let separate_plus (l:t) : (t * t) option =
    begin match l with
      | LensUnion (l1,l2) -> Some (l1,l2)
      | _ -> None
    end

  let separate_times (l:t) : (t * t) option =
    begin match l with
      | LensConcat (l1,l2) -> Some (l1,l2)
      | _ -> None
    end

  let separate_star (l:t) : t option =
    begin match l with
      | LensIterate l' -> Some l'
      | _ -> None
    end

  let make_plus (l1:t) (l2:t) : t =
    LensUnion (l1,l2)

  let make_times (l1:t) (l2:t) : t =
    LensConcat (l1,l2)

  let make_star (l:t) : t =
    LensIterate l

  let rec size (l:t) : int =
    begin match l with
      | LensConst _ -> 1
      | LensConcat (l1,l2) ->
        1 + (size l1) + (size l2)
      | LensCompose (l1,l2) ->
        1 + (size l1) + (size l2)
      | LensSwap (l1,l2) ->
        1 + (size l1) + (size l2)
      | LensUnion (l1,l2) ->
        1 + (size l1) + (size l2)
      | LensIterate (l') ->
        1 + (size l')
      | LensIdentity _ -> 1
      | LensInverse l' ->
        1 + (size l')
      | LensVariable _ -> 1
      | LensPermute (_,ls) ->
        1 + (List.fold_left
               ~f:(fun acc l' -> acc + (size l'))
               ~init:0
               ls)
    end

  let rec is_sublens (sublens:t) (suplens:t) : bool =
    if sublens = suplens then
      true
    else
      begin match suplens with
        | LensConcat (l1,l2) ->
          (is_sublens sublens l1) || (is_sublens sublens l2)
        | LensSwap (l1,l2) ->
          (is_sublens sublens l1) || (is_sublens sublens l2)
        | LensUnion (l1,l2) ->
          (is_sublens sublens l1) || (is_sublens sublens l2)
        | LensCompose (l1,l2) ->
          (is_sublens sublens l1) || (is_sublens sublens l2)
        | LensIterate l' ->
          is_sublens sublens l'
        | LensInverse l' ->
          is_sublens sublens l'
        | _ -> false
      end

  let rec has_common_sublens (l1:t) (l2:t) : bool =
    begin match l1 with
      | LensConcat (l11,l12) ->
        (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
      | LensSwap (l11,l12) ->
        (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
      | LensUnion (l11,l12) ->
        (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
      | LensCompose (l11,l12) ->
        (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
      | LensIterate l1' ->
        has_common_sublens l1' l2
      | LensInverse l1' ->
        has_common_sublens l1' l2
      | _ -> is_sublens l1 l2
    end

  let rec apply_at_every_level (f:t -> t) (l:t) : t =
    let l =
      begin match l with
        | LensConcat (l1,l2) ->
          LensConcat (apply_at_every_level f l1, apply_at_every_level f l2)
        | LensSwap (l1,l2) ->
          LensSwap (apply_at_every_level f l1, apply_at_every_level f l2)
        | LensUnion (l1,l2) ->
          LensUnion (apply_at_every_level f l1, apply_at_every_level f l2)
        | LensCompose (l1,l2) ->
          LensCompose (apply_at_every_level f l1, apply_at_every_level f l2)
        | LensIterate (l') ->
          LensIterate (apply_at_every_level f l')
        | LensInverse (l') ->
          LensInverse (apply_at_every_level f l')
        | _ -> l
      end
    in
    f l

  let rec applies_for_every_applicable_level
      (_:t -> t option)
      (_:t)
    : t list =
    failwith "TODO"
end

let lens_semiring = (module Lens : Semiring.Sig with type t = Lens.t)
let lens_star_semiring = (module Lens : StarSemiring.Sig with type t = Lens.t)

(***** }}} *****)



(**** Language {{{ *****)

type examples = (string * string) list

type specification = (Id.t * Regex.t * Regex.t * (string * string) list)

type declaration =
  | DeclRegexCreation of (Id.t * Regex.t * bool)
  | DeclTestString of (Regex.t * string)
  | DeclSynthesizeLens of specification
  | DeclLensCreation of Id.t * Regex.t * Regex.t * Lens.t
  | DeclTestLens of Id.t * examples

type program = declaration list

type synth_problems = (Id.t * Regex.t * bool) list * (specification list) 

(***** }}} *****)


