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
    | RegExClosed of t
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

  let separate_closed
      (r:t)
    : t option =
    begin match r with
      | RegExClosed r -> Some r
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

  let make_closed
      (r:t)
    : t =
    RegExClosed r

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
      ~upward_closed:(upward_closed:'b -> 'a -> 'a)
      ?downward_concat:(downward_concat:'b -> 'b = ident)
      ?downward_or:(downward_or:'b -> 'b = ident)
      ?downward_star:(downward_star:'b -> 'b = ident)
      ?downward_closed:(downward_closed:'b -> 'b = ident)
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
        | RegExClosed r' ->
          let downward_acc' = downward_closed downward_acc in
          upward_closed
            downward_acc
            (fold_downward_upward_internal downward_acc' r')
      end
    in
    fold_downward_upward_internal init

  let fold
      ~empty_f:(empty_f:'a)
      ~base_f:(base_f:string -> 'a)
      ~concat_f:(concat_f:'a -> 'a -> 'a)
      ~or_f:(or_f:'a -> 'a -> 'a)
      ~star_f:(star_f:'a -> 'a)
      ~closed_f:(closed_f:'a -> 'a)
      (r:t)
    : 'a =
    fold_downward_upward
      ~init:()
      ~upward_empty:(thunk_of empty_f)
      ~upward_base:(thunk_of base_f)
      ~upward_concat:(thunk_of concat_f)
      ~upward_or:(thunk_of or_f)
      ~upward_star:(thunk_of star_f)
      ~upward_closed:(thunk_of closed_f)
      r

  let fold_with_subcomponents
      ~empty_f:(empty_f:'a)
      ~base_f:(base_f:string -> 'a)
      ~concat_f:(concat_f:t -> t -> 'a -> 'a -> 'a)
      ~or_f:(or_f:t -> t -> 'a -> 'a -> 'a)
      ~star_f:(star_f:t -> 'a -> 'a)
      ~closed_f:(closed_f:t -> 'a -> 'a)
      (r:t)
    : 'a =
    snd
      (fold
         ~empty_f:(RegExEmpty,empty_f)
         ~base_f:(fun s -> (RegExBase s, base_f s))
         ~concat_f:(fun (r1,x1) (r2,x2) ->
             (RegExConcat (r1,r2), concat_f r1 r2 x1 x2))
         ~or_f:(fun (r1,x1) (r2,x2) ->
             (RegExOr (r1,r2), or_f r1 r2 x1 x2))
         ~star_f:(fun (r',x') ->
             (RegExStar r', star_f r' x'))
         ~closed_f:(fun (r',x') ->
             (RegExClosed r', closed_f r' x'))
         r)


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
      ~closed_f:(fun r' -> f (RegExClosed r'))
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
      ~closed_f:(fun (r',r's) ->
          let closed_r = RegExClosed r' in
          let level_contribution = option_to_empty_or_singleton (f closed_r) in
          let recursed_inner =
            List.map
              ~f:(fun r'' -> RegExClosed r'')
              r's
          in
          (closed_r, level_contribution@recursed_inner))

  let rec size
    : t -> int =
    fold
      ~empty_f:1
      ~base_f:(fun _ -> 1)
      ~concat_f:(fun n1 n2 -> 1+n1+n2)
      ~or_f:(fun n1 n2 -> 1+n1+n2)
      ~star_f:(fun n -> 1+n)
      ~closed_f:(fun n -> n+1)


  let from_char_set (l : (int * int) list) : t =
	  let charOf (index : int) : char =
		  match Char.of_int index with
		  | None -> failwith "Bad Index Bro"
		  | Some c -> c
	  in let helper ((m, n) : int * int) : t =
		     let rec innerHelper (i : int) (r : t) : t =
			     if i > n then r else
				     innerHelper (i + 1) (RegExOr(r, RegExBase (String.of_char (charOf i))))
		     in if n < m then failwith "Malformed Character Set" else
		     if n = m then RegExBase (String.of_char (charOf m)) else
			     innerHelper (m + 1) (RegExBase (String.of_char (charOf m)))
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

  let rec is_empty
      (r:t)
    : bool =
    begin match r with
      | RegExConcat (r1,r2) ->
        is_empty r1 || is_empty r2
      | RegExOr (r1,r2) ->
        is_empty r1 && is_empty r2
      | RegExEmpty ->
        true
      | RegExClosed r ->
        is_empty r
      | RegExStar r ->
        false
      | RegExBase _ ->
        false
    end

  let rec is_singleton
      (r:t)
    : bool =
    begin match r with
      | RegExBase _ ->
        true
      | RegExStar r ->
        is_empty r
      | RegExClosed r ->
        is_singleton r
      | RegExEmpty ->
        false
      | RegExOr (r1,r2) ->
        (is_singleton r1 && is_empty r2)
        || (is_singleton r2 && is_empty r1)
      | RegExConcat (r1,r2) ->
        is_singleton r1 && is_singleton r2
    end

  let rec representative
      (r:t)
    : string option =
    begin match r with
      | RegExConcat (r1,r2) ->
        option_bind
          ~f:(fun s ->
              Option.map
                ~f:(fun s' -> s^s')
                (representative r2))
          (representative r1)
      | RegExOr (r1,r2) ->
        begin match representative r1 with
          | Some s -> Some s
          | None -> representative r2
        end
      | RegExEmpty -> None
      | RegExClosed r -> representative r
      | RegExBase s -> Some s
      | RegExStar _ -> Some ""
    end

  let rec representative_exn
      (r:t)
    : string =
    Option.value_exn (representative r)
end

let regex_semiring = (module Regex : Semiring.Sig with type t = Regex.t)
let regex_star_semiring = (module Regex : StarSemiring.Sig with type t = Regex.t)
(***** }}} *****)




(**** Lens {{{ *****)

module Lens =
struct
  type function_container =
    {
      rr:Regex.t;
      rl:Regex.t;
      creater:string -> string;
      createl:string -> string;
      putr:string -> string -> string;
      putl:string -> string -> string;
    }
  [@@deriving show]

  type t =
    | Disconnect of Regex.t * Regex.t * string * string
    | Concat of t * t
    | Swap of t * t
    | Union of t * t
    | Compose of t * t
    | Iterate of t
    | Identity of Regex.t
    | Inverse of t
    | Closed of int * function_container
    | Permute of Permutation.t * (t list)
  [@@deriving show]

  let get_left_right_regex_closed
      (fc:function_container)
    : Regex.t * Regex.t =
    (fc.rl,fc.rr)

  let get_left_create_closed
      (fc:function_container)
    : string -> string =
    fc.createl

  let get_right_create_closed
      (fc:function_container)
    : string -> string =
    fc.creater

  let hash_fold_t
      _
      _
    : Base__Hash.state = failwith "no hashing on lenses"

  let hash
      _
    : int = failwith "no hashing on lenses"

  let rec is_eq
      (l:t)
      (m:t)
    : bool =
    begin match (l,m) with
      | (Disconnect (r1,r2,s1,s2),Disconnect (t1,t2,u1,u2)) ->
        is_equal (Regex.compare r1 t1)
        && is_equal (Regex.compare r2 t2)
        && is_equal (String.compare s1 u1)
        && is_equal (String.compare s2 u2)
      | (Concat (l1,l2),Concat (m1,m2)) ->
        is_eq l1 m1
        && is_eq l2 m2
      | (Swap (l1,l2),Swap (m1,m2)) ->
        is_eq l1 m1
        && is_eq l2 m2
      | (Union (l1,l2),Union (m1,m2)) ->
        is_eq l1 m1
        && is_eq l2 m2
      | (Compose (l1,l2),Compose (m1,m2)) ->
        is_eq l1 m1
        && is_eq l2 m2
      | (Iterate l',Iterate m') ->
        is_eq l' m'
      | (Identity r,Identity t) ->
        is_equal (Regex.compare r t)
      | (Inverse l',Inverse m') ->
        is_eq l' m'
      | (Closed (i,l'),Closed (j,m')) ->
        is_equal (Int.compare i j)
      | (Permute (p,ls),Permute (s,ms)) ->
        is_equal (Permutation.compare p s)
        && (begin match List.zip ls ms with
            | None -> false
            | Some lms ->
              List.for_all
                ~f:(uncurry is_eq)
                lms
          end)
      | _ -> false
    end

  let compare
      _
      _
    : int = failwith "no compare lenses"

  let one = Identity (Regex.one)

  let zero = Identity (Regex.zero)

  let separate_plus (l:t) : (t * t) option =
    begin match l with
      | Union (l1,l2) -> Some (l1,l2)
      | _ -> None
    end

  let separate_times (l:t) : (t * t) option =
    begin match l with
      | Concat (l1,l2) -> Some (l1,l2)
      | _ -> None
    end

  let separate_star (l:t) : t option =
    begin match l with
      | Iterate l' -> Some l'
      | _ -> None
    end

  let make_or (l1:t) (l2:t) : t =
    Union (l1,l2)

  let make_plus : t -> t -> t = make_or

  let make_times (l1:t) (l2:t) : t =
    Concat (l1,l2)

  let make_concat : t -> t -> t = make_times

  let make_star (l:t) : t =
    Iterate l

  let make_swap
      (r1:t)
      (r2:t)
    : t =
    Swap (r1,r2)

  let make_ident (r:Regex.t) : t =
    Identity r

  let make_disconnect
      (r1:Regex.t)
      (r2:Regex.t)
      (s1:string)
      (s2:string)
    : t =
    Disconnect (r1,r2,s1,s2)

  let make_compose
      (l1:t)
      (l2:t)
    : t =
    Compose (l1,l2)

  let make_const
      (s1:string)
      (s2:string)
    : t =
    make_disconnect
      (Regex.make_base s1)
      (Regex.make_base s2)
      s1
      s2

  let make_permute
      (p:Permutation.t)
      (ls:t list)
    : t =
    Permute (p,ls)

  let make_closed
      ~rr:rr
      ~rl:rl
      ~creater:creater
      ~createl:createl
      ~putr:putr
      ~putl:putl
      (i:int)
    : t =
    let fc =
      {
        rr = rr;
        rl = rl;
        creater = creater;
        createl = createl;
        putr = putr;
        putl = putl;
      }
    in
    Closed (i,fc)

  let rec size (l:t) : int =
    begin match l with
      | Disconnect (r1,r2,_,_) ->
        1 + (Regex.size r1) + (Regex.size r2)
      | Concat (l1,l2) ->
        1 + (size l1) + (size l2)
      | Compose (l1,l2) ->
        1 + (size l1) + (size l2)
      | Swap (l1,l2) ->
        1 + (size l1) + (size l2)
      | Union (l1,l2) ->
        1 + (size l1) + (size l2)
      | Iterate (l') ->
        1 + (size l')
      | Identity _ -> 1
      | Inverse l' ->
        1 + (size l')
      | Closed _ -> 1
      | Permute (_,ls) ->
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
        | Concat (l1,l2) ->
          (is_sublens sublens l1) || (is_sublens sublens l2)
        | Swap (l1,l2) ->
          (is_sublens sublens l1) || (is_sublens sublens l2)
        | Union (l1,l2) ->
          (is_sublens sublens l1) || (is_sublens sublens l2)
        | Compose (l1,l2) ->
          (is_sublens sublens l1) || (is_sublens sublens l2)
        | Iterate l' ->
          is_sublens sublens l'
        | Inverse l' ->
          is_sublens sublens l'
        | _ -> false
      end

  let rec has_common_sublens (l1:t) (l2:t) : bool =
    begin match l1 with
      | Concat (l11,l12) ->
        (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
      | Swap (l11,l12) ->
        (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
      | Union (l11,l12) ->
        (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
      | Compose (l11,l12) ->
        (has_common_sublens l11 l2) || (has_common_sublens l12 l2)
      | Iterate l1' ->
        has_common_sublens l1' l2
      | Inverse l1' ->
        has_common_sublens l1' l2
      | _ -> is_sublens l1 l2
    end

  let rec apply_at_every_level (f:t -> t) (l:t) : t =
    let l =
      begin match l with
        | Concat (l1,l2) ->
          Concat (apply_at_every_level f l1, apply_at_every_level f l2)
        | Swap (l1,l2) ->
          Swap (apply_at_every_level f l1, apply_at_every_level f l2)
        | Union (l1,l2) ->
          Union (apply_at_every_level f l1, apply_at_every_level f l2)
        | Compose (l1,l2) ->
          Compose (apply_at_every_level f l1, apply_at_every_level f l2)
        | Iterate (l') ->
          Iterate (apply_at_every_level f l')
        | Inverse (l') ->
          Inverse (apply_at_every_level f l')
        | _ -> l
      end
    in
    f l

  let rec applies_for_every_applicable_level
      (_:t -> t option)
      (_:t)
    : t list =
    failwith "TODO"

  let fold
      (type a)
      ~disc_f:(disc_f:
               Regex.t ->
               Regex.t ->
               string ->
               string ->
               a)
      ~concat_f:(concat_f:a -> a -> a)
      ~swap_f:(swap_f:a -> a -> a)
      ~union_f:(union_f:a -> a -> a)
      ~compose_f:(compose_f:a -> a -> a)
      ~iterate_f:(iterate_f:a -> a)
      ~identity_f:(identity_f:Regex.t -> a)
      ~inverse_f:(inverse_f:a -> a)
      ~permute_f:(permute_f:Permutation.t -> a list -> a)
      ~closed_f:(closed_f:int -> function_container -> a)
      (l:t)
    : a =
    let rec fold_internal
        (l:t)
      : a =
      begin match l with
        | Disconnect(r1,r2,s1,s2) -> disc_f r1 r2 s1 s2
        | Concat(l1,l2) ->
          let acc1 = fold_internal l1 in
          let acc2 = fold_internal l2 in
          concat_f acc1 acc2
        | Swap(l1,l2) ->
          let acc1 = fold_internal l1 in
          let acc2 = fold_internal l2 in
          swap_f acc1 acc2
        | Union(l1,l2) ->
          let acc1 = fold_internal l1 in
          let acc2 = fold_internal l2 in
          union_f acc1 acc2
        | Compose(l1,l2) ->
          let acc1 = fold_internal l1 in
          let acc2 = fold_internal l2 in
          compose_f acc1 acc2
        | Iterate l -> 
          let acc = fold_internal l in
          iterate_f acc
        | Identity r ->
          identity_f r
        | Inverse l ->
          let acc = fold_internal l in
          inverse_f acc
        | Permute (p,ls) ->
          let accs = List.map ~f:fold_internal ls in
          permute_f p accs
        | Closed (i,l') ->
          closed_f i l'
      end
    in
    fold_internal l
end

let lens_semiring = (module Lens : Semiring.Sig with type t = Lens.t)
let lens_star_semiring = (module Lens : StarSemiring.Sig with type t = Lens.t)

(***** }}} *****)



(**** Language {{{ *****)

type create_examples = (string * string) list
type put_examples = (string * string * string) list

type specification = (Id.t * Regex.t * Regex.t * (string * string) list)

type declaration =
  | DeclRegexCreation of (Id.t * Regex.t * bool)
  | DeclTestString of (Regex.t * string)
  | DeclSynthesizeLens of specification
  | DeclLensCreation of Id.t * Regex.t * Regex.t * Lens.t
  | DeclTestLens of Id.t * create_examples

type program = declaration list

type synth_problems = (Id.t * Regex.t * bool) list * (specification list) 

(***** }}} *****)


