open MyStdlib
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
  type t = t_node hash_consed
  and t_node =
    | RegExEmpty
    | RegExBase of string
    | RegExConcat of t * t
    | RegExOr of t * t
    | RegExStar of t
    | RegExClosed of t
    | RegExSkip of t
    | RegExRequire of t
  [@@deriving ord, show, hash]

  let table = HashConsTable.create 100000
  let hashcons = HashConsTable.hashcons hash_t_node compare_t_node table

  let uid (r:t) = r.tag

  let rec show_with_closed
      (r:t)
    : string =
    begin match r.node with
      | RegExEmpty -> "{}"
      | RegExBase s -> "\"" ^ s ^ "\""
      | RegExConcat (r1,r2) -> show_with_closed r1 ^ "." ^ show_with_closed r2
      | RegExOr (r1,r2) -> show_with_closed r1 ^ "|" ^ show_with_closed r2
      | RegExStar (r1) -> show_with_closed r1 ^ "*"
      | RegExSkip r1 -> "skip(" ^ show_with_closed r1 ^ ")"
      | RegExRequire r1 -> "require(" ^ show_with_closed r1 ^ ")"
      | RegExClosed _ -> "closed"
    end

  let separate_plus
      (r:t)
    : (t * t) option =
    begin match r.node with
      | RegExOr (r1,r2) -> Some (r1,r2)
      | _ -> None
    end

  let separate_times
      (r:t)
    : (t * t) option =
    begin match r.node with
      | RegExConcat (r1,r2) -> Some (r1,r2)
      | _ -> None
    end

  let separate_star
      (r:t)
    : t option =
    begin match r.node with
      | RegExStar r' -> Some r'
      | _ -> None
    end

  let separate_closed
      (r:t)
    : t option =
    begin match r.node with
      | RegExClosed r -> Some r
      | _ -> None
    end

  let empty : t = hashcons RegExEmpty

  let make_concat
      (r1:t)
      (r2:t)
    : t =
    hashcons (RegExConcat (r1,r2))

  let make_or
      (r1:t)
      (r2:t)
    : t =
    hashcons (RegExOr (r1,r2))

  let make_star
      (r:t)
    : t =
    hashcons (RegExStar r)

  let make_skip
      (r:t)
    : t =
    hashcons (RegExSkip r)

  let make_require
      (r:t)
    : t =
    hashcons (RegExRequire r)

  let make_closed
      (r:t)
    : t =
    hashcons (RegExClosed r)

  let make_plus = make_or

  let make_times = make_concat

  let make_base
      (s:string)
    : t =
    hashcons (RegExBase s)

  let one = make_base ""

  let zero = empty

  let fold_downward_upward
      ~init:(init:'b)
      ~upward_empty:(upward_empty:'b -> 'a)
      ~upward_base:(upward_base:'b -> string -> 'a)
      ~upward_concat:(upward_concat:'b -> 'a -> 'a -> 'a)
      ~upward_or:(upward_or:'b -> 'a -> 'a -> 'a)
      ~upward_star:(upward_star:'b -> 'a -> 'a)
      ~upward_closed:(upward_closed:'b -> 'a -> 'a)
      ~upward_skip:(upward_skip:'b -> 'a -> 'a)
      ~upward_require:(upward_require:'b -> 'a -> 'a)
      ?downward_concat:(downward_concat:'b -> 'b = ident)
      ?downward_or:(downward_or:'b -> 'b = ident)
      ?downward_star:(downward_star:'b -> 'b = ident)
      ?downward_closed:(downward_closed:'b -> 'b = ident)
      ?downward_skip:(downward_skip:'b -> 'b = ident)
      ?downward_require:(downward_require:'b -> 'b = ident)
    : t -> 'a =
    let rec fold_downward_upward_internal
        (downward_acc:'b)
        (r:t)
      : 'a =
      begin match r.node with
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
        | RegExSkip r' ->
          let downward_acc' = downward_skip downward_acc in
          upward_skip
            downward_acc
            (fold_downward_upward_internal downward_acc' r')
        | RegExRequire r' ->
          let downward_acc' = downward_require downward_acc in
          upward_require
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
      ~skip_f:(skip_f:'a -> 'a)
      ~require_f:(require_f:'a -> 'a)
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
      ~upward_skip:(thunk_of skip_f)
      ~upward_require:(thunk_of require_f)
      r

  let fold_with_subcomponents
      ~empty_f:(empty_f:'a)
      ~base_f:(base_f:string -> 'a)
      ~concat_f:(concat_f:t -> t -> 'a -> 'a -> 'a)
      ~or_f:(or_f:t -> t -> 'a -> 'a -> 'a)
      ~star_f:(star_f:t -> 'a -> 'a)
      ~closed_f:(closed_f:t -> 'a -> 'a)
      ~skip_f:(skip_f:t -> 'a -> 'a)
      ~require_f:(require_f:t -> 'a -> 'a)
      (r:t)
    : 'a =
    snd
      (fold
         ~empty_f:(empty,empty_f)
         ~base_f:(fun s -> (make_base s, base_f s))
         ~concat_f:(fun (r1,x1) (r2,x2) ->
             (make_concat r1 r2, concat_f r1 r2 x1 x2))
         ~or_f:(fun (r1,x1) (r2,x2) ->
             (make_or r1 r2, or_f r1 r2 x1 x2))
         ~star_f:(fun (r',x') ->
             (make_star r', star_f r' x'))
         ~closed_f:(fun (r',x') ->
             (make_closed r', closed_f r' x'))
         ~skip_f:(fun (r',x') ->
             (make_skip r', skip_f r' x'))
         ~require_f:(fun (r',x') ->
             (make_require r', require_f r' x'))
         r)


  let rec apply_at_every_level
      (f:t -> t)
      (r:t)
    : t =
    fold
      ~empty_f:(f empty)
      ~base_f:(fun s -> f (make_base s))
      ~concat_f:(fun r1 r2 -> f (make_concat r1 r2))
      ~or_f:(fun r1 r2 -> f (make_or r1 r2))
      ~star_f:(fun r' -> f (make_star r'))
      ~closed_f:(fun r' -> f (make_closed r'))
      ~skip_f:(fun r' -> f (make_skip r'))
      ~require_f:(fun r' -> f (make_require r'))
      r

  let rec applies_for_every_applicable_level
      (f:t -> t option)
    : t -> t list =
    snd
    %
    fold
      ~empty_f:(
        let empty_r = empty in
        let level_contribution = option_to_empty_or_singleton (f empty_r) in
        (empty_r, level_contribution))
      ~base_f:(fun s ->
          let base_r = make_base s in
          let level_contribution = option_to_empty_or_singleton (f base_r) in
          (base_r, level_contribution))
      ~concat_f:(fun (r1,r1s) (r2,r2s) ->
          let concat_r = make_concat r1 r2 in
          let level_contribution = option_to_empty_or_singleton (f concat_r) in
          let recursed_lefts =
            List.map
              ~f:(fun r1' -> make_concat r1' r2)
              r1s
          in
          let recursed_rights =
            List.map
              ~f:(fun r2' -> make_concat r1 r2')
              r2s
          in
          (concat_r, level_contribution@recursed_lefts@recursed_rights))
      ~or_f:(fun (r1,r1s) (r2,r2s) ->
          let or_r = make_or r1 r2 in
          let level_contribution = option_to_empty_or_singleton (f or_r) in
          let recursed_lefts =
            List.map
              ~f:(fun r1' -> make_or r1' r2)
              r1s
          in
          let recursed_rights =
            List.map
              ~f:(fun r2' -> make_or r1 r2')
              r2s
          in
          (or_r, level_contribution@recursed_lefts@recursed_rights))
      ~star_f:(fun (r',r's) ->
          let star_r = make_star r' in
          let level_contribution = option_to_empty_or_singleton (f star_r) in
          let recursed_inner =
            List.map
              ~f:(fun r'' -> make_star r'')
              r's
          in
          (star_r, level_contribution@recursed_inner))
      ~closed_f:(fun (r',r's) ->
          let closed_r = make_closed r' in
          let level_contribution = option_to_empty_or_singleton (f closed_r) in
          let recursed_inner =
            List.map
              ~f:(fun r'' -> make_closed r'')
              r's
          in
          (closed_r, level_contribution@recursed_inner))
      ~skip_f:(fun (r',r's) ->
          let skip_r = make_skip r' in
          let level_contribution = option_to_empty_or_singleton (f skip_r) in
          let recursed_inner =
            List.map
              ~f:(fun r'' -> make_skip r'')
              r's
          in
          (skip_r, level_contribution@recursed_inner))
      ~require_f:(fun (r',r's) ->
          let require_r = make_require r' in
          let level_contribution = option_to_empty_or_singleton (f require_r) in
          let recursed_inner =
            List.map
              ~f:(fun r'' -> make_require r'')
              r's
          in
          (require_r, level_contribution@recursed_inner))

  let rec size
    : t -> int =
    fold
      ~empty_f:1
      ~base_f:(fun _ -> 1)
      ~concat_f:(fun n1 n2 -> 1+n1+n2)
      ~or_f:(fun n1 n2 -> 1+n1+n2)
      ~star_f:(fun n -> 1+n)
      ~closed_f:(fun n -> n+1)
      ~skip_f:(fun n -> n+1)
      ~require_f:(fun n -> n+1)

  let contains_skip
    : t -> bool =
    fold
      ~empty_f:false
      ~base_f:(fun _ -> false)
      ~concat_f:(||)
      ~or_f:(||)
      ~star_f:ident
      ~closed_f:ident
      ~skip_f:(fun _ -> true)
      ~require_f:ident

  let from_char_set (l : (int * int) list) : t =
	  let charOf (index : int) : char =
		  match Char.of_int index with
		  | None -> failwith "Bad Index Bro"
		  | Some c -> c
	  in let helper ((m, n) : int * int) : t =
		     let rec innerHelper (i : int) (r : t) : t =
			     if i > n then r else
				     innerHelper (i + 1) (make_or r (make_base (String.of_char (charOf i))))
		     in if n < m then failwith "Malformed Character Set" else
		     if n = m then make_base (String.of_char (charOf m)) else
			     innerHelper (m + 1) (make_base (String.of_char (charOf m)))
	  in
	  List.fold_left l ~init:empty
		  ~f: (fun r x -> if r = empty then helper x else make_or r (helper x))

  let iterate_n_times (n : int) (r : t) : t =
	  let rec helper (index : int) (temp : t) : t =
		  if index > n then temp else helper (index + 1) (make_concat r temp) in
	  if n < 0 then empty else if n = 0 then one else helper 2 r

  let iterate_m_to_n_times (m : int) (n : int) (r : t) : t =
	  let rec helper (index : int) (temp : t) : t =
		  if index > n then temp else
			  helper (index + 1) (make_or temp (iterate_n_times index r)) in
	 if n < m then empty else helper (m + 1) (iterate_n_times m r)

  let rec is_empty
      (r:t)
    : bool =
    begin match r.node with
      | RegExConcat (r1,r2) ->
        is_empty r1 || is_empty r2
      | RegExOr (r1,r2) ->
        is_empty r1 && is_empty r2
      | RegExEmpty ->
        true
      | RegExClosed r ->
        is_empty r
      | RegExSkip r ->
        is_empty r
      | RegExStar r ->
        false
      | RegExBase _ ->
        false
      | RegExRequire r ->
        is_empty r
    end

  let rec is_singleton
      (r:t)
    : bool =
    begin match r.node with
      | RegExBase _ ->
        true
      | RegExStar r ->
        is_empty r
      | RegExClosed r ->
        is_singleton r
      | RegExSkip r ->
        is_singleton r
      | RegExRequire r ->
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
    begin match r.node with
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
      | RegExSkip r -> representative r
      | RegExRequire r -> representative r
      | RegExBase s -> Some s
      | RegExStar r -> Some ""
    end

  let rec representative_exn
      (r:t)
    : string =
    Option.value_exn (representative r)

  (*let likelihood_star : float = 0.8
  let info_content_star_multiplier : float =
    likelihood_star /. (1. -. likelihood_star)
  let info_content_star_const : float =
    -4. *. (Math.log2 likelihood_star)
    -. (Math.log2 (1. -. likelihood_star))
  let information_content
      (r:t)
    : float =
    let include_choice_info
        ((f,n):float * int)
      : float =
      f +. (Math.log2 (Float.of_int n))
    in
    let rec information_content_internal
        (r:t)
      : float * int =
      begin match r.node with
        | RegExOr(r1,r2) ->
          let (ic1,n1) = information_content_internal r1 in
          let (ic2,n2) = information_content_internal r2 in
          let n1f = Float.of_int n1 in
          let n2f = Float.of_int n2 in
          (((ic1 *. n1f) +. (ic2 *. n2f)) /. (n1f +. n2f)
          ,n1 + n2)
        | RegExConcat (r1,r2) ->
          let (ic1,n1) = information_content_internal r1 in
          let (ic2,n2) = information_content_internal r2 in
          (ic1 +. ic2,n1*n2)
        | RegExBase _ -> (0.,1)
        | RegExEmpty -> (0.,0)
        | RegExClosed r -> information_content_internal r
        | RegExSkip r -> information_content_internal r
        | RegExRequire r -> information_content_internal r
        | RegExStar r ->
          let base = include_choice_info (information_content_internal r) in
          ((info_content_star_multiplier *. base) +. info_content_star_const,1)
      end
    in
    include_choice_info (information_content_internal r)*)
end

let regex_semiring = (module Regex : Semiring.Sig with type t = Regex.t)
let regex_star_semiring = (module Regex : StarSemiring.Sig with type t = Regex.t)

(**** Regex {{{ *****)
module StochasticRegex =
struct
  type t = l hash_consed
  and l =
    {
      node  : t_node                                     ;
      mutable regex : (Regex.t option) [@hash.ignore] ;
    }
  and t_node =
    | Empty
    | Base of string
    | Concat of t * t
    | Or of t * t * Probability.t
    | Star of t * Probability.t
    | Closed of t
    | Skip of t
    | Require of t
  [@@deriving show,hash]

  let rec compare
      (r1:t)
      (r2:t)
    : int =
    compare_hash_consed
      compare_l
      r1
      r2
  and compare_l
      (l1:l)
      (l2:l)
    : int =
    compare_t_node
      l1.node
      l2.node
  and compare_t_node
      (t1:t_node)
      (t2:t_node)
    : int =
    begin match (t1,t2) with
      | (Empty,Empty) -> 0
      | (Empty,_    ) -> -1
      | (_    ,Empty) -> 1
      | (Base s1,Base s2) -> String.compare s1 s2
      | (Base _ ,_      ) -> -1
      | (_      ,Base _ ) -> 1
      | (Concat (r11,r12),Concat (r21,r22)) ->
        pair_compare
          compare
          compare
          (r11,r12)
          (r21,r22)
      | (Concat _        ,_               ) -> -1
      | (_               ,Concat _        ) -> 1
      | (Or (r11,r12,p1),Or (r21,r22,p2)) ->
        triple_compare
          compare
          compare
          Float.compare
          (r11,r12,p1)
          (r21,r22,p2)
      | (Or _        ,_           ) -> -1
      | (_           ,Or _        ) -> 1
      | (Star (r1,p1),Star (r2,p2)) ->
        pair_compare
          compare
          Float.compare
          (r1,p1)
          (r2,p2)
      | (Star _      ,_           ) -> -1
      | (_           ,Star _      ) -> 1
      | (Closed r1,Closed r2) -> compare r1 r2
      | (Closed r1,_        ) -> -1
      | (_        ,Closed r2) -> 1
      | (Skip r1, Skip r2) -> compare r1 r2
      | (Skip r1,_        ) -> -1
      | (_        ,Skip r2) -> 1
      | (Require r1, Require r2) -> compare r1 r2
    end

  let table = HashConsTable.create 10000
  let mk_l
      (r:t_node)
    : l =
    {
      node  = r    ;
      regex = None ;
    }
  let mk_t = HashConsTable.hashcons hash_l compare_l table % mk_l

  let uid (r:t) = r.tag

  let node (r:t) =
    r.node.node

  let separate_plus
      (r:t)
    : (t * t * Probability.t) option =
    begin match node r with
      | Or (r1,r2,p) -> Some (r1,r2,p)
      | _ -> None
    end

  let separate_times
      (r:t)
    : (t * t) option =
    begin match node r with
      | Concat (r1,r2) -> Some (r1,r2)
      | _ -> None
    end

  let separate_star
      (r:t)
    : (t * Probability.t) option =
    begin match node r with
      | Star (r',p) -> Some (r',p)
      | _ -> None
    end

  let separate_closed
      (r:t)
    : t option =
    begin match node r with
      | Closed r -> Some r
      | _ -> None
    end

  let empty : t = mk_t Empty

  let make_concat
      (r1:t)
      (r2:t)
    : t =
    mk_t (Concat (r1,r2))

  let make_or
      (r1:t)
      (r2:t)
      (p:Probability.t)
    : t =
    mk_t (Or (r1,r2,p))

  let make_star
      (r:t)
      (p:Probability.t)
    : t =
    mk_t (Star (r,p))

  let make_closed
      (r:t)
    : t =
    mk_t (Closed r)

  let make_skip
      (r:t)
    : t =
    mk_t (Skip r)

  let make_require
      (r:t)
    : t =
    mk_t (Require r)

  let make_plus = make_or

  let make_times = make_concat

  let make_base
      (s:string)
    : t =
    mk_t (Base s)

  let one = make_base ""

  let zero = empty

  let fold_downward_upward
      ~init:(init:'b)
      ~upward_empty:(upward_empty:'b -> 'a)
      ~upward_base:(upward_base:'b -> string -> 'a)
      ~upward_concat:(upward_concat:'b -> 'a -> 'a -> 'a)
      ~upward_or:(upward_or:'b -> Probability.t -> 'a -> 'a -> 'a)
      ~upward_star:(upward_star:'b -> Probability.t -> 'a -> 'a)
      ~upward_closed:(upward_closed:'b -> 'a -> 'a)
      ~upward_skip:(upward_skip:'b -> 'a -> 'a)
      ~upward_require:(upward_require:'b -> 'a -> 'a)
      ?downward_concat:(downward_concat:'b -> 'b = ident)
      ?downward_or:(downward_or:'b -> 'b = ident)
      ?downward_star:(downward_star:'b -> 'b = ident)
      ?downward_closed:(downward_closed:'b -> 'b = ident)
      ?downward_skip:(downward_skip:'b -> 'b = ident)
      ?downward_require:(downward_require:'b -> 'b = ident)
    : t -> 'a =
    let rec fold_downward_upward_internal
        (downward_acc:'b)
        (r:t)
      : 'a =
      begin match node r with
        | Empty -> upward_empty downward_acc
        | Base s -> upward_base downward_acc s
        | Concat (r1,r2) ->
          let downward_acc' = downward_concat downward_acc in
          upward_concat
            downward_acc
            (fold_downward_upward_internal downward_acc' r1)
            (fold_downward_upward_internal downward_acc' r2)
        | Or (r1,r2,p) ->
          let downward_acc' = downward_or downward_acc in
          upward_or
            downward_acc
            p
            (fold_downward_upward_internal downward_acc' r1)
            (fold_downward_upward_internal downward_acc' r2)
        | Star (r',p) ->
          let downward_acc' = downward_star downward_acc in
          upward_star
            downward_acc
            p
            (fold_downward_upward_internal downward_acc' r')
        | Closed r' ->
          let downward_acc' = downward_closed downward_acc in
          upward_closed
            downward_acc
            (fold_downward_upward_internal downward_acc' r')
        | Skip r' ->
          let downward_acc' = downward_skip downward_acc in
          upward_skip
            downward_acc
            (fold_downward_upward_internal downward_acc' r')
        | Require r' ->
          let downward_acc' = downward_require downward_acc in
          upward_require
            downward_acc
            (fold_downward_upward_internal downward_acc' r')
      end
    in
    fold_downward_upward_internal init

  let fold
      ~empty_f:(empty_f:'a)
      ~base_f:(base_f:string -> 'a)
      ~concat_f:(concat_f:'a -> 'a -> 'a)
      ~or_f:(or_f:Probability.t -> 'a -> 'a -> 'a)
      ~star_f:(star_f:Probability.t -> 'a -> 'a)
      ~closed_f:(closed_f:'a -> 'a)
      ~skip_f:(skip_f:'a -> 'a)
      ~require_f:(require_f:'a -> 'a)
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
      ~upward_skip:(thunk_of skip_f)
      ~upward_require:(thunk_of require_f)
      r

  let contains_skip
    : t -> bool =
    fold
      ~empty_f:false
      ~base_f:(fun _ -> false)
      ~concat_f:(||)
      ~or_f:(fun _ -> (||))
      ~star_f:(fun _ -> ident)
      ~closed_f:ident
      ~skip_f:(fun _ -> true)
      ~require_f:ident

  let fold_with_subcomponents
      ~empty_f:(empty_f:'a)
      ~base_f:(base_f:string -> 'a)
      ~concat_f:(concat_f:t -> t -> 'a -> 'a -> 'a)
      ~or_f:(or_f:t -> t -> Probability.t -> 'a -> 'a -> 'a)
      ~star_f:(star_f:t -> Probability.t -> 'a -> 'a)
      ~closed_f:(closed_f:t -> 'a -> 'a)
      ~skip_f:(skip_f:t -> 'a -> 'a)
      ~require_f:(require_f:t -> 'a -> 'a)
      (r:t)
    : 'a =
    snd
      (fold
         ~empty_f:(empty,empty_f)
         ~base_f:(fun s -> (make_base s, base_f s))
         ~concat_f:(fun (r1,x1) (r2,x2) ->
             (make_concat r1 r2, concat_f r1 r2 x1 x2))
         ~or_f:(fun p (r1,x1) (r2,x2) ->
             (make_or r1 r2 p, or_f r1 r2 p x1 x2))
         ~star_f:(fun p (r',x') ->
             (make_star r' p, star_f r' p x'))
         ~closed_f:(fun (r',x') ->
             (make_closed r', closed_f r' x'))
         ~skip_f:(fun (r',x') ->
             (make_skip r', skip_f r' x'))
         ~require_f:(fun (r',x') ->
             (make_require r', require_f r' x'))
         r)

  let dnf_size
      (s:t)
    : int =
    let rec dnf_size_internal
        (s:t)
      : int * int =
      begin match (node s) with
        | Empty -> (0,0)
        | Base _ -> (0,1)
        | Concat (r1,r2) ->
          let (size1,or_size1) = dnf_size_internal r1 in
          let (size2,or_size2) = dnf_size_internal r2 in
          (size1*or_size2 + size2*or_size1, or_size1 * or_size2) 
        | Or (r1,r2,_) ->
          let (size1,or_size1) = dnf_size_internal r1 in
          let (size2,or_size2) = dnf_size_internal r2 in
          (size1+size2,or_size1+or_size2)
        | Star (r',_) ->
          let (size,_) = dnf_size_internal r' in
          (size+1,1)
        | Closed _ ->
          (1,1)
        | Skip _ ->
          (1,1)
        | Require r -> dnf_size_internal r
      end
    in
    fst (dnf_size_internal s)

  let rec apply_at_every_level
      (f:t -> t)
      (r:t)
    : t =
    fold
      ~empty_f:(f empty)
      ~base_f:(fun s -> f (make_base s))
      ~concat_f:(fun r1 r2 -> f (make_concat r1 r2))
      ~or_f:(fun p r1 r2 -> f (make_or r1 r2 p))
      ~star_f:(fun p r' -> f (make_star r' p))
      ~closed_f:(fun r' -> f (make_closed r'))
      ~skip_f:(fun r' -> f (make_skip r'))
      ~require_f:(fun r' -> f (make_require r'))
      r

  let rec applies_for_every_applicable_level
      (f:t -> t option)
    : t -> t list =
    snd
    %
    fold
      ~empty_f:(
        let empty_r = empty in
        let level_contribution = option_to_empty_or_singleton (f empty_r) in
        (empty_r, level_contribution))
      ~base_f:(fun s ->
          let base_r = make_base s in
          let level_contribution = option_to_empty_or_singleton (f base_r) in
          (base_r, level_contribution))
      ~concat_f:(fun (r1,r1s) (r2,r2s) ->
          let concat_r = make_concat r1 r2 in
          let level_contribution = option_to_empty_or_singleton (f concat_r) in
          let recursed_lefts =
            List.map
              ~f:(fun r1' -> make_concat r1' r2)
              r1s
          in
          let recursed_rights =
            List.map
              ~f:(fun r2' -> make_concat r1 r2')
              r2s
          in
          (concat_r, level_contribution@recursed_lefts@recursed_rights))
      ~or_f:(fun p (r1,r1s) (r2,r2s) ->
          let or_r = make_or r1 r2 p in
          let level_contribution = option_to_empty_or_singleton (f or_r) in
          let recursed_lefts =
            List.map
              ~f:(fun r1' -> make_or r1' r2 p)
              r1s
          in
          let recursed_rights =
            List.map
              ~f:(fun r2' -> make_or r1 r2' p)
              r2s
          in
          (or_r, level_contribution@recursed_lefts@recursed_rights))
      ~star_f:(fun p (r',r's) ->
          let star_r = make_star r' p in
          let level_contribution = option_to_empty_or_singleton (f star_r) in
          let recursed_inner =
            List.map
              ~f:(fun r'' -> make_star r'' p)
              r's
          in
          (star_r, level_contribution@recursed_inner))
      ~closed_f:(fun (r',r's) ->
          let closed_r = make_closed r' in
          let level_contribution = option_to_empty_or_singleton (f closed_r) in
          let recursed_inner =
            List.map
              ~f:(fun r'' -> make_closed r'')
              r's
          in
          (closed_r, level_contribution@recursed_inner))
      ~skip_f:(fun (r',r's) ->
          let skip_r = make_skip r' in
          let level_contribution = option_to_empty_or_singleton (f skip_r) in
          let recursed_inner =
            List.map
              ~f:(fun r'' -> make_skip r'')
              r's
          in
          (skip_r, level_contribution@recursed_inner))
      ~require_f:(fun (r',r's) ->
          let require_r = make_require r' in
          let level_contribution = option_to_empty_or_singleton (f require_r) in
          let recursed_inner =
            List.map
              ~f:(fun r'' -> make_require r'')
              r's
          in
          (require_r, level_contribution@recursed_inner))

  let rec size
    : t -> int =
    fold
      ~empty_f:1
      ~base_f:(fun _ -> 1)
      ~concat_f:(fun n1 n2 -> 1+n1+n2)
      ~or_f:(fun _ n1 n2 -> 1+n1+n2)
      ~star_f:(fun _ n -> 1+n)
      ~closed_f:(fun n -> n+1)
      ~skip_f:(fun n -> n+1)
      ~require_f:(fun n -> n+1)

  let from_regex
      (r:Regex.t)
    : t =
    snd
      (Regex.fold
         ~empty_f:(0.,empty)
         ~base_f:(fun s -> (1.,make_base s))
         ~concat_f:(fun (i1,r1) (i2,r2) -> (i1 *. i2,make_concat r1 r2))
         ~or_f:(fun (i1,r1) (i2,r2) ->
             let prob = i1 /. (i1 +. i2) in
             let prob =
               if prob = 0. then
                 Float.min_positive_subnormal_value
               else if prob = 1. then
                 1. -. Float.epsilon_float
               else
                 prob
             in
             (i1 +. i2,make_or r1 r2 prob))
         ~star_f:(fun (_,r) -> (1.,make_star r 0.8))
         ~closed_f:(fun (i,r) -> (i,make_closed r))
         ~skip_f:(fun (i,r) -> (i,make_skip r))
         ~require_f:(fun (i,r) -> (i,make_require r))
         r)

  let rec to_regex
    (r:t)
    : Regex.t =
    begin match r.node.regex with
      | None ->
        let ans =
          begin match node r with
            | Empty -> Regex.empty
            | Base s -> Regex.make_base s
            | Concat (r1,r2) ->
              let r1 = to_regex r1 in
              let r2 = to_regex r2 in
              Regex.make_concat r1 r2
            | Or (r1,r2,_) ->
              let r1 = to_regex r1 in
              let r2 = to_regex r2 in
              Regex.make_or r1 r2
            | Star (r,_) ->
              let r = to_regex r in
              Regex.make_star r
            | Closed r ->
              let r = to_regex r in
              Regex.make_closed r
            | Skip r ->
              to_regex r
            | Require r ->
              to_regex r
          end
        in
        r.node.regex <- Some ans;
        ans
      | Some ans -> ans
    end

  let rec is_empty
      (r:t)
    : bool =
    begin match node r with
      | Concat (r1,r2) ->
        is_empty r1 || is_empty r2
      | Or (r1,r2,_) ->
        is_empty r1 && is_empty r2
      | Empty ->
        true
      | Closed r ->
        is_empty r
      | Star (r,_) ->
        false
      | Base _ ->
        false
      | Skip r ->
        is_empty r
      | Require r ->
        is_empty r
    end

  let rec is_singleton
      (r:t)
    : bool =
    begin match node r with
      | Base _ ->
        true
      | Star (r,_) ->
        is_empty r
      | Closed r ->
        is_singleton r
      | Empty ->
        false
      | Or (r1,r2,_) ->
        (is_singleton r1 && is_empty r2)
        || (is_singleton r2 && is_empty r1)
      | Concat (r1,r2) ->
        is_singleton r1 && is_singleton r2
      | Skip r ->
        is_singleton r
      | Require r ->
        is_singleton r
    end

  let rec representative
      (r:t)
    : string option =
    begin match node r with
      | Concat (r1,r2) ->
        option_bind
          ~f:(fun s ->
              Option.map
                ~f:(fun s' -> s^s')
                (representative r2))
          (representative r1)
      | Or (r1,r2,p) ->
        begin match representative r1 with
          | Some s -> Some s
          | None -> representative r2
        end
      | Empty -> None
      | Closed r -> representative r
      | Base s -> Some s
      | Star (r,_) -> Some ""
      | Skip r -> representative r
      | Require r -> representative r
    end

  let rec representative_exn
      (r:t)
    : string =
    Option.value_exn (representative r)

  let likelihood_star : float = 0.8
  let info_content_star_multiplier : float =
    likelihood_star /. (1. -. likelihood_star)
  let info_content_star_const : float =
    -4. *. (Math.log2 likelihood_star)
    -. (Math.log2 (1. -. likelihood_star))
  let rec information_content
      (r:t)
    : float =
    let x = begin match node r with
      | Or(r1,r2,p) ->
        let ic1 = information_content r1 in
        let ic2 = information_content r2 in
        let not_p = Probability.not p in
        ((ic1 +. Probability.information_content p) *. p) +.
        ((ic2 +. Probability.information_content not_p) *. not_p)
      | Concat (r1,r2) ->
        let ic1 = information_content r1 in
        let ic2 = information_content r2 in
        ic1 +. ic2
      | Base _ -> 0.
      | Empty -> 0.
      | Closed r -> information_content r
      | Star (r,p) ->
        let ic = information_content r in
        let not_p = Probability.not p in
        let multiplier = p /. not_p in
        (multiplier *. ic) +.
        (multiplier *. (Probability.information_content p)) +.
        (Probability.information_content not_p)
      | Skip r -> 0.
      | Require r -> Float.infinity (* TODO: will this explode everything *)
    end in
    if (Float.compare x Float.nan = 0) then failwith (Regex.show @$ to_regex r);
    x
end

let stochastic_regex_star_semiring =
  (module StochasticRegex : StochasticStarSemiring.Sig with type t = StochasticRegex.t)

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

  let pp_function_container _ _ = ()

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

  let make_inverse
      (l:t)
    : t =
    Inverse l

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

  let invert
    : t -> t =
    fold
      ~disc_f:(fun r1 r2 s1 s2 -> make_disconnect r2 r1 s2 s1)
      ~concat_f:(fun l1 l2 -> make_concat l1 l2)
      ~swap_f:(fun l1 l2 -> make_swap l2 l1)
      ~union_f:(fun l1 l2 -> make_or l1 l2)
      ~compose_f:(fun l1 l2 -> make_compose l2 l1)
      ~iterate_f:(fun l -> make_star l)
      ~identity_f:(fun r -> make_ident r)
      ~inverse_f:(fun l -> make_inverse l)
      ~permute_f:(fun p ls ->
          let p_inv = Permutation.inverse p in
          let ls = Permutation.apply_to_list_exn p ls in
          make_permute p_inv ls)
      ~closed_f:(fun i fc ->
          make_closed
            fc.rl
            fc.rr
            fc.createl
            fc.creater
            fc.putl
            fc.putr
            i)
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


