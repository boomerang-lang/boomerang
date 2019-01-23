open MyStdlib
open Consts

module type BaseData = sig
  include Data
  val information_content : t -> float
end

module type LabelData = sig
  include Data
  val as_count : t -> int
end


module NonemptyLabelledPlusTimesStarTreeOf
    (PD : Data)
    (TD : Data)
    (SD : Data)
    (BD : BaseData)
    (L  : LabelData) =
struct
  type t = t_node hash_consed
  and t_node =
    | Base of BD.t
    | Plus of PD.t * (l * Probability.t * int) list
    | Times of TD.t * (l * bool) list
    | Star of SD.t * l * Probability.t
  and l = t * L.t
  [@@deriving ord, show, hash]

  let table = HashConsTable.create 10
  let hashcons = HashConsTable.hashcons hash_t_node compare_t_node table

  let uid (t:t) = t.tag

  let mk_base bl = hashcons (Base bl)

  let mk_plus pl ls = hashcons (Plus (pl,ls))

  let mk_times tl ls = hashcons (Times (tl,ls))

  let mk_star sl l p = hashcons (Star (sl,l,p))

  let fold_downward_upward
      (type a)
      (type b)
      ~init:(init:b)
      ~upward_base:(upward_base:b -> BD.t -> a)
      ~upward_plus:(upward_plus:b -> PD.t -> (L.t * a * Probability.t * int) list -> a)
      ~upward_times:(upward_times:b -> TD.t -> (L.t * a * bool) list -> a)
      ~upward_star:(upward_star:b -> SD.t -> Probability.t -> L.t -> a -> a)
      ?downward_plus:(downward_plus:b -> PD.t -> L.t -> b = curry3 fst_trip)
      ?downward_times:(downward_times:b -> TD.t -> L.t -> b = curry3 fst_trip)
      ?downward_star:(downward_star:b -> SD.t -> Probability.t -> L.t -> b = curry4 fst_quad)
      (ptst:t)
    : a =
    let rec fold_downward_upward_internal
        (downward_acc:b)
        (nptst:t)
      : a =
      begin match nptst.node with
        | Base bd ->
          upward_base downward_acc bd
        | Plus (pd,ts) ->
          upward_plus
            downward_acc
            pd
            (List.map
               ~f:(fun ((t,l),p,i) ->
                   let downward_acc' = downward_plus downward_acc pd l in
                   (l
                   ,fold_downward_upward_internal downward_acc' t
                   ,p
                   ,i))
               ts)
        | Times (td,ts) ->
          upward_times
            downward_acc
            td
            (List.map
               ~f:(fun ((t,l),b) ->
                   let downward_acc' = downward_times downward_acc td l in
                   (l
                   ,fold_downward_upward_internal downward_acc' t
                   ,b))
               ts)
        | Star (sd,(t,l),p) ->
          let downward_acc' = downward_star downward_acc sd p l in
          upward_star
            downward_acc
            sd
            p
            l
            (fold_downward_upward_internal downward_acc' t)
      end

    in
    fold_downward_upward_internal init ptst

  let fold
      (type a)
      ~base_f:(base_f:BD.t -> a)
      ~plus_f:(plus_f:PD.t -> (L.t * a * Probability.t * int) list -> a)
      ~times_f:(times_f:TD.t -> (L.t * a * bool) list -> a)
      ~star_f:(star_f:SD.t -> Probability.t -> L.t -> a -> a)
      v
    : a =
    fold_downward_upward
      ~init:()
      ~upward_base:(thunk_of base_f)
      ~upward_plus:(thunk_of plus_f)
      ~upward_times:(thunk_of times_f)
      ~upward_star:(thunk_of star_f)
      v

  let likelihood_star : float = 0.8
  let info_content_star_multiplier : float =
    likelihood_star /. (1. -. likelihood_star)
  let info_content_star_const : float =
    -4. *. (Math.log2 likelihood_star)
    -. (Math.log2 (1. -. likelihood_star))

  let information_content
      (tr:t)
    : float =
    if !no_intelligent_cost then
      0.
    else if !constants_cost then
      1.
    else
      fold
        ~base_f:BD.information_content
        ~plus_f:(fun _ lfl ->
            let nfl = List.map ~f:(fun (l,f,p,_) -> (L.as_count l,f)) lfl in
            let (n,f) =
              List.fold_left
                ~f:(fun (n_acc,f_acc) (n,f) ->
                    (n_acc+n
                    ,f_acc +. (f *. (Float.of_int n))))
                ~init:(0,0.)
                nfl
            in
            (Math.log2 @$ Float.of_int @$ n)
            +. (f /. Float.of_int n))
        ~times_f:(fun _ lfl ->
            let nfl = List.map ~f:(fun (l,f,_) -> (L.as_count l,f)) lfl in
            List.fold_left
              ~f:(fun acc (n,f) ->
                  acc +. (f *. (Float.of_int n)))
              ~init:0.
              nfl)
        ~star_f:(fun _ p _ ic ->
            let not_p = Probability.not p in
            let multiplier = p /. not_p in
            (multiplier *. ic) +.
            (multiplier *. (Probability.information_content p)) +.
            (Probability.information_content not_p))
        tr
end

module LabelledPlusTimesStarTreeOf
    (PD : Data)
    (TD : Data)
    (SD : Data)
    (BD : BaseData)
    (L  : LabelData) =
struct
  module Nonempty = NonemptyLabelledPlusTimesStarTreeOf(PD)(TD)(SD)(BD)(L)

  type t =
    | Empty
    | Nonempty of Nonempty.t
  [@@deriving ord, show, hash]

  let fold
      (type a)
      ~empty_f:(empty_f:a)
      ~nonempty_f:(nonempty_f:Nonempty.t -> a)
      (ptst:t)
    : a =
    begin match ptst with
      | Empty -> empty_f
      | Nonempty nptst -> nonempty_f nptst
    end


end


module PlusTimesStarTreeOf
    (PD : Data)
    (TD : Data)
    (SD : Data)
    (BD : Data) =
struct
  type nonempty_t =
    | Base of BD.t
    | Plus of PD.t * (nonempty_t * Probability.t * int) list
    | Times of TD.t * (nonempty_t * bool) list
    | Star of SD.t * Probability.t * nonempty_t
  [@@deriving ord, show, hash]

  type t =
    | Empty
    | Nonempty of nonempty_t
  [@@deriving ord, show, hash]

  let mk_base
      (b:BD.t)
    : nonempty_t =
    Base b

  let mk_plus
      (p:PD.t)
      (ts:(nonempty_t * Probability.t * int) list)
    : nonempty_t =
    Plus (p,ts)

  let mk_times
      (t:TD.t)
      (ts:(nonempty_t * bool) list)
    : nonempty_t =
    Times (t,ts)

  let mk_star
      (s:SD.t)
      (p:Probability.t)
      (t:nonempty_t)
    : nonempty_t =
    Star (s,p,t)

  let mk_nonempty
      (t:nonempty_t)
    : t =
    Nonempty t

  let mk_empty
      (t:nonempty_t)
    : t =
    Nonempty t

  let fold_downward_upward
      ~init:(init:'b)
      ~upward_empty:(upward_empty:'c)
      ~upward_nonempty:(upward_nonempty:'a -> 'c)
      ~upward_base:(upward_base:'b -> BD.t -> 'a)
      ~upward_plus:(upward_plus:'b -> PD.t -> ('a * Probability.t * int) list -> 'a)
      ~upward_times:(upward_times:'b -> TD.t -> ('a * bool) list -> 'a)
      ~upward_star:(upward_star:'b -> SD.t -> Probability.t -> 'a -> 'a)
      ?downward_plus:(downward_plus:'b -> PD.t -> 'b = curry fst)
      ?downward_times:(downward_times:'b -> TD.t -> 'b = curry fst)
      ?downward_star:(downward_star:'b -> SD.t -> Probability.t -> 'b = curry3 fst_trip)
      (ptst:t)
    : 'c =
    let rec fold_downward_upward_nonempty_internal
        (downward_acc:'b)
        (nptst:nonempty_t)
      : 'a =
      begin match nptst with
        | Base bd ->
          upward_base downward_acc bd
        | Plus (pd,ts) ->
          let downward_acc' = downward_plus downward_acc pd in
          upward_plus
            downward_acc
            pd
            (List.map
               ~f:(fun (l,p,i) -> (fold_downward_upward_nonempty_internal downward_acc' l,p,i))
               ts)
        | Times (td,ts) ->
          let downward_acc' = downward_times downward_acc td in
          upward_times
            downward_acc
            td
            (List.map
               ~f:(fun (l,b) -> (fold_downward_upward_nonempty_internal downward_acc' l,b))
               ts)
        | Star (sd,p,t) ->
          let downward_acc' = downward_star downward_acc sd p in
          upward_star
            downward_acc
            sd
            p
            (fold_downward_upward_nonempty_internal downward_acc' t)
      end

    in
    begin match ptst with
      | Empty -> upward_empty
      | Nonempty nptst ->
        upward_nonempty
          (fold_downward_upward_nonempty_internal init nptst)
    end


  let fold
      ~empty_f:(empty_f:'c)
      ~nonempty_f:(nonempty_f:'a -> 'c)
      ~base_f:(base_f:BD.t -> 'a)
      ~plus_f:(plus_f:PD.t -> ('a * Probability.t * int) list -> 'a)
      ~times_f:(times_f:TD.t -> ('a * bool) list -> 'a)
      ~star_f:(star_f:SD.t -> Probability.t -> 'a -> 'a)
      v
    : 'c =
    fold_downward_upward
      ~init:()
      ~upward_empty:(empty_f)
      ~upward_nonempty:(nonempty_f)
      ~upward_base:(thunk_of base_f)
      ~upward_plus:(thunk_of plus_f)
      ~upward_times:(thunk_of times_f)
      ~upward_star:(thunk_of star_f)
      v
end

module NormalizedPlusTimesStarTreeOf
    (PD : Data)
    (TD : Data)
    (SD : Data)
    (BD : BaseData) =
struct
  module NormalizationScript =
  struct
    module PD_NormalizationLabel =
    struct
      type t =
        {
          label : PD.t                 ;
          perm  : CountedPermutation.t ;
        }
      [@@deriving ord, show, hash, make]

      let get_label
          (nl:t)
        : PD.t =
        nl.label

      let get_perm
          (nl:t)
        : CountedPermutation.t =
        nl.perm
    end

    module TD_NormalizationLabel =
    struct
      type t =
        {
          label : TD.t                 ;
          perm  : CountedPermutation.t ;
        }
      [@@deriving ord, show, hash, make]

      let get_label
          (nl:t)
        : TD.t =
        nl.label

      let get_perm
          (nl:t)
        : CountedPermutation.t =
        nl.perm
    end

    include PlusTimesStarTreeOf
        (PD_NormalizationLabel)
        (TD_NormalizationLabel)
        (SD)
        (BD)
  end

  module NonNormalizedTree = PlusTimesStarTreeOf(PD)(TD)(SD)(BD)

  module IntLabel =
  struct
    include IntModule
    let as_count = ident
  end

  include LabelledPlusTimesStarTreeOf
      (PD)
      (TD)
      (SD)
      (BD)
      (IntLabel)

  let from_tree : NonNormalizedTree.t -> t * NormalizationScript.t =
    NonNormalizedTree.fold
      ~empty_f:(Empty,NormalizationScript.Empty)
      ~nonempty_f:(fun (nt,nns) ->
          (Nonempty nt
          ,NormalizationScript.Nonempty nns))
      ~base_f:(fun bl ->
          (Nonempty.mk_base bl
          ,NormalizationScript.Base bl))
      ~plus_f:(fun pl nsntps ->
          let (nsnts,ps,is) = List.unzip3 nsntps in
          let (nts,nss) = List.unzip nsnts in
          let ntsps = List.zip_exn nts ps in
          let ntspsis =
            List.map
              ~f:(fun ((i,j),k) -> (i,j,k))
              (List.zip_exn ntsps is)
          in
          let nssps = List.zip_exn nss ps in
          let nsspsis =
            List.map
              ~f:(fun ((i,j),k) -> (i,j,k))
              (List.zip_exn nssps is)
          in
          let (perm,sv) =
            CountedPermutation.sorting
              ~cmp:(triple_compare Nonempty.compare Probability.compare Int.compare)
              ntspsis
          in
          let nvs =
            List.map
              ~f:(fun xs ->
                  let hd = List.hd_exn xs in
                  ((fst3 hd
                   ,List.length xs)
                  ,snd3 hd
                  ,trd3 hd))
              sv
          in
          let norm_label =
            NormalizationScript.PD_NormalizationLabel.make
              ~label:pl
              ~perm:perm
          in
          (Nonempty.mk_plus pl nvs
          ,NormalizationScript.Plus (norm_label,nsspsis)))
      ~times_f:(fun t nsntsbs ->
          let (nsnts,bs) = List.unzip nsntsbs in
          let (nts,nss) = List.unzip nsnts in
          let ntsbs = List.zip_exn nts bs in
          let (perm,sv) =
            CountedPermutation.sorting
              ~cmp:(pair_compare Nonempty.compare Bool.compare)
              ntsbs
          in
          let nvs =
            List.map
              ~f:(fun xs ->
                  let hd = List.hd_exn xs in
                  ((fst hd
                   ,List.length xs)
                  ,snd hd))
              sv
          in
          let norm_label =
            NormalizationScript.TD_NormalizationLabel.make
              ~label:t
              ~perm:perm
          in
          (Nonempty.mk_times t nvs
          ,NormalizationScript.Times (norm_label,(List.map ~f:(fun ns -> (ns,true)) nss))))
      ~star_f:(fun s p (t,ns) ->
          (Nonempty.mk_star s (t,1) p
          ,NormalizationScript.Star (s,p,ns)))
end
