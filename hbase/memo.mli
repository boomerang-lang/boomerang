(***************************************************)
(* The Harmony Project                             *)
(* harmony@lists.seas.upenn.edu                    *)
(*                                                 *)
(* memo.mli - memoizing infrastructure              *)
(***************************************************)
(* $Id$ *)


module type MemoType = sig
  type arg
  type res
  val name : string
  val init_size : int
  val format_arg : arg -> unit
  val format_res : res -> unit
  val hash : arg -> int
  val equal : arg -> arg -> bool
end

module type MemoType2 = sig
  include MemoType
  val hash' : arg -> int
  val equal' : arg -> arg -> bool
end

module type MemoFun = sig
  include MemoType 
  val f : arg -> res
end  

module type MemoFun2 = sig
  include MemoType2
  val f : arg -> res
end
  
module Make(M:MemoFun) : sig 
  val memoized : M.arg -> M.res
  val find : M.arg -> M.res option
end

module Make2(M:MemoFun2) : sig 
  val memoized : M.arg -> M.res
  val find : M.arg -> M.res option
end

module MakeLater(M:MemoType) : sig
  val memoize : (M.arg -> M.res) -> M.arg -> M.res
  val find : M.arg -> M.res option
end

module MakeLater2(M:MemoType2) : sig
  val memoize : (M.arg -> M.res) -> M.arg -> M.res
  val find : M.arg -> M.res option
end
  
val format_stats : unit -> unit

  

