open Stdlib
open Star_semiring_tree_alignment

module Regex = struct
  type t =
    | Empty
    | Base of String.t
    | Concat of t * t
    | Union of t * t
    | Star of t
  [@@deriving ord, show, hash]
end

module Lens = struct
  type t =
    | Disconnect of Regex.t * Regex.t
    | Union of t * t
    | Concat of t * t
    | Iterate of t
  [@@deriving ord, show, hash]
end
