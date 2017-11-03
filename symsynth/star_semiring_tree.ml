open Stdlib

module PlusTimesStarTreeOf
    (PD : Data)
    (TD : Data)
    (SD : Data)
    (BD : Data) =
struct
  type nonempty_t =
    | Plus of PD.t * nonempty_t list
    | Times of TD.t * nonempty_t list
    | Star of SD.t * nonempty_t
    | Base of BD.t
  [@@deriving ord, show, hash]

  type t =
    | Empty
    | NonemptyTree of nonempty_t
  [@@deriving ord, show, hash]
end
