(** Used for equality testing *)
type unique = unit ref [@@deriving show]

type t =
  | Int
  | String
  | Record of (Symbol.t) list * unique
  | Array of t * unique
  | Nil
  | Unit
  | Name of Symbol.t * t option ref
