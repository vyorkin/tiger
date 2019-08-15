open Error

(** Used for equality testing
    to distinguish between different record types *)
type unique = unit ref [@@deriving show]

type t =
  | Int
  | String
  | Record of (Symbol.t * t) list * unique [@printer fun fmt _ -> fprintf fmt "Record"]
  | Array of t * unique
  | Nil
  | Unit
  | Name of Symbol.t * t option ref
  [@@deriving show]

(** Recursively lookups the underlying type *)
let rec actual = function
  | Name (sym, { contents = None }) ->
    type_error (Location.dummy sym) @@ Printf.sprintf
    "unresolved type alias %s" (Symbol.name sym)
  | Name (_, { contents = Some t }) ->
    actual t
  | t -> t
