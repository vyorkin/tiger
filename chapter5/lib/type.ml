open Error
open Printf

type t =
  | Int
  | String
  | Record of (Symbol.t * t) list * Unique.t
  | Array of t * Unique.t
  | Nil
  | Unit
  | Name of Symbol.t * t option ref
  [@@deriving eq]

(** Recursively lookups the underlying type *)
let rec actual = function
  | Name (sym, { contents = None }) ->
    type_error (Location.dummy sym) @@ Printf.sprintf
      "type %s is undefined" (Symbol.name sym)
  | Name (_, { contents = Some t }) ->
    actual t
  | t -> t

let rec to_string = function
  | Int -> "int"
  | String -> "string"
  | Nil -> "nil"
  | Unit -> "()"
  | Name (s, _) -> Symbol.name s
  | Array (t, u) -> sprintf "[%s]<%s>" (to_string t) (Unique.to_string u)
  | Record (_, u) -> sprintf "record<%s>" (Unique.to_string u)
