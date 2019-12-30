open Core_kernel
open Err

module S = Symbol

type t =
  | Int
  | String
  | Record of (S.t * t) list * Unique.t
  | Array of t * Unique.t
  | Nil
  | Unit
  | Name of S.t * t option ref
[@@deriving eq, show]

(** Recursively lookups the underlying type *)
let rec actual = function
  | Name (sym, { contents = None }) ->
    type_error (Location.dummy sym) @@ Printf.sprintf
      "type %s is undefined" sym.S.name
  | Name (_, { contents = Some t }) ->
    actual t
  | t -> t

let (~!) x = actual x
let (=) x y = equal x y
let (<>) x y = not (equal x y)

let rec to_string = function
  | Int -> "int"
  | String -> "string"
  | Nil -> "nil"
  | Unit -> "()"
  | Name (s, _) -> s.S.name
  | Array (t, u) -> sprintf "[%s]<%s>" (to_string t) (Unique.to_string u)
  | Record (_, u) -> sprintf "record<%s>" (Unique.to_string u)
