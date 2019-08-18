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

let compare x y =
  match x, y with
  | Record (_, u1), Record (_, u2) ->
    Pervasives.compare u1 u2
  | Record _, Nil -> 0
  | Nil, Record _ -> 0
  | Array (_, u1), Array (_, u2) ->
    Pervasives.compare u1 u2
  | Name (sx, _), Name (sy, _) ->
    Pervasives.compare (Symbol.id sx) (Symbol.id sy)
  | x, y ->
    Pervasives.compare x y

let eq x y = compare x y = 0
let neq a b = not (eq a b)
