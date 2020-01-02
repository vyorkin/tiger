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
[@@deriving show]

(* we cannot use [@@deriving eq] here, so
   let's just implement the comparsion ourselves *)

let compare x y =
  match x, y with
  | Record (_, u1), Record (_, u2) ->
    compare u1 u2
  | Record _, Nil -> 0
  | Nil, Record _ -> 0
  | Array (_, u1), Array (_, u2) ->
    compare u1 u2
  | Name (sx, _), Name (sy, _) ->
    S.(compare sx.id sy.id)
  | x, y ->
    compare x y

(** Recursively lookups the underlying type *)
let rec actual = function
  | Name (sym, { contents = None }) ->
    type_error (Location.dummy sym) @@ Printf.sprintf
      "type %s is undefined" sym.name
  | Name (_, { contents = Some t }) ->
    actual t
  | t -> t

let (=) x y = compare x y = 0
let (<>) a b = not (a = b)
let (~!) x = actual x

let assignable x y =
  match ~!x, ~!y with
  (* "nil" is legal value for records *)
  | Record _, Nil -> true
  | a, b -> a = b

let rec to_string x =
  let open Core_kernel in
  match x with
  | Int -> "int"
  | String -> "string"
  | Nil -> "nil"
  | Unit -> "()"
  | Name (s, _) -> s.name
  | Array (t, u) -> sprintf "[%s]<%s>" (to_string t) (Unique.to_string u)
  | Record (_, u) -> sprintf "record<%s>" (Unique.to_string u)
