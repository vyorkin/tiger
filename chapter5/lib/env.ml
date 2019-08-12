module T = Type
module S = Symbol

type access

type entry =
  | VarEntry of T.t
  | FunEntry of
      T.t list * (* types of the formal parameters *)
      T.t (* type of the result returned by the function (or unit) *)

let base_venv = S.Table.empty

let base_tenv =
  S.Table.empty
  |> S.Table.add (S.symbol "string") T.String
  |> S.Table.add (S.symbol "int") T.Int
