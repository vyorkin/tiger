open Symbol

module T = Type

type access

type entry =
  | VarEntry of T.t
  | FunEntry of
      T.t list * (* types of the formal parameters *)
      T.t (* type of the result returned by the function (or unit) *)

let base_venv = Table.empty

let base_tenv =
  Table.empty
  |> Table.add (symbol "string") T.String
  |> Table.add (symbol "int") T.Int
