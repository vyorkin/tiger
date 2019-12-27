module S = Symbol
module T = Type

type access

type entry =
  | VarEntry of T.t
  | FunEntry of
      T.t list * (* types of the formal parameters *)
      T.t (* type of the result returned by the function (or unit) *)

let base_venv = S.Table.empty

let base_tenv =
  S.Table.empty
  |> S.Table.add_exn ~key:(S.mk "string") ~data:T.String
  |> S.Table.add_exn ~key:(S.mk "int") ~data:T.Int
