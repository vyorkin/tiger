module T = Type
module S = Symbol
module Table = S.Table

type access

type var_entry = {
  access: Translate.access;
  ty: T.t
}

type fun_entry = {
  level: Translate.level;
  label: Temp.label;
  formals: T.t list;
  result: T.t
}

type entry =
  | VarEntry of var_entry
  | FunEntry of fun_entry

let base_venv = Table.empty

let base_tenv =
  Table.empty
  |> Table.add (S.symbol "string") T.String
  |> Table.add (S.symbol "int") T.Int
