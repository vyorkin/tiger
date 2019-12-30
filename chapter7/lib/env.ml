module T = Type
module S = Symbol

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

let base_venv = S.Table.empty

let base_tenv =
  S.Table.empty
  |> S.Table.add_exn ~key:(S.mk "string") ~data:T.String
  |> S.Table.add_exn ~key:(S.mk "int") ~data:T.Int
