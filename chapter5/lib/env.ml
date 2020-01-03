module S = Symbol
module ST = Symbol_table
module T = Type

type access

type entry =
  | VarEntry of T.t
  | FunEntry of
      T.t list * (* types of the formal parameters *)
      T.t (* type of the result returned by the function (or unit) *)
  [@@deriving show { with_path = false }]

type venv = entry ST.t
type tenv = T.t ST.t

let base_venv = ST.empty

let base_tenv =
  let open ST in
  empty
  |> add_exn ~key:(S.mk "string") ~data:T.String
  |> add_exn ~key:(S.mk "int") ~data:T.Int
