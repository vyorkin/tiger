module T = Type
module L = Location
module S = Symbol
module ST = Symbol_table

type var_entry = {
  access: Translate.access;
  ty: T.t
} [@@deriving show { with_path = false }]

type fun_entry = {
  level: Translate.level;
  label: Temp.label;
  formals: T.t list;
  result: T.t
} [@@deriving show { with_path = false }]

type ventry =
  | VarEntry of var_entry
  | FunEntry of fun_entry
[@@deriving show { with_path = false }]

type venv = ventry ST.t
type tenv = T.t ST.t

type t = {
  tenv : tenv;
  venv : venv;
  level : Translate.level;
  path : (Syntax.expr L.t) list;
  loop : S.t option;
}

let base_venv = ST.empty

let base_tenv =
  let open ST in
  empty
  |> add_exn ~key:(S.mk "string") ~data:T.String
  |> add_exn ~key:(S.mk "int") ~data:T.Int
  |> add_exn ~key:(S.mk "nil") ~data:T.Nil
  |> add_exn ~key:(S.mk "unit") ~data:T.Unit

let mk () = {
  tenv = base_tenv;
  venv = base_venv;
  level = Translate.outermost;
  path = [];
  loop = None;
}

let enter_expr env expr =
  { env with path = expr :: env.path }

let enter_loop env name =
  { env with loop = Some (S.mk_unique name) }
