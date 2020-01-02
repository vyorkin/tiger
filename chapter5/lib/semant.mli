(** Type-level environment *)
type tenv = Type.t Symbol.Table.t

(** Term-level environment *)
type venv = Env.entry Symbol.Table.t

(** Semantic analysis parameters *)
type params =
  { trace : bool
  }

type ctx = {
  tenv : tenv;
  venv : venv;
  path : (Syntax.expr Location.t) list;
  params : params
}

(** Translated expression and its type *)
type expr_ty =
  { expr : Translate.t;
    ty : Type.t;
  }

(** Type-checks an AST and produces an error in
    case of mismatching types or undeclared identifiers *)
val trans_prog : Syntax.expr -> params:params -> unit

(** Type-checks and translates the
    expression into intermediate code *)
val trans_expr : Syntax.expr Location.t -> ctx:ctx -> expr_ty

(** Translates a AST type expression into
    a digested type description that we keed in
    the type-level environment *)
val trans_ty : tenv -> Syntax.ty -> Type.t
