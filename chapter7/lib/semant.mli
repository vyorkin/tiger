(** Type-level environment *)
type tenv = Type.t Symbol.Table.t

(** Term-level environment *)
type venv = Env.entry Symbol.Table.t

(** Translated expression and its type *)
type expr = {
  ty : Type.t;
  expr : Translate.expr;
}

(** Type-checks an AST and produces an error in
    case of mismatching types or undeclared identifiers. *)
val trans_prog : Syntax.expr -> unit

(** Type-checks and translates the expression into intermediate code. *)
val trans_expr : venv -> tenv -> Translate.level -> Syntax.expr Location.t -> expr

(** Translates a AST type expression into
    a digested type description that we keed in the type-level environment. *)
val trans_ty : tenv -> Syntax.ty -> Type.t