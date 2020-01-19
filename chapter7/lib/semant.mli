module ST = Symbol_table

(** Translated expression and its type *)
type expr_ty = {
  expr : Translate.expr;
  ty : Type.t;
}

(** Type-checks an AST and produces an error in
    case of mismatching types or undeclared identifiers *)
val trans_prog : Syntax.expr -> Fragment.t list

(** Type-checks and translates the expression into intermediate code *)
val trans_expr : Syntax.expr Location.t -> env:Env.t -> expr_ty

(** Translates a AST type expression into
    a digested type description that we keed in
    the type-level environment *)
val trans_ty : Env.tenv -> Syntax.ty -> Type.t
