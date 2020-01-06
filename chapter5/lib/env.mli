module T = Type
module L = Location
module S = Symbol
module ST = Symbol_table

type access

type ventry =
  | VarEntry of T.t
  | FunEntry of
      T.t list * (** types of the formal parameters *)
      T.t (** type of the result returned by the function (or unit) **)
[@@deriving show]

type venv = ventry ST.t
type tenv = T.t ST.t

type t = {
  (** Type-level environemnt *)
  tenv : tenv;
  (** Term-level (value) environment *)
  venv : venv;
  (** AST traversal path *)
  path : (Syntax.expr L.t) list;
  (** "Inside a loop" marker **)
  loop : S.t option;
}

(** Create a new environment *)
val mk : unit -> t

(** Push the given expression to the [t.path] stack *)
val enter_expr : t -> Syntax.expr L.t -> t

(** Set the "inside a loop" marker *)
val enter_loop : t -> string -> t

(** Contains bindings for predefined functions *)
val base_venv : venv

(** Predefined types *)
val base_tenv : tenv
