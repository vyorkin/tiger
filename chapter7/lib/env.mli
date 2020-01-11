module T = Type
module Tr = Translate
module L = Location
module S = Symbol
module ST = Symbol_table

(** Variable entry *)
type var_entry = {
  (** Describes how to access the variable.Basically it is
      location (in memory/frame or in a register) and [Translate.level] *)
  access: Tr.access;
  (** Type of the variable *)
  ty: T.t
}

(** Function entry *)
type fun_entry = {
  level: Tr.level; (** Nesting level *)
  label: Temp.label; (** Label of the machine-code entry point *)
  formals: T.t list; (** Types of the formal parameters *)
  result: T.t (** Type of the result returned by the function **)
}

(** Term-level entry *)
type ventry =
  | VarEntry of var_entry
  | FunEntry of fun_entry
[@@deriving show]

type venv = ventry ST.t
type tenv = T.t ST.t

type t = {
  (** Type-level environemnt *)
  tenv : tenv;
  (** Term-level (value-level) environment *)
  venv : venv;
  (** Nesting level *)
  level : Tr.level;
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
