module T = Type
module Table = Symbol.Table

type access

(** Variable entry *)
type var_entry = {
  access: Translate.access; (** Describes how to access the variable **)
  ty: T.t (** Type of the variable *)
}

(** Function entry *)
type fun_entry = {
  level: Translate.level; (** Nesting level *)
  label: Temp.label; (** Label of the machine-code entry point *)
  formals: T.t list; (** Types of the formal parameters *)
  result: T.t (** Type of the result returned by the function **)
}

(** Term-level entry *)
type entry =
  | VarEntry of var_entry
  | FunEntry of fun_entry

(** Contains bindings for predefined functions *)
val base_venv : entry Table.t

(** Predefined types *)
val base_tenv : T.t Table.t
