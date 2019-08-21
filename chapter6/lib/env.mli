type access

type var_entry = {
  access: Translate.access; (* Describes how to access the variable *)
  ty: Type.t (* Type of the variable *)
}

type fun_entry = {
  level: Translate.level; (* nesting level *)
  label: Temp.label; (* label of the machine-code entry point *)
  formals: Type.t list;  (* types of the formal parameters *)
  result: Type.t (* type of the result returned by the function (or unit) *)
}

type entry =
  | VarEntry of var_entry
  | FunEntry of fun_entry

(** Contains bindings for predefined functions *)
val base_venv : entry Symbol.Table.t

(** Predefined types *)
val base_tenv : Type.t Symbol.Table.t
