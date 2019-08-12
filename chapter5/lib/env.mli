type access

type entry =
  | VarEntry of Type.t
  | FunEntry of
      Type.t list * (* types of the formal parameters *)
      Type.t (* type of the result returned by the function (or unit) *)

(** Contains bindings for predefined functions *)
val base_venv : entry Symbol.Table.t

(** Predefined types *)
val base_tenv : Type.t Symbol.Table.t
