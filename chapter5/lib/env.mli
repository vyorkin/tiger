open Symbol

module T = Type

type access

type entry =
  | VarEntry of T.t
  | FunEntry of
      T.t list * (* types of the formal parameters *)
      T.t (* type of the result returned by the function (or unit) *)

(** Contains bindings for predefined functions *)
val base_venv : entry Table.t

(** Predefined types *)
val base_tenv : T.t Table.t
