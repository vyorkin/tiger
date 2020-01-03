module T = Type
module ST = Symbol_table

type access

type entry =
  | VarEntry of T.t
  | FunEntry of
      T.t list * (** types of the formal parameters *)
      T.t (** type of the result returned by the function (or unit) **)
  [@@deriving show]

type venv = entry ST.t
type tenv = T.t ST.t

(** Contains bindings for predefined functions *)
val base_venv : venv

(** Predefined types *)
val base_tenv : tenv
