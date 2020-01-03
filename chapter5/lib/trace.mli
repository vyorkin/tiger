module S = Symbol
module L = Location

(** Trace target *)
type target =
  | Stdout
  | File of string
[@@deriving eq, show]

(** Trace sources *)
type source =
  | Env of target list
  | Semant of target list
[@@deriving eq, show]

(** Trace symbol table lookup *)
val env_look : string -> S.t L.t -> unit

(** Trace new symbol table binding *)
val env_bind : string -> S.t L.t -> unit
