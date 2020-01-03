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
val find_env : string -> S.t L.t -> unit

(** Trace symbol table new binding *)
val set_env : string -> S.t L.t -> unit
