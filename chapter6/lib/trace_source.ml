(** Trace target *)
type target =
  | Stdout
  | File of string
[@@deriving eq, show]

(** Trace sources *)
type t =
  | Symbol of target list
  | Semant of target list
[@@deriving eq, show]
