(** Trace target *)
type target =
  | Stdout
  | File of string
[@@deriving eq, show]

(** Trace sources *)
type t =
  | SymbolTable of target list
  | SemanticAnalysis of target list
  | StackFrame of target list
  | Translation of target list
  | Escaping of target list
[@@deriving eq, show]
