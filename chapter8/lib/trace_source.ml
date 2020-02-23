(* see https://github.com/janestreet/base/blob/master/src/ppx_compare_lib.ml#L26 *)
open Ppx_compare_lib.Builtin

(** Trace target *)
type target =
  | Stdout
  | File of string
[@@deriving compare, equal, show]

(** Trace sources *)
type t =
  | SymbolTable of target list
  | SemanticAnalysis of target list
  | StackFrame of target list
  | Translation of target list
  | Escaping of target list
[@@deriving compare, equal, show]
