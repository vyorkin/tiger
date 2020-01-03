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

module Symbol : sig
  (** Trace lookup *)
  val look : string -> S.t L.t -> unit

  (** Trace new binding *)
  val bind : string -> S.t L.t -> unit
end

(* module Semant : sig
 *   val trans_prog : Syntax.expr L.t -> unit
 *   val trans_expr : Syntax.expr L.t -> unit
 *   val trnas_ty : Syntax.ty -> unit
 * end *)
