module S = Symbol
module L = Location
module T = Type

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

module Semant : sig
  open Syntax

  val trans_prog : expr -> unit
  val trans_ty : ty -> unit
  val tr_expr : expr L.t -> unit
  val tr_var : var L.t -> unit
  val tr_simple_var : S.t L.t -> unit
  val tr_field_var : var L.t -> S.t L.t -> unit
  val tr_subscript_var : var L.t -> expr L.t -> unit
  val tr_call : S.t L.t -> expr L.t list -> unit
  val tr_op : expr L.t -> expr L.t -> op -> unit
  val tr_record : S.t L.t -> (S.t L.t * expr L.t) list -> unit
  val tr_record_field : T.t -> S.t L.t -> expr L.t -> unit
  val tr_seq : (expr L.t) list -> unit
  val tr_assign : var L.t -> expr L.t -> unit
  val tr_cond : expr L.t -> expr L.t -> (expr L.t) option -> unit
  val tr_while : expr L.t -> expr L.t -> unit
  val tr_for : S.t L.t -> expr L.t -> expr L.t -> expr L.t -> unit
  val tr_break : unit L.t -> S.t option -> unit
  val tr_let : dec list -> expr L.t -> unit
  val tr_array : S.t L.t -> expr L.t -> expr L.t -> unit

  val trans_decs : dec list -> unit
  val trans_tys : (type_dec L.t) list -> unit
  val trans_funs : (fun_dec L.t) list -> unit
  val trans_fun_head : fun_dec L.t -> unit
  val trans_var : var_dec L.t -> unit

  val ret_ty : T.t -> unit

  val assert_ty : T.t -> expr L.t -> unit
  val assert_comparison : expr L.t -> expr L.t -> expr L.t -> unit
  val assert_op : expr L.t -> expr L.t -> unit
  val assert_fun_body : fun_dec L.t -> T.t -> unit
  val assert_init : var_dec L.t -> T.t -> unit
end
