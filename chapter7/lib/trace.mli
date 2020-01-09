module S = Symbol
module L = Location
module T = Type

module SymbolTable : sig
  (** Trace lookup *)
  val look : string -> S.t L.t -> unit

  (** Trace new binding *)
  val bind : string -> S.t L.t -> unit
end

module SemanticAnalysis : sig
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
  val tr_record_field : S.t L.t -> expr L.t -> T.t -> unit
  val tr_seq : (expr L.t) list -> unit
  val tr_assign : var L.t -> expr L.t -> unit
  val tr_cond : expr L.t -> expr L.t -> (expr L.t) option -> unit
  val tr_while : expr L.t -> expr L.t -> unit
  val tr_for : S.t L.t -> expr L.t -> expr L.t -> expr L.t -> bool ref -> unit
  val tr_break : unit L.t -> S.t option -> unit
  val tr_let : dec list -> expr L.t -> unit
  val tr_array : S.t L.t -> expr L.t -> expr L.t -> unit

  val trans_decs : dec list -> unit
  val trans_type_decs : (type_dec L.t) list -> unit
  val trans_fun_decs : (fun_dec L.t) list -> unit
  val trans_fun_head : fun_dec L.t -> unit
  val trans_var_dec : var_dec L.t -> unit

  val ret_ty : T.t -> unit

  val assert_ty : T.t -> expr L.t -> unit
  val assert_comparison : expr L.t -> expr L.t -> expr L.t -> unit
  val assert_op : expr L.t -> expr L.t -> unit
  val assert_fun_body : fun_dec L.t -> T.t -> unit
  val assert_init : var_dec L.t -> T.t -> unit
end

module StackFrame : sig
  open Frame

  val mk : t -> unit
end

module Translation : sig
  open Translate

  val new_level : level -> unit
  val alloc_local : access -> unit
end

module Escaping : sig
  val escapes : S.t L.t -> int -> unit
end

val mk_reporter : Config.t -> Logs.reporter
