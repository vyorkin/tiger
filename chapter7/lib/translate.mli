(** Translated to IR expression *)
type expr [@@deriving show]

(** Represents a nesting level *)
type level = {
  (** Parent level *)
  parent: level option;
  (** Frame of the current level *)
  frame: Frame.t
} [@@deriving show]

(** Returns a stack of all frames, including the current/given frame *)
val frames : level -> Frame.t list

(** Returns a list of ids of all stack frames,
    including the current/given frame *)
val frames_path : level -> int list

(** Describes a way to access a formal parameter or a local variable.
    Basically, it is just a [Frame.access] plus a nesting [level] *)
type access = level * Frame.access [@@deriving show]

(** Outermost level at which all
    top-level functions and variables are declared *)
val outermost : level

(** Creates a new "nesting level" for a function *)
val new_level
  :  parent:level option
  -> label:Temp.label
  -> formals:bool list
  -> level

(** Extracts a list of accesses *)
val formals : level -> access list

(** Creates an [access] at the given [level].
    The argument [bool] specifies whether the variable escapes *)
val alloc_local : level:level -> escapes:bool -> access

(** Static link -- pointer to / address of the frame of
    the function statically enclosing current function **)
module Sl : sig
  (** We need to use static links to access variables
      declared at an outer level of static scope.

      For example, to access some variable [x] which is declared
      somewhere outside of the current level/scope/frame the
      generated IR code should look like:

      Mem(BinOp(Const k_n, Plus, Mem(BinOp(Const k_n-1, Plus,
      ...
      Mem(BinOp(Const k_1, Plus, Temp fp))))))

      where k_1,...,k_n-1 are the various SL offsets in nested functions,
      and k_n is the offset of our variable [x] in its own frame.

      This function follows static links (SL) between the
      [current] [Translate.level] of use and the [Translate.level] of [definition]. *)
  val follow : cur:level -> def:level -> Ir.expr
end

module Printer : sig
  val print_expr : expr -> string
end

(* The manipulation of [Ir.t] nodes should all be in
   this module, not it [Semant]. Doing it in [Semant] would
   clutter up the readability of that moudle and would make
   [Semant] dependent on the [Ir] representation.

   Instead, we introduce the following interface: *)

val e_unit : expr
val e_nil : expr
val e_int : int -> expr
val e_string : string -> expr
val e_binop : expr * Syntax.op * expr -> expr
val e_relop : expr * Syntax.op * expr -> expr
val e_simple_var : access * level -> expr
val e_subscript_var : expr * expr -> expr
val e_field_var : expr * Symbol.t * Symbol.t list -> expr
val e_record : expr list -> expr
val e_array : expr * expr -> expr
val e_cond : expr * expr * expr option -> expr
val e_loop : expr * expr * Temp.label -> expr
val e_break : Temp.label -> expr
val e_call : level * Temp.label * expr list * bool -> expr
val e_assign : expr * expr -> expr
val e_seq : expr list -> expr
val e_let : expr list * expr -> expr

val e_dummy : unit -> expr
