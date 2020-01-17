(** Translated to IR expression *)
type expr [@@deriving show]

(** Represents a nesting level *)
type level = {
  (** Parent level *)
  parent: level option;
  (** Frame of the current level *)
  frame: Frame.t
} [@@deriving show]

(** Returns a list of all parent frames,
    including the current/given frame *)
val parent_frames : level -> Frame.t list

(** Returns a list of ids of all stack frames,
    including the current/given frame *)
val frames_path : level -> int list

(** Given a [level] returns a number indicating
    how deep it is in the frame stack *)
val nesting_depth : level -> int

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

(** Static link (SL) -- pointer to / address of the frame of
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

(** Translates a [()] unit expression to [Const 0] **)
val e_unit : expr
(** Translates "nil" to [Const 0] **)
val e_nil : expr
(** Translates an integer [n : int] to [Const n] **)
val e_int : int -> expr
(** Translates a [string] literal to the constant address (label) of
    a segment of memory initialized to the proper characters *)
val e_string : string -> expr
(** Translates a binary operator on two integer expressions *)
val e_binop : expr * Syntax.op * expr -> expr
(** Translates a relational operator to a [Ir.CJump] statement *)
val e_relop : expr * Syntax.op * expr -> expr
(** Translates a simple variable expression *)
val e_simple_var : access * level -> expr
(** Translates an indexed expression to a [Ir.expr] by
    generating a memory accessor instruction that looks
    like [Mem (BinOp(Mem(expr), Plus, (BinOp(sub, Mult, word_size))))] *)
val e_subscript_var : expr -> expr -> expr
(** Given a field initialer expression and
    its index generates an [Ir.expr] to init that field *)
val e_field_var : expr -> int -> expr
(** Translates a given [n]-element record [expr list] by
    generating an [Ir.expr], that creates an [n]-element record
    by calling an external memory-allocating function and initializes
    fields with a series of [Move] statements *)
val e_record : expr list -> expr
(** Translates an array expression to [Ir.expr] that
    calls an external function that allocates a memory and initializes it *)
val e_array : expr * expr -> expr
(** Translates conditional expression *)
val e_cond : expr * expr * expr option -> expr
(** Produces an [Ir.expr] corresponding to the
    conditional expression of the AST **)
val e_loop : expr * expr * Temp.label -> expr
(** Translates a break expression to an [Ir.Jump] to the
    "done"-label of the nearest enclosing loop *)
val e_break : Temp.label -> expr
(** Translates a call expression at the given label *)
val e_call
  :  Temp.label * expr list
  -> level * level
  -> bool
  -> expr
(** Translates an assignment expression to [Ir.expr] that
    is equivalent to [Move(dst, src)] *)
val e_assign : expr * expr -> expr
(** Translates a sequences of expressions *)
val e_seq : expr list -> expr
(** Translates a "let"-expression *)
val e_let : expr list * expr -> expr

(** A dummy expression (to be removed by
    the end of implementation of this module) *)
val e_dummy : unit -> expr
