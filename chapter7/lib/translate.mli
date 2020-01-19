(** Each Tiger function is translated into a segment of assembly language
    with a prologue, a body and an epilogue. The body of a Tiger
    function is an expression, and the body of the translation is
    simply the translation of that expression.

    The prologue, which precedes the body in the assembly-language
    version of the function contains:

    1. Pseudo-instructions, as needed in the particular assembly language,
      to announce the beginning of a function;
    2. A label definition for the function name;
    3. An instruction to adjust the stack pointer (to allocate a new frame);
    4. Instructions to save "escaping" arguments (including the static link) into
      the frame, and to move non-escaping arguments into fresh temporary registers;
    5. Store instructions to save any callee-save registers (including the
      return address register) used within the function

    Then comes

    6. The function body;

    The epilogue comes after the body and contains

    7. An instruction to move the return value (result of the function) to
      the register reserved for that purpose (e.g. RV1 in case of x64);
    8. Load instructions to restore the callee-save registers;
    9. An instruction to reset the stack pointer (to deallocate the frame);
    10. A return instruction ([Ir.Jump] to the return address);
    11. Pseudo-instructions, as needed, to announce the end of a function.

    1, 3, 9, 11:
    ------------
    Depend of exact knowledge of the frame size, which
    will not be known until after the register allocator determines how many
    local variables need to be kept in the frame because they don't fit in registers.
    So these instructions should be generated very late in a [Frame.procEntryExit3]
    function (see p.261 of the Tiger-book).

    2, 10:
    ------
    Are also handled at that time.

    7:
    --
    Generate a move instruction [Move(RV, body)] that
    puts the result of evaluating the body in the return value (RV) location
    specified by the machine-specific frame structure

    4, 5, 8:
    --------
    Are part of the "view shift" described on p.136 of the Tiger-book.
    They should be done by a function in the [Frame] module:

    [val procEntryExit1 : Frame.t * Ir.stmt -> Ir.stmt]

 **)


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
(** Translates an assignment expression to [Ir.expr]
    that is equivalent to [Move(dst, src)] *)
val e_assign : expr * expr -> expr
(** Translates a sequences of expressions *)
val e_seq : expr list -> expr
(** Translates a "let"-expression *)
val e_let : expr list * expr -> expr

(** A dummy expression (to be removed by
    the end of implementation of this module) *)
val e_dummy : unit -> expr

(** *)
val proc_entry_exit : level * expr -> unit

(** *)
val result : unit -> Fragment.t list
