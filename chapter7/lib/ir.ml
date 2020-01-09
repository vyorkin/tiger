(** Represents a computation of some value (possibly with side effects) *)
type expr =
  (** Integer constant *)
  | Const of int
  (** Symbolic constant corresponding to
      an assembly language label *)
  | Name of Temp.label
  (** Temporary in abstract machine is similar to a register in a real machine.
      However, the abstract machine has an infinite number of temporaries *)
  | Temp of Temp.t
  (** Application of binary operator [op] to operands of type [expr].
      The left subexpression is evaluated before the right subexpression *)
  | BinOp of expr * binop * expr
  (** Represents contents of [Frame.word_size] bytes of
      memory starting at address [expr].If [Mem] is used as
      the left child of a [Move] it means "store", but anywhere else it means "fetch" *)
  | Mem of expr
  (** Procedure call: the application of a function [f] to argument list [l].
      The subexpression [f] is evaluated before the arguments which are evaluated from left to right *)
  | Call of expr * expr list
  (** The statement [stm] is evaluated for side effects,
      then [expr] is evaluated for a result *)
  | ESeq of stm * expr

(** Statements of the IR language perform side effects and control flow *)
and stm =
  (** There are 2 options:
      - Move (Temp.t, e) - Evaluate [e] and move it into temporary
      - Move (Mem(e1), e2) - Evaluate e1, yielding address [a].
        Then evaluate [e2], and store the result into
        [Frame.word_size] bytes of memory starting at [a] *)
  | Move of expr * expr
  (** Evaluate [expr] and discard the result *)
  | Expr of expr
  (** Transfer control (jump) to address [expr].
      The destination [expr] may be a literal label, as in [Name lab],
      or it may be an address calculated by any other kind of expression.
      The list of labels [Temp.label list] specifies all the
      possible locations that the expression [e] can evaluate to
      (this is necessary for dataflow analysis later) *)
  | Jump of expr * Temp.label list
  (** Evaluate [left], [right] in that order, yielding values [l], [r].
      Then compare [l], [r] using the relational operator [op].
      If the result is [true], jump to [t]; otherwise jump to [f] *)
  | CJump of cjump
  (** Two consequent statements *)
  | Seq of stm * stm
  (** Define the constant value of name [n] to be the current machine code address.
      This is like a label definition in assembly language. The value
      [Name n] may be the target of jumps, calls, etc *)
  | Label of Temp.label

(* a > b | c < d *)
(* ------------------------------------------------------ *)
(* Cx (fn (t, f) => Seq(CJump(Gt,a,b,t,z),
 *                      Seq(Label z, CJump(Lt,c,d,t,f)))) *)
(* ------------------------------------------------------ *)

(** Conditional jump parameters *)
and cjump = {
  op : relop;
  left : expr;
  right : expr;
  t : Temp.label;
  f : Temp.label;
}

(** The integer arithmetic operators are [Plus], [Minus], [Mul] and [Div].
    Integer bitwise logical operators are [And], [Or] and [Xor].
    Integer local shift operators [LShift] and [RShift].
    Integer arithmetic right-shift is [ARShift] *)
and binop =
  | Plus | Minus | Mul | Div
  | And | Or | Xor
  | LShift | RShift | ARShift

(** The relational operators are [Eq] and [Ne] for
    integer equality and nonequality (signed or unsigned).
    Signed integer inequalities [Lt], [Gt], [Le] and [Ge].
    Unsigned integer inequalities [Ult], [Ule], [Ugt] and [Uge] *)
and relop =
  | Eq | Ne
  | Lt | Gt | Le | Ge
  | Ult | Ule | Ugt | Uge
