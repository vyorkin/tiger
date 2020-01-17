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
  (** Represents contents of [Frame.word_size] bytes of memory starting at
      address [expr]. If [Mem] is used as the left child of
      a [Move] it means "store", but anywhere else it means "fetch" *)
  | Mem of expr
  (** Procedure call: the application of a function [f] to
      argument list [l]. The subexpression [f] is evaluated before the
      arguments which are evaluated from left to right *)
  | Call of expr * expr list
  (** The statement [stmt] is evaluated for side effects,
      then [expr] is evaluated for a result *)
  | ESeq of stmt * expr
  [@@deriving show { with_path = false }]

(** Statements of the IR language perform side effects and control flow *)
and stmt =
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
  | Seq of stmt * stmt
  (** Define the constant value of name [n] to be the current machine code address.
      This is like a label definition in assembly language. The value
      [Label n] may be the target of jumps, calls, etc *)
  | Label of Temp.label
  [@@deriving show { with_path = false }]

(* a > b | c < d *)
(* ---------------------------------------------------------- *)
(* Cx (fn (t, f) => Seq(CJump(Gt, a, b, t, z),                *)
(*                      Seq(Label z, CJump(Lt, c, d, t, f)))) *)
(* ---------------------------------------------------------- *)

(** Conditional jump params *)
and cjump = {
  op : relop;
  left : expr;
  right : expr;
  t : Temp.label;
  f : Temp.label;
} [@@deriving show { with_path = false }]

(** The integer arithmetic operators are [Plus], [Minus], [Mul] and [Div].
    Integer bitwise logical operators are [And], [Or] and [Xor].
    Integer local shift operators [LShift] and [RShift].
    Integer arithmetic right-shift is [ARShift] *)
and binop =
  | Plus | Minus | Mul | Div
  | And | Or | Xor
  | LShift | RShift | ARShift
  [@@deriving show { with_path = false }]

(** The relational operators are [Eq] and [Ne] for
    integer equality and nonequality (signed or unsigned).
    Signed integer inequalities [Lt], [Gt], [Le] and [Ge].
    Unsigned integer inequalities [Ult], [Ule], [Ugt] and [Uge] *)
and relop =
  | Eq | Ne
  | Lt | Gt | Le | Ge
  | Ult | Ule | Ugt | Uge
  [@@deriving show { with_path = false }]

(** Make a [binop] out of a [Syntax.op] *)
let binop_of_op = function
  | Syntax.Plus -> Plus
  | Syntax.Minus -> Minus
  | Syntax.Times -> Mul
  | Syntax.Divide -> Div
  | op -> failwith @@
    "Invalid integer arithmetic operator: " ^
    (Syntax_printer.print_op_sym op)

(** Make a [relop] out of a [Syntax.op] *)
let relop_of_op = function
  | Syntax.Ge -> Ge
  | Syntax.Gt -> Gt
  | Syntax.Le -> Le
  | Syntax.Lt -> Lt
  | Syntax.Eq -> Eq
  | Syntax.Neq -> Ne
  | op -> failwith @@
    "Invalid relational operator: " ^
    (Syntax_printer.print_op_sym op)

(* Helper operators to simplify
   construction of complex IR expressions *)

let (|+|) l r = BinOp (l, Plus, r)
let (|-|) l r = BinOp (l, Minus, r)
let (|*|) l r = BinOp (l, Mul, r)

(* [expr] operators *)

(** Constant operator *)
let (~$) k = Const k
(** Label expression operator **)
let (~:) l = Name l
(** Memory access operator **)
let (~@) e = Mem e
(** Temp opeartor *)
let (~*) t = Temp t

(* [stmt] operators *)

let (~|) l = Label l

let (<<<) e1 e2 = Move (e1, e2)
let (<|~) e labels = Jump (e, labels)

(* Other helper operators for convenience
   (see the Page 155 of the Tiger book for the equivalent definition) *)

let (<+>) l r = ~@(l |+| r)
let (<->) l r = ~@(l |-| r)

(** Helper function to ease construction of
   array elements and record fields accessor IR.

   If [a] is a memory-resident array variable represented as [Mem(e)],
   then the contents of address [e] will be a one-word pointer value [p].
   The contents of addresses [p], [p + w], [p + 2*w], ... [p + n*w]
   (where [w] is the word size) will be elements of the array
   (all elements are one word long) *)
let indexed e i w = ~@(~@e |+| (i |*| ~$w))

(** Helper function to simplify
    construction of conditional expressions **)
let cjump left op right t f =
  CJump { op = relop_of_op op; left; right; t; f }

(** Helper function to construct a sequence of statements **)
let rec seq = function
  | s :: [] -> s
  | s :: ss -> Seq (s, seq ss)
  | [] -> Expr ~$0
