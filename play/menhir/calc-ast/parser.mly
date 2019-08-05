%token <int> INT "int"
%token PLUS      "+"
%token MINUS     "-"
%token TIMES     "*"
%token DIV       "/"
%token LPAREN    "("
%token RPAREN    ")"
%token EOL

%start <Syntax.expr> main
%{ open Syntax %}

%%

(* [fold_left(op, elem)] recognizes a nonempty,
   left-associative list of elements. *)
let fold_left(op, elem) :=
  | elem
  | located(
      sum = fold_left(op, elem);
      ~ = op; ~ = elem; <EBinOp>
    )

(* [app(f, x)] recognizes the sequence [f; x]. *)
let app(f, x) ==
  ~ = f; ~ = x; <EUnOp>

let main :=
  ~ = expr; EOL; <>

let expr ==
  additive_expr

let additive_expr ==
  fold_left(additive_op, multiplicative_expr)

let additive_op ==
  | "+"; { OpPlus }
  | "-"; { OpMinus }

let multiplicative_expr ==
  fold_left(multiplicative_op, atomic_expr)

let multiplicative_op ==
  | "*"; { OpTimes }
  | "/"; { OpDiv }

let atomic_expr :=
  | located(id_expr)
  | ~ = delimited("(", expr, ")"); <>
  | located(app(unary_op, atomic_expr))

let id_expr ==
  | ~ = "int"; <ELiteral>

let unary_op ==
  | "-"; { OpNeg }

let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
