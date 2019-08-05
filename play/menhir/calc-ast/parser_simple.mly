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

let main :=
  ~ = expr; EOL; <>

let expr ==
  additive_expr

let additive_expr :=
  | multiplicative_expr
  | located(
    ~ = additive_expr; ~ = additive_op; ~ = multiplicative_expr; <EBinOp>
    )

let additive_op ==
  | "+"; { OpPlus }
  | "-"; { OpMinus }

let multiplicative_expr :=
  | atomic_expr
  | located(
    ~ = multiplicative_expr; ~ = multiplicative_op; ~ = atomic_expr; <EBinOp>
    )

let multiplicative_op ==
  | "*"; { OpTimes }
  | "/"; { OpDiv }

let atomic_expr :=
  | "("; ~ = expr; ")"; <>
  | located(
    | ~ = "int"; <ELiteral>
    | ~ = unary_op; ~ = atomic_expr; <EUnOp>
    )

let unary_op ==
  | "-"; { OpNeg }

let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
