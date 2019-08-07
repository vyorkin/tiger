%token <int> INT "int"
%token PLUS      "+"
%token MINUS     "-"
%token TIMES     "*"
%token DIV       "/"
%token LPAREN    "("
%token RPAREN    ")"
%token EOL

%start <unit> main

%%

let main :=
  add_e; EOL

let add_e :=
  | mul_e
  | add_e; "+"; mul_e
  | add_e; "-"; mul_e

let mul_e :=
  | atomic_expr
  | mul_e; "*"; atomic_expr
  | mul_e; "/"; atomic_expr

let atomic_expr :=
  | delimited("(", add_e, ")")
  | "int"
  | preceded("-", atomic_expr)
