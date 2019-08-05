%token <int> INT "int"
%token PLUS      "+"
%token MINUS     "-"
%token TIMES     "*"
%token DIV       "/"
%token LPAREN    "("
%token RPAREN    ")"
%token EOL

%left "+" "-"
%left "*" "/"
%nonassoc UMINUS

%start <int> main

%%

main:
  | e = expr EOL { e }

expr:
  | i = "int"
    { i }
  | "(" e = expr ")"
    { e }
  | e1 = expr "+" e2 = expr
    { e1 + e2 }
  | e1 = expr "-" e2 = expr
    { e1 - e2 }
  | e1 = expr "*" e2 = expr
    { e1 * e2 }
  | e1 = expr "/" e2 = expr
    { e1 / e2 }
  | "-" e = expr %prec UMINUS
    { - e }
