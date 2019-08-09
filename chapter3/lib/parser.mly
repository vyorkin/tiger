(* Base keywords *)
%token TYPE     "type"
%token VAR      "var"
%token FUNCTION "function"
%token BREAK    "break"
%token OF       "of"
%token END      "end"
%token IN       "in"
%token NIL      "nil" (* nil denotes a value belonging to every record type *)
%token LET      "let"
%token ARRAY    "array"

(* Loops *)
%token DO    "do"
%token TO    "to"
%token FOR   "for"
%token WHILE "while"

(* Conditionals *)
%token IF   "if"
%token THEN "then"
%token ELSE "else"

(* General operators *)
%token ASSIGN ":="

(* Logical *)
%token OR  "|"
%token AND "&"

(* Comparison *)
%token GE  ">="
%token GT  ">"
%token LE  "<="
%token LT  "<"
%token NEQ "<>"
%token EQ  "="

(* Arithmetics *)
%token DIVIDE "/"
%token TIMES  "*"
%token PLUS   "+"
%token MINUS  "-"

(* Separators *)
%token DOT       "."
%token LBRACE    "{"
%token RBRACE    "}"
%token LBRACK    "["
%token RBRACK    "]"
%token LPAREN    "("
%token RPAREN    ")"
%token SEMICOLON ";"
%token COLON     ":"
%token COMMA     ","

(* Strings, Numbers, Indentifiers *)
%token <string> STRING "string"
%token <int>    INT    "int"
%token <string> ID     "id"

(* Other tokens *)
%token EOF

(* Associativity of operators *)
%nonassoc "of"
%nonassoc "then"
%nonassoc "else"
%nonassoc "do"
%nonassoc ":="
%left     "|"
%left     "&"
%nonassoc ">=" ">" "<=" "<" "<>" "="
%left     "+" "-"
%left     "*" "/"

%start <unit> main

%%

let main :=
  ~ = expr; EOF; <>

(* Top-level expression *)
let expr :=
  | primitive
  | "nil"
  | "break"
  | create_rec
  | create_arr
  | lvalue
  | assignment
  | local
  | conditional
  | loop
  | fun_call
  | unary
  | binary
  (* Note that: no_val := "(" ")" *)
  | seq

(* Primitive type *)
let primitive :=
  | "string"; { () }
  | "int"; { () }

(* Unary operator *)
let unary :=
  "-"; expr

(* Binary operator *)
let binary :=
  | bin
  | boolean

let loop :=
  | while_loop
  | for_loop

let while_loop := "while"; expr; "do"; expr
let for_loop   := "for"; "id"; ":="; expr; "to"; expr; "do"; expr

let conditional :=
  | "if"; expr; "then"; expr; "else"; expr
  | "if"; expr; "then"; expr

(* Local bindings *)
let local := "let"; decs; "in"; expr_seq; "end"

(* A declaration-sequence is a sequence of type, value, and function declarations;
   no punctuation separates or terminates individual declarations. *)
let decs := list(dec); { () }

let dec :=
  | ty_dec  (* type *)
  | var_dec (* value *)
  | fun_dec (* function declaration *)

(* Data types *)
let ty_dec := "type"; "id"; "="; ty

(* Type *)
let ty :=
  | braced(ty_fields) (* records *)
  | "array"; "of"; "id"; { () } (* arrays *)
  | "id"; { () }

let ty_fields := separated_list(",", ty_field); { () }
let ty_field  := "id"; ty_ann

(* Variables *)
let var_dec :=
  | "var"; "id";         ":="; expr
  | "var"; "id"; ty_ann; ":="; expr

(* Record and array creation *)
let create_rec := "id"; braced(init_rec_fields)
let create_arr := "id"; bracketed(expr); "of"; expr

let init_rec_fields := separated_list(",", init_rec_field); { () }
let init_rec_field  := "id"; "="; expr

(* functions *)
let fun_dec :=
  (* procedures doesn't return values *)
  | fun_head; "="; fun_body
  (* functions return values and the type is specified after the colon *)
  | fun_head; ":"; "id"; "="; fun_body

let fun_head   := "function"; "id"; fun_params
let fun_body   := expr
let fun_params := parenthesized(ty_fields)

let lvalue :=
  | "id"; { () }
  | lvalue_t

let lvalue_t :=
  | "id"; "."; "id"; { () }
  | lvalue_t; "."; "id"; { () }
  | "id"; bracketed(expr)
  | lvalue_t; bracketed(expr)

(* Assignment of the expression to lvalue *)
let assignment := lvalue; ":="; expr

(* Sequence of expressions delimited by semicolon *)
let expr_seq := separated_list(";", expr); { () }

let seq := parenthesized(expr_seq)

(* Function call *)
let fun_call := "id"; parenthesized(fun_args)

(* Function arguments *)
let fun_args := separated_list(",", expr); { () }

(* Comparison expression *)
let bin := expr; bin_op; expr
let bin_op == "+" | "-" | "*" | "/" | ">=" | ">" | "<=" | "<" | "<>" | "="

(* Boolean expression *)
let boolean := expr; boolean_op; expr
let boolean_op == "&" | "|"

(* Type annotation *)
let ty_ann := ":"; "id"

(* Helper functions *)
let parenthesized(x) == delimited("(", x, ")")
let bracketed(x)     == delimited("[", x, "]")
let braced(x)        == delimited("{", x, "}")
