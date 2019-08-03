type exp =
    | TYPE
    | VAR
    | FUNCTION
    | BREAK
    | OF
    | END
    | IN
    | NIL
    | LET
    | DO
    | TO
    | FOR
    | WHILE
    | ELSE
    | THEN
    | IF
    | ARRAY
    | ASSIGN
    | OR
    | AND
    | GE
    | GT
    | LE
    | LT
    | NEQ
    | EQ
    | DIVIDE
    | TIMES
    | MINUS
    | PLUS
    | DOT
    | RBRACE
    | LBRACE
    | RBRACK
    | LBRACK
    | RPAREN
    | LPAREN
    | SEMICOLON
    | COLON
    | COMMA
    | STRING of string
    | INT of int
    | ID of string
    | EOF
    [@@deriving show]
