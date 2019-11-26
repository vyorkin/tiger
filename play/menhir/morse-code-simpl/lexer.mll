{
  open Lexing
  open Syntax

  (* Custom exception type for lexer errors *)
  exception SyntaxError of string
}

(* Regular expressions: *)

(* We use whitespace as a separator, so
   it is a valid token in our language *)
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let sym     = ['.' '-']+

(* Lexing rules: *)

rule read = parse
  (* Skip the new lines *)
  | newline { new_line lexbuf; SEP }
  | sym     { SYM (lexeme lexbuf) }
  | white   { SEP }
  | _       { raise (SyntaxError (Printf.sprintf "At offset %d: unexpected character.\n" (lexeme_start lexbuf))) }
  | eof     { EOF }
