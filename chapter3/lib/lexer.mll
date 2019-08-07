{
open Lexing
open Base
open Parser

exception SyntaxError of string
}

(* Regular expressions *)

let digit = ['0'-'9']
(* There are no negative integer literals in Tiger *)
let int = digit+
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? int
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let id = ['a'-'z' 'A'-'Z'] alphanum*

(* Lexing rules *)

rule read = parse
  (* Whitespaces *)
  | white { read lexbuf }

  (* New lines *)
  | newline { new_line lexbuf; read lexbuf }

  (* Strings *)
  | '"' { read_string (Buffer.create 16) lexbuf }

  (* Comments *)
  | "/*" { read_comment [lexbuf.lex_curr_p] lexbuf }

  (* Base keywords *)
  | "type"     { TYPE }
  | "var"      { VAR }
  | "function" { FUNCTION }
  | "break"    { BREAK }
  | "of"       { OF }
  | "end"      { END }
  | "in"       { IN }
  | "nil"      { NIL }
  | "let"      { LET }
  | "array"    { ARRAY }

  (* Loops *)
  | "do"    { DO }
  | "to"    { TO }
  | "for"   { FOR }
  | "while" { WHILE }

  (* Conditionals *)
  | "else" { ELSE }
  | "then" { THEN }
  | "if"   { IF }

  (* General operators *)
  | ":=" { ASSIGN }

  (* Logical *)
  | "|" { OR }
  | "&" { AND }

  (* Comparison *)
  | ">=" { GE }
  | ">"  { GT }
  | "<=" { LE }
  | "<"  { LT }
  | "<>" { NEQ }
  | "="  { EQ }

  (* Arithmetics *)
  | "/" { DIVIDE }
  | "*" { TIMES }
  | "-" { MINUS }
  | "+" { PLUS }

  (* Separators *)
  | "." { DOT }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ";" { SEMICOLON }
  | ":" { COLON }
  | "," { COMMA }

  (* Numbers *)
  | int { INT (Int.of_string (Lexing.lexeme lexbuf)) }

  (* Indentifiers *)
  | id { ID (Lexing.lexeme lexbuf) }

  (* Other tokens *)
  | _   { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

(* The rule to match string literals *)
and read_string buf = parse
  (* If we reach the terminating double quote, then
   * we return the contents of the buffer as a STRING. *)
  | '"'       { STRING (Buffer.contents buf) }
  (* Handling escape sequences *)
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' '/'  { Buffer.add_char buf '/';  read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _   { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError "String is not terminated") }

(* The rule to match comments (including nested comments),
 * keeping a list of where comments open. *)
and read_comment opened = parse
  (* Opening comment *)
  | "/*" { read_comment (lexbuf.lex_curr_p::opened) lexbuf }
  (* Closing comment *)
  | "*/"
    { match opened with
      (* No nested opened comments left, continue parsing. *)
      | _::[] -> read lexbuf
      (* Continue parsing comment. *)
      | _ -> read_comment (List.tl_exn opened) lexbuf
    }
  | newline { new_line lexbuf; read_comment opened lexbuf }
  | _ { read_comment opened lexbuf }
  (* Unexpected end-of-file. Update the current location to
   * point to the opening token that wasn't closed and raise an error. *)
  | eof
    { lexbuf.lex_curr_p <- List.hd_exn opened;
      raise (SyntaxError "Unterminated comment")
    }
