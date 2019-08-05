{
  open Parser
  exception Error of string
}

let white = [' ' '\t']+
let eol = '\n'
let code = ['.' '-']+

rule line = parse
  | ([^'\n']* '\n') as line
    { Some line, true }
  | eof
    { None, false }
  | ([^'\n']+ as line) eof
    { Some (line ^ "\n"), false }

and token = parse
  | code  { CODE (Lexing.lexeme lexbuf) }
  | white { SEP }
  | eol   { EOL }
  | _     { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
