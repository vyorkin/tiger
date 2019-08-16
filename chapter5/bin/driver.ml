open Core
open Lexing
open Ch5
open Ch5.Lexer
open Ch5.Syntax
open Ch5.Semant

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum col

let parse_with_error lexbuf =
  try
    let expr = Parser.main Lexer.read lexbuf in
    Printf.printf "%s\n" (show_expr expr);
    trans_prog expr;
  with
  | LexingError msg ->
    fprintf stderr "%a: lexing error%s\n" print_position lexbuf msg |> ignore
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf
  | Error.Error (err, loc, msg) ->
    fprintf stderr "%s\n" (Error.to_string err loc msg);
    exit (-1)

let parse filename ch =
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with pos_fname = filename
  };
  parse_with_error lexbuf

let run_parser filename () =
  In_channel.with_file filename ~f:(parse filename)

let () =
  let spec = Command.Spec.(empty +> anon ("filename" %: file)) in
  run_parser
  |> Command.basic_spec ~summary:"Run the parser" spec
  |> Command.run
