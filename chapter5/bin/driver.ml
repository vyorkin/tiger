open Core

open Ch5
open Syntax
open Semant

let run fn ch =
  let lexbuf = Lexbuf.mk fn ch in
  try
    let expr = Parser.main Lexer.read lexbuf in
    Printf.printf "%s\n" (show_expr expr);
    trans_prog expr ~params:{ trace = true };
  with
  | Lexer.LexingError msg ->
    Printf.eprintf "%s: lexing error%s\n" (Lexbuf.pos lexbuf) msg
  | Parser.Error ->
    Printf.eprintf "%s: syntax error\n" (Lexbuf.pos lexbuf)
  | Err.Error (err, loc, msg) ->
    Printf.eprintf "%s\n" (Err.to_string err loc msg)

let run_file fn () =
  In_channel.with_file fn ~f:(run fn)

let () =
  let spec = Command.Spec.(empty +> anon ("filename" %: string)) in
  run_file
  |> Command.basic_spec ~summary:"Run the parser" spec
  |> Command.run
