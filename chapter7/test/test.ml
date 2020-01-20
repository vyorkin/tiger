open Core
open Alcotest

open Ch7

let run_file fn ch =
  let lexbuf = Lexbuf.mk fn ch in
  try
    let expr = Parser.main Lexer.read lexbuf in
    Escape.traverse_prog expr;
    ignore @@ Semant.trans_prog expr;
    pass
  with
  | Lexer.LexingError msg ->
    fail @@ Lexbuf.pos lexbuf ^ " : " ^ msg
  | Parser.Error ->
    fail @@ "Syntax error: " ^ Lexbuf.pos lexbuf
  | Err.Error (err, loc, msg) ->
    fail @@ Err.to_string err loc msg

let mk fn =
  let name = Filename.basename fn in
  let run () = In_channel.with_file fn ~f:(run_file fn) in
  let test () = check (run ()) name () () in
  test_case name `Quick test
