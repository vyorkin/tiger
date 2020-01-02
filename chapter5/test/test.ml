open Core
open Alcotest

open Ch5
open Ch5.Lexer

let run_test fn ch =
  let lexbuf = Lexbuf.mk fn ch in
  try
    lexbuf
    |> Parser.main Lexer.read
    |> Semant.trans_prog ~params:{ trace = false };
    pass
  with
  | LexingError msg ->
    fail @@ Lexbuf.pos lexbuf ^ " : " ^ msg
  | Parser.Error ->
    fail @@ "Syntax error: " ^ Lexbuf.pos lexbuf
  | Err.Error (err, loc, msg) ->
    fail @@ Err.to_string err loc msg

let mk fn =
  let name = Filename.basename fn in
  let run () = In_channel.with_file fn ~f:(run_test fn) in
  let test () = check (run ()) name () () in
  test_case fn `Quick test
