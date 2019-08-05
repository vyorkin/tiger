open Syntax

let rec eval (e: expr) =
  match e.value with
  | ELiteral i ->
    i
  | EBinOp (e1, OpPlus, e2) ->
    eval e1 + eval e2
  | EBinOp (e1, OpMinus, e2) ->
    eval e1 - eval e2
  | EBinOp (e1, OpTimes, e2) ->
    eval e1 * eval e2
  | EBinOp (e1, OpDiv, e2) ->
    eval e1 / eval e2
  | EUnOp (OpNeg, e) ->
    - (eval e)

let process line =
  let linebuf = Lexing.from_string line in
  try
    Printf.printf "%d\n%!" (eval (Parser.main Lexer.token linebuf))
  with
  | Lexer.Error msg ->
    Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
    Printf.fprintf stderr "At offset %d: syntax error.\n%!"
      (Lexing.lexeme_start linebuf)

let process = function
  | None -> ()
  | Some line -> process line

let rec repeat ch =
  let line, continue = Lexer.line ch in
  process line;
  if continue then
    repeat ch

let () =
  repeat (Lexing.from_channel stdin)
