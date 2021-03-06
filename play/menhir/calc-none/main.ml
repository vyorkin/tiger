let process line =
  let linebuf = Lexing.from_string line in
  try Parser.main Lexer.token linebuf; print_endline "OK"
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
