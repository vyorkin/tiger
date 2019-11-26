let process line =
  let linebuf = Lexing.from_string line in
  try
    Printf.printf "%d\n%!" (Parser.main Lexer.token linebuf)
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
  let lexbuf = Lexing.from_string "1 + 2" in
  let line, _ = Lexer.line lexbuf in
  process line;
