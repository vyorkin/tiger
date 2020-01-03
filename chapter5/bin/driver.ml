open Core

open Ch5

let run_tiger fn ch =
  let open Printf in
  let lexbuf = Lexbuf.mk fn ch in
  try
    let expr = Parser.main Lexer.read lexbuf in
    printf "%s\n" (Syntax.show_expr expr);
    Semant.trans_prog expr
  with
  | Lexer.LexingError msg ->
    eprintf "%s: lexing error%s\n" (Lexbuf.pos lexbuf) msg
  | Parser.Error ->
    eprintf "%s: syntax error\n" (Lexbuf.pos lexbuf)
  | Err.Error (err, loc, msg) ->
    eprintf "%s\n" (Err.to_string err loc msg)

let run_file fn () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level @@ Some Logs.Debug;
  Logs.set_reporter @@ Logs_fmt.reporter ();
  In_channel.with_file fn ~f:(run_tiger fn)

let () =
  let spec = Command.Spec.(empty +> anon ("filename" %: string)) in
  run_file
  |> Command.basic_spec ~summary:"Run the tiger" spec
  |> Command.run
