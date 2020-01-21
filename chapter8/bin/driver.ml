open Core

open Ch8

let tiger lexbuf =
  let expr = Parser.main Lexer.read lexbuf in
  Escape.traverse_prog expr;
  expr
  |> Semant.trans_prog
  |> List.map ~f:Fragment.print
  |> String.concat ~sep:"\n"
  |> printf "\nFragments:\n\n%s\n"

let run_tiger fn ch =
  let open Printf in
  let lexbuf = Lexbuf.mk fn ch in
  try
    tiger lexbuf
  with
  | Lexer.LexingError msg ->
    eprintf "%s: lexing error%s\n" (Lexbuf.pos lexbuf) msg
  | Parser.Error ->
    eprintf "%s: syntax error\n" (Lexbuf.pos lexbuf)
  | Err.Error (err, loc, msg) ->
    eprintf "\n%s\n" (Err.to_string err loc msg)

let mk_config () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level @@ Some Logs.Debug;
  let trace_sources = Trace_source.[
      SymbolTable [Stdout];
      SemanticAnalysis [Stdout];
      StackFrame [Stdout];
      Escaping [Stdout]
    ] in
  Config.make ~trace_sources ()

let run_file fn () =
  (* Fmt.set_style_renderer Format.std_formatter `Ansi_tty; *)
  let cfg = mk_config () in
  Logs.set_reporter @@ Trace.mk_reporter cfg;
  (* Logs.set_reporter @@ Logs_fmt.reporter (); *)
  In_channel.with_file fn ~f:(run_tiger fn)

let () =
  let spec = Command.Spec.(empty +> anon ("filename" %: string)) in
  run_file
  |> Command.basic_spec ~summary:"Run the tiger" spec
  |> Command.run
