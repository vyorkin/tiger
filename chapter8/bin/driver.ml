open Core

open Ch8

let process_fragment frag =
  let out = Fragment.print frag in
  print_endline out;
  match frag with
  | Proc { body; frame } ->
     let pure = Ir_printer.print_stmt body in
     let linearized = Canon.linearize body in
     let fid = Frame.id frame in
     print_endline @@ sprintf "\nFrame %d body:\n\n%s" fid pure;
     print_endline @@ sprintf "\nFrame %d linearized:\n" fid;
     List.iteri linearized ~f:(fun i stmt ->
         let out = Ir_printer.print_stmt stmt in
         print_endline @@ sprintf "%d -----------------\n%s\n" i out
       );
     let (label, blocks) = Canon.basic_blocks linearized in
     print_endline @@ sprintf "\nFrame %d basic blocks (label %s):\n" fid (Temp.print_label label);
     List.iteri blocks ~f:(fun bi block ->
         print_endline @@ sprintf "%d =================\n" bi;
         List.iteri block ~f:(fun si stmt ->
             let out = Ir_printer.print_stmt stmt in
             print_endline @@ sprintf "%d:\n%s\n" si out
       ));
     let trace = Canon.trace_schedule (label, blocks) in
     print_endline @@ sprintf "\nFrame %d trace (label %s):\n" fid (Temp.print_label label);
     List.iteri trace ~f:(fun i stmt ->
         let out = Ir_printer.print_stmt stmt in
         print_endline @@ sprintf "%d >>>>>>>>>>>>>>>>>\n%s\n" i out
       )
  | String _ ->
     ()

let tiger lexbuf =
  let expr = Parser.main Lexer.read lexbuf in
  Escape.traverse_prog expr;
  print_endline "\nFragments:\n\n";
  expr
  |> Semant.trans_prog
  |> List.iter ~f:process_fragment

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
