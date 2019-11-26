open Core
open Syntax

let rec tokens buf =
  match Lexer.read buf with
  | EOF -> [EOF]
  | tok -> tok::tokens buf

let lex_print ch =
  ch
  |> Lexing.from_channel
  |> tokens
  |> List.iter ~f:(fun e -> Format.printf "%s\n" (show_exp e))

let run filename () =
  In_channel.with_file filename ~f:lex_print

let () =
  let spec = Command.Spec.(empty +> anon ("filename" %: string)) in
  run
  |> Command.basic_spec ~summary:"Run the lexer and display tokens" spec
  |> Command.run
