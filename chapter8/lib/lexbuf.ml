open Core
open Lexing

let mk fn ch =
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with pos_fname = Filename.basename fn
  };
  lexbuf

let pos { lex_curr_p = pos; _ } =
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum col
