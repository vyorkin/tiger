open Core

val mk : string -> In_channel.t -> Lexing.lexbuf

val pos : Lexing.lexbuf -> string
