open Lexing

type loc = position * position

type 'a t =
  { value: 'a;
    loc: loc;
  } [@@deriving show]

val mk : 'a -> loc -> 'a t

val dummy : 'a -> 'a t

val range_string : loc -> string