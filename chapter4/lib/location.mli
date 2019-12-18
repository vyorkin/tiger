open Lexing

type loc = position * position

type 'a t =
  { value: 'a;
    loc: loc [@opaque];
  } [@@deriving show]

val mk : 'a -> loc -> 'a t

val dummy : 'a -> 'a t

val range_string : 'a t -> string
