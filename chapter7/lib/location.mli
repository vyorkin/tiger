open Lexing

type loc = position * position

type 'a t =
  { value: 'a;
    loc: loc;
  } [@@deriving show { with_path = false }]

val mk : 'a -> loc -> 'a t

val dummy : 'a -> 'a t

val line : 'a t -> int

val range_string : loc -> string
