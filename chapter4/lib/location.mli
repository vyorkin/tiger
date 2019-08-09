open Lexing

type 'a t [@@deriving show]

type loc = position * position

val mk : 'a -> loc -> 'a t

val dummy : 'a -> 'a t

val loc : 'a t -> loc

val value : 'a t -> 'a

val range_string : 'a t -> string
