module L = Location

type t =
  | SyntaxError
  | TypeError
  | IdError
[@@deriving show]

exception Error of t * L.loc * string

val fail : t -> 'a L.t -> string -> 'b

val syntax_error : 'a L.t -> string -> 'b
val type_error   : 'a L.t -> string -> 'b
val id_error     : 'a L.t -> string -> 'b

val to_string : t -> L.loc -> string -> string
