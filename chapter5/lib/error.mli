type t =
  | SyntaxError
  | TypeError
  | IdError
[@@deriving show]

exception Error of t * Location.loc * string

val fail : t -> 'a Location.t -> string -> 'b

val syntax_error : 'a Location.t -> string -> 'b
val type_error   : 'a Location.t -> string -> 'b
val id_error     : 'a Location.t -> string -> 'b

val to_string : t -> Location.loc -> string -> string
