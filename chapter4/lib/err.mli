type t =
  | LexingError
  | SyntaxError
  [@@deriving show]

exception Error of t * Location.loc * string

val fail : t -> 'a Location.t -> string -> 'b

val to_string : t -> 'a Location.t -> string -> string
