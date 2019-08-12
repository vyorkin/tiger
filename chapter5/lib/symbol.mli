type t [@@deriving show]

val symbol : string -> t
val name : t -> string

module SymbolOrd : Map.OrderedType

module Table : Map.S with type key = t
