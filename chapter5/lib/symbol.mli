module L = Location

type t [@@deriving show]

val symbol : string -> t
val id : t -> int
val name : t -> string

module SymbolOrd : Map.OrderedType

module Table : sig
  include Map.S with type key = t

  val find_env : string -> key L.t -> 'a t -> 'a
  val find_var : key L.t -> 'a t -> 'a
  val find_fun : key L.t -> 'a t -> 'a
  val find_ty  : key L.t -> 'a t -> 'a
end
