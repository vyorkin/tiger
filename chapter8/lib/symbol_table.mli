open Core_kernel

module L = Location

include Map.S with type Key.t = Symbol.t

type entry =
  | Var
  | Fun
  | Typ

val entry_abbr : entry -> string
val entry_string : entry -> string

val look_var : 'a t -> Key.t L.t -> 'a
val look_fun : 'a t -> Key.t L.t -> 'a
val look_typ : 'a t -> Key.t L.t -> 'a

val bind_var : 'a t -> Key.t L.t -> 'a -> 'a t
val bind_fun : 'a t -> Key.t L.t -> 'a -> 'a t
val bind_typ : 'a t -> Key.t L.t -> 'a -> 'a t
