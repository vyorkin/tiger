open Core_kernel

module L = Location

include Map.S with type Key.t = Symbol.t

val find_var : 'a t -> Key.t L.t -> 'a
val find_fun : 'a t -> Key.t L.t -> 'a
val find_ty  : 'a t -> Key.t L.t -> 'a

val set_var : 'a t -> Key.t L.t -> 'a -> 'a t
val set_fun : 'a t -> Key.t L.t -> 'a -> 'a t
val set_ty  : 'a t -> Key.t L.t -> 'a -> 'a t
