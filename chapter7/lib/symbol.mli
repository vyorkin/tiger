open Core_kernel

module L = Location

(** Symbol table item *)
type t = {
  id : int;
  name : string;
} [@@deriving eq, show]

val mk : string -> t

val (=) : t -> t -> bool
val (<>) : t -> t -> bool

(** Symbol table *)
module Table : sig
  include Map.S with type Key.t = t

  val find_var : Key.t L.t -> 'a t -> 'a
  val find_fun : Key.t L.t -> 'a t -> 'a
  val find_ty  : Key.t L.t -> 'a t -> 'a
end
