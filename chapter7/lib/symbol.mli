(** Symbol table item *)
type t = {
  id : int;
  name : string;
} [@@deriving compare, eq, sexp, show]

(** Make a new symbol *)
val mk : string -> t

(** Make a new unique symbol *)
val mk_unique : string -> t

val (=) : t -> t -> bool
val (<>) : t -> t -> bool

val to_string : t Location.t -> string
