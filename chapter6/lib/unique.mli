(** Used for equality testing to distinguish between different record types *)
type t

val mk : unit -> t

val to_string : t -> string
