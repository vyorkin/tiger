(** Used for equality testing to
    distinguish between different record types *)
type t [@@deriving compare, equal, show]

val mk : unit -> t

val to_string : t -> string
