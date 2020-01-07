(** Abstract name for a local variable (or a formal parameter)
    that is temporarily held in a register *)
type t [@@deriving show]

(** Abstract name for a static memory address that
    is yet to be determined *)
type label [@@deriving show]

(** Returns a new temporary from
    an infinite set of temporaries *)
val mk : unit -> t

(** Returns a new [label], whose assembly-language name is
    the given string (if given), otherwise it is generated *)
val mk_label : string option -> label

(** Returns a string representation of the
    given label (useful for tracing) *)
val print_label : label -> string
