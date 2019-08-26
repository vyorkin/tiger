(** Abstract name for a local variable that
    is temporarily held in a register *)
type t

(** Abstract name for a static memory address that
    is yet to be determined *)
type label

(** Returns a new temporary from an infinite set of temporaries *)
val mk : unit -> t

(** Returns a new [label], whose assembly-language name is
    the given string (if given), otherwise it is generated. *)
val mk_label : string option -> label
