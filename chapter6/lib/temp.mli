(** Abstract name for a local variable that
    is temporarily held in a register *)
type t

(** Abstract name for a static memory address that
    is yet to be determined. Just like a label in assembly language *)
type label

(** Returns a new temporary from an infinite set of temps *)
val new_temp : unit -> t

(** Returns a new label from an infinite set of labels *)
val new_label : unit -> label

(** Returns a new label whose assembly-language name is [string] *)
val named_label : string -> label
