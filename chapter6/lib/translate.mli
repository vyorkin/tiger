type expr = unit

(** Represents a nesting level *)
type level

(** Describes a way to access a formal parameter or a local variable.
    Basically, it is just a [Frame.access] plus a nesting [level] *)
type access = level * Frame.access

(** Outermost level at which all
    top-level functions are declared *)
val outermost : level

(** Creates a new "nesting level" for a function *)
val mk : level option -> Temp.label -> bool list -> level

(** Extracts a list of accesses *)
val formals : level -> access list

(** Creates an [access] at the given [level].
    The argument [bool] specifies whether the variable escapes *)
val alloc_local : level -> bool -> access
