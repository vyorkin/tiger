(** Handles the notion of nested scopes (via static links),
    providing the interface to the [Semant] module *)
type t = unit

(** Represents a nesting level *)
type level = int

(** Describes a way to access a formal parameter (function argument) or
    a local variable. Basically, it is just a [Frame.access] plus a nesting [level] *)
type access = level * Frame.access

type lvl_args = {
  parent: level;
  name: Temp.label;
  formals: bool list
}

(** Outermost level at which all
    top-level functions are declared *)
val outermost : level

(** Creates a new "nesting level" for a function *)
val new_level : lvl_args -> level

val formals : level -> access list

(** Creates an [access] at the given [level].
    The argument [bool] specifies whether the variable escapes *)
val alloc_local : level -> bool -> access
