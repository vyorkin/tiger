(** Looks for escaping variables and records this
    info in the [escape] fields of the abstract syntax *)
val traverse_prog : Syntax.expr -> unit
