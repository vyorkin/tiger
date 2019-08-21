(** Depth (nesting level) of the function that
    contains the variable declaration *)
type depth = int

(** Environment that maps variables to pairs of depth and
    a reference to a boolean flag indicating if a
    particular variable escapes *)
type env = (depth * bool ref) Symbol.Table.t

(** Looks for escaping variables and records this
    info in the [escape] fields of the abstract syntax *)
val traverse_prog : Syntax.expr -> unit
