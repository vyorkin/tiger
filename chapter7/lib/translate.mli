type expr =
  (** "Expression" represented as [Ir.expr] *)
  | Ex of Ir.expr
  (** "No result" represented as [Ir.stm] *)
  | Nx of Ir.stm
  (** "Conditional" represented as a function from label-pair to [Ir.stm].
      If you pass it a true-destination and a false-destination, it will make a
      statement that evaluates some conditionals and then jumps to
      one of the destinations (the statement will never "fall through") *)
  | Cx of (Temp.label * Temp.label -> Ir.stm)

(** Represents a nesting level *)
type level = {
  parent: level option;
  frame: Frame.t
} [@@deriving show]

(** Returns a stack of all frames, including the current/given frame *)
val stack_frames : level -> Frame.t list

(** Returns a list of ids of all stack frames,
    including the current/given frame *)
val stack_frames_path : level -> int list

(** Describes a way to access a formal parameter or a local variable.
    Basically, it is just a [Frame.access] plus a nesting [level] *)
type access = level * Frame.access [@@deriving show]

(** Outermost level at which all
    top-level functions and variables are declared *)
val outermost : level

(** Creates a new "nesting level" for a function *)
val new_level
  :  parent:level option
  -> label:Temp.label
  -> formals:bool list
  -> level

(** Extracts a list of accesses *)
val formals : level -> access list

(** Creates an [access] at the given [level].
    The argument [bool] specifies whether the variable escapes *)
val alloc_local : level:level -> escapes:bool -> access
