type expr = unit [@@deriving show]

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
