(** Holds information about formal parameters and
    local variables allocated in this frame *)
type t [@@deriving show]

(** Abstract location of a formal parameter (function argument) or
    a local variable that may be placed in a frame or in a register *)
type access [@@deriving show]

(** Makes a new frame for a function with the
    given label and formal parameters *)
val mk : label:Temp.label -> formals:bool list -> t

(** Get a unique identifier of the given stack frame *)
val id : t -> int

(** Extracts a list of accesses denoting
    the locations where the formal parameters will be
    kept at runtime, as seen from inside the callee *)
val formals : t -> access list

(** Allocates a new local variable in the given frame or in a register.
    The boolean argument specifies whether the new variable
    escapes and needs to go in the frame.
    Returns "in-memory" access with an offset from the frame pointer or
    "in-register" access in case if it can be allocated in a register *)
val alloc_local : t -> escapes:bool -> access

module Printer : sig
  val print_frame : t -> string
  val print_access : access -> string
end
