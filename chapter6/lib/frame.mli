(** Holds information about formal parameters and
    local variables allocated in this frame *)
type t

(** Location of a formal parameter (function argument) or
    a local variable that may be placed in a frame or in a register *)
type access =
  | InFrame of int (** Indicates a memory location at the specific offset from the frame pointer *)
  | InReg of Temp.t (** Indicates a register location *)

(** Makes a new frame for a function with the
    given label and formal parameters *)
val new_frame : Temp.label -> bool list -> t

(** Extracts a list of accesses denoting
    the locations where the formal parameters will be
    kept at runtime, as seen from inside the callee *)
val formals : t -> access list

(** Allocates a new local variable in the give frame.
    The boolean argument specifies whether the new variable
    escapes and needs to go in the frame.
    Returns an [InFrame] access with an offset from the frame pointer *)
val alloc_local : t -> bool -> access
