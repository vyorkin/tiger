(** Removes [Ir.ESeq]'s and moves [Ir.Call]'s to top level *)
val linearize : Ir.stmt -> Ir.stmt list

(** Groups statements into sequences of straight-line code *)
val basic_blocks : (Ir.stmt list) list * Temp.label -> Ir.stmt list

(** Orders conditional jumps so taht every
    [Ir.CJump] is followed by its [false] label *)
val trace_schedule : (Ir.stmt list) list * Temp.label -> Ir.stmt list
