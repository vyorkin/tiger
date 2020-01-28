(** Removes [Ir.ESeq]'s and moves [Ir.Call]'s to top level *)
val linearize : Ir.stmt -> Ir.stmt list

(** Groups statements into sequences of
    straight-line code (no internal jumps or labels) *)
val basic_blocks : Ir.stmt list -> Temp.label * (Ir.stmt list) list

(** Orders conditional jumps so that every
    [Ir.CJump] is followed by its [false] label *)
val trace_schedule : Temp.label * (Ir.stmt list) list -> Ir.stmt list
