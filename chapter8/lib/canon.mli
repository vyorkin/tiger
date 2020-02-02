open Ir
open Temp

(** Removes [Ir.ESeq]'s and moves [Ir.Call]'s to the top level *)
val linearize : stmt -> stmt list

(** Groups statements into sequences of
    straight-line code (no internal jumps or labels) *)
val basic_blocks : stmt list -> label * (stmt list) list

(** Orders conditional jumps so that every
    [Ir.CJump] is followed by its [false] label *)
val trace_schedule : label * (stmt list) list -> stmt list
