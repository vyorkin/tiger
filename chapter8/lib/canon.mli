open Ir
open Temp

(** Block is a list of statements *)
type block = stmt list

(** Removes [Ir.ESeq]'s and moves [Ir.Call]'s to the top level *)
val linearize : stmt -> stmt list

(** Groups statements into sequences of
    straight-line code (no internal jumps or labels) *)
val basic_blocks : stmt list -> label * block list

(** Orders conditional jumps so that every
    [Ir.CJump] is followed by its [false] label *)
val trace_schedule : label * block list -> stmt list
