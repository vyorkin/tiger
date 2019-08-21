(* Every target machine architecture will have a
   different standard stack frame layout. But we don't want the
   specifics of any particular machine intruding on the
   implementation of the semantic analysis module of the Tiger compiler.
   Thus we must use abstraction to represent frames*)

type access =
  | InFrame of int
  | InReg of Temp.t


type t = {
  label: Temp.label; (* label at which the function's machine code begins *)
  formals: access; (* locations of all the formals *)
  num_locals: int ref; (* number of locals allocated so far *)
  instrs: Instruction.t list; (* instructions required to implement the "view shift" *)
}

(* variable escapes if:
   - it is passed by reference
   - it is accessed from a nested function
   - its address is taken (using C's "&" operator) *)

let new_frame label formals = "123"
(* for each formal parameter:
   - how the parameters will be seen from inside the
     function (in a register or in a frame location)
   - what instructions must be produced to
     implement the "view shift" *)

let formals f = [InFrame 1]

(* for some fn: g (x1,   x2,    x3) where x1 escapes
                g [true; false; false]
   ---------------------------------------------------------
        Pentium    |         MIPS        |       Sparc
   ---------------------------------------------------------
   M[sp + 0] <- fp | sp <- sp - k        | save %sp, -k, %sp
   fp <- sp        | M[sp + k + 0] <- r2 | M[fp + 68] <- i0
   sp <- sp - k    | t157 <- r4          | t157 <- i1
                   | t158 <- r5          | t158 <- i2
   ---------------------------------------------------------
   note that we should copy/move values that are passed
   in registers to temporaries (another CPU registers?),
   otherwise they will be replaced/overwritten if
   we call another/nested function which uses the
   same registers to receive its parameters

   the register allocator will eventually choose which
   machine register should hold t157 and t158 (for example)
   this copying (move instructions) could be a no-op if
   there is no interference (no overlapping registers?)
   *)

(* some local variables are kept in frames;
   others are kept in registers. to allocate those kept in
   frames we have the [alloc_local] function *)
let alloc_local f escapes = InFrame 2

(* for example, to allocate two local variables on the Sparc,
   [allocLocal] would be called twice, returning successively:
   - InFrame (-4)
   - InFrame (-8) *)

(* local variables that do not escape can be allocated in a register,
   escaping variables must be allocated in the frame *)
