(* Every target machine architecture will have a
   different standard stack frame layout. But we don't want the
   specifics of any particular machine intruding on the
   implementation of the semantic analysis module of the Tiger compiler.
   Thus we must use abstraction to represent frames*)

(* We will use the x64 Linux ABI.

   According to the System V AMD64 ABI,
   the first 6 integer arguments are passed in left-to-right order in
   RDI, RSI, RDX, RCX, R8 and R9 registers, respectively.

   Arguments five and higher are passed in memory (kept in a stack frame).
   They are pushed onto the stack in reversed (right-to-left) order.

   Helpful links:

   - http://refspecs.linuxfoundation.org/elf/x86_64-abi-0.99.pdf (page 12)
   - https://en.wikipedia.org/wiki/X86_calling_conventions
   - https://wiki.osdev.org/Calling_Conventions
 *)


(* Location of a formal parameter (function argument) or
   a local variable that may be placed in a frame or in a register *)
type access =
   (* memory location at the specific offset from the frame pointer *)
  | InFrame of int
  (* register location *)
  | InReg of Temp.t
  [@@deriving show]

type t = {
  (* label at which the function's machine code begins *)
  label: Temp.label;
  (* locations of all the formals *)
  formals: access list;
  (* number of locals allocated so far *)
  locals: int ref;
  (* instructions required to implement the "view shift" *)
  instrs: Instruction.t list;
} [@@deriving show]

(* word size in bytes *)
let word_size = 64 / 8 (* = 8 bytes *)

(* special registers *)
let fp = "rbp" (* frame pointer *)
let sp = "rsp" (* stack pointer *)

(* x64 "parameter"-registers *)
let rdi = "rdi"
let rsi = "rsi"
let rdx = "rdx"
let rcx = "rcx"
let	r8  = "r8"
let r9  = "r9"
let arg_regs = [rdi; rsi; rdx; r8; r9]

(* other x64 registers *)
let rbx = "rbx"
let r10 = "r10"
let r11 = "r11"
let r12 = "r12"
let r13 = "r13"
let r14 = "r14"
let r15 = "r15"

(* registers that are preserved by the caller *)
let caller_preserved_regs = [r10; r11]

(* registers that are preserved by the callee *)
let callee_preserved_regs = [rbx; r12; r13; r14; r15]

(* creates a new location formal parameter or
   a local variable, given its index and [esc] flag *)
let mk_access i = function
  | true -> InFrame ((i + 1) * (-word_size)) (* escapes -- alloc in frame *)
  | false -> InReg (Temp.mk ()) (* doesn't escape -- use temp (register) *)

(* makes a new stack frame *)
let mk label formals =
  let formals = List.mapi mk_access formals in
  let locals = ref 0 in
	(* don't know yet what instruction we need,
     so just leave it empty for now *)
  let instrs = [] in
  { label; formals; locals; instrs }

(* for each formal parameter:
   - how the parameters will be seen from inside the
     function (in a register or in a frame location)
   - what instructions must be produced to
     implement the "view shift" *)

let formals { formals; _ } = formals

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
   others are kept in registers. To allocate those kept in
   frames we use the [alloc_local] function *)

(* for example, to allocate two local variables on the Sparc,
   [allocLocal] would be called twice, returning successively:
   - InFrame (-4)
   - InFrame (-8) *)

(* local variables that do not escape can be allocated in a register,
   escaping variables must be allocated in the frame *)

let alloc_local { locals; _ } = function
  | true ->
		incr locals;
    let offset = (!locals + 1) * (-word_size) in
    InFrame offset
  | false ->
    InReg (Temp.mk ())
