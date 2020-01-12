open Core_kernel

(* Every target machine architecture will have a
   different standard stack frame layout. But we don't want the
   specifics of any particular machine intruding on the
   implementation of the semantic analysis module of the Tiger compiler.
   Thus we must use abstraction to represent frames *)

(* We will use the x64 Linux ABI.

   According to the System V AMD64 ABI (see Figure 3.4 "Register usage" on p.24),
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
  (* Memory location at the specific offset from the frame pointer *)
  | InFrame of int
  (* Register location *)
  | InReg of Temp.t
[@@deriving show { with_path = false }]

type t = {
  (* Frame unique id (used for equality testing and tracing) *)
  id : int;
  (* Label at which the function's machine code begins *)
  label: Temp.label;
  (* Locations of all the formals *)
  formals: access list;
  (* Number of locals allocated so far *)
  mutable locals: int;
  (* Instructions required to implement the "view shift" *)
  instrs: Instruction.t list;
} [@@deriving show { with_path = false }]

let equal x y = x.id = y.id

let (=) = equal
let (<>) x y = not (equal x y)

(* Word size in bytes *)
let word_size = 64 / 8 (* = 8 bytes *)

(* Special registers *)
let fp = Temp.mk () (* Frame Pointer (RBP in x64) *)
let sp = Temp.mk () (* Stack Pointer (RSP in x64) *)

(* Return Value (RV) register (see "Returning of values"
   on p.25 of the System-V ABI specification) *)
let rv = Temp.mk ()

(* x64 "parameter"-registers *)
let rdi = Temp.mk ()
let rsi = Temp.mk ()
let rdx = Temp.mk ()
let rcx = Temp.mk ()
let	r8  = Temp.mk ()
let r9  = Temp.mk ()
let arg_regs = [rdi; rsi; rdx; r8; r9]

(* Other x64 registers *)
let rbx = Temp.mk ()
let r10 = Temp.mk ()
let r11 = Temp.mk ()
let r12 = Temp.mk ()
let r13 = Temp.mk ()
let r14 = Temp.mk ()
let r15 = Temp.mk ()

(* Registers that are preserved by the caller *)
let caller_regs = [r10; r11]

(* Registers that are preserved by the callee *)
let callee_regs = [rbx; r12; r13; r14; r15]

(* We need the [addr] here to access different frames.
   The address of the frame is the same as the
   current frame pointer only when accessing the variable
   from its own level. When accessing the variable [access] from an
   inner-nested function, the frame address must be calculated
   using static links, and the result of this calculation will be
   the [addr] argument to our [expr] function (Page 156) *)

let expr access ~addr =
  let open Ir in
  match access with
  (* Memory accessor at offset [k] *)
  | InFrame k -> addr <+> ~$k
  (* Temp t *)
  | InReg t -> ~*t

let next_id =
  let n = ref (-1) in
  fun () -> incr n; !n

(* Creates a new location for a formal parameter or
   a local variable, given its index and [escape] flag *)
let mk_formal i = function
  (* If it escapes then we allocate it in a frame.
     We start with the offset [i + 1] here because the
     first access/formal (which is 0) always should be a
     static link (SL, pointer to the previous frame) *)
  | true -> InFrame ((i + 1) * (-word_size))
  (* When it doesn't escape we use [Temp.t] (register) *)
  | false -> InReg (Temp.mk ())

(* Makes a new stack frame *)
let mk ~label ~formals =
  let id = next_id () in
  let formals = List.mapi formals ~f:mk_formal in
  let locals = 0 in
  (* Don't know yet what instructions we need,
     so just leave it empty for now *)
  let instrs = [] in
  { id; label; formals; locals; instrs }

let id { id; _ } = id

(* For each formal parameter:
   - How the parameters will be seen from inside the
     function (in a register or in a frame location)
   - What instructions must be produced to
     implement the "view shift" *)

let formals { formals; _ } = formals

(* For some fn: g (x1,   x2,    x3) where x1 escapes
                g [true; false; false]
   ---------------------------------------------------------
        Pentium    |         MIPS        |       Sparc
   ---------------------------------------------------------
   M[sp + 0] <- fp | sp <- sp - k        | save %sp, -k, %sp
   fp <- sp        | M[sp + k + 0] <- r2 | M[fp + 68] <- i0
   sp <- sp - k    | t157 <- r4          | t157 <- i1
                   | t158 <- r5          | t158 <- i2
   ---------------------------------------------------------

   Note that we should copy/move values that are passed
   in registers to temporaries (another CPU registers?),
   otherwise they will be replaced/overwritten if
   we call another/nested function which uses the
   same registers to receive its parameters

   The register allocator will eventually choose which
   machine register should hold t157 and t158 (for example)
   this copying (move instructions) could be a no-op if
   there is no interference (no overlapping registers?)
*)

(* Some local variables are kept in frames;
   others are kept in registers *)

(* For example, to allocate two local variables on the Sparc,
   [allocLocal] would be called twice, returning successively:
   - InFrame (-4)
   - InFrame (-8) *)

(* Local variables that do not escape can be allocated in a register,
   escaping variables must be allocated in the frame *)

let alloc_local frame ~escapes =
  match escapes with
  | true ->
		frame.locals <- frame.locals + 1;
    let offset = (frame.locals + 1) * -word_size in
    InFrame offset
  | false ->
    InReg (Temp.mk ())

module Printer = struct
  let print_access = function
    | InFrame offset -> sprintf "F(%d)" offset
    | InReg temp -> sprintf "R(%s)" (Temp.show temp)

  let print_frame frame =
    let formals = List.map frame.formals ~f:print_access in
    let instrs = List.map frame.instrs ~f:Instruction.print in
    let lines =
      [ "     id: " ^ Int.to_string frame.id
      ; "  label: " ^ Temp.print_label frame.label
      ; "formals: " ^ String.concat formals ~sep:" "
      ; " locals: " ^ Int.to_string frame.locals
      ; " instrs: " ^ String.concat instrs ~sep:" "
      ]
    in String.concat lines ~sep:"\n"
end
