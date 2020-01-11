open Core_kernel

module F = Frame

(* We separate [Semant] from [Translate] module to
   avoid a huge, unweildy module that does both:
   type checking and semantic translation *)

(* [expr] is an abstract data type,
   its constructors visible only within the [Translate] module *)

type expr =
  (** "Expression" represented as [Ir.expr] *)
  | Ex of Ir.expr
  (** "No result" represented as [Ir.stmt] *)
  | Nx of Ir.stmt
  (** "Conditional" represented as a function from label-pair to [Ir.stmt].
      If you pass it a true-destination and a false-destination, it will make a
      statement that evaluates some conditionals and then jumps to
      one of the destinations (the statement will never "fall through") *)
  | Cx of (Temp.label * Temp.label -> Ir.stmt)
  [@@deriving show { with_path = false }]

(* Sometimes we will have an expression of one kind and
   we will need to convert it to an equivalent
   expression of another kind. It is helpful to have
   the following 3 conversion functions: *)

let unEx (expr : expr) = Ir.(~$ 0)
(* let unEx expr =
 *   let open Ir in
 *   match expr with
 *   | Ex e -> e
 *   | Cx gen_stmt ->
 *     let r = Temp.mk () in
 *     let t = Temp.mk_label None in
 *     let f = Temp.mk_label None in
 *     ESeq (mk_seq(
 *         Move(Temp r, Const 1),
 *         gen_stmt(t, f),
 *         Label f,
 *         Move (Temp r, Const 0),
 *         Label t
 *       ), Temp r)
 *   | Nx s -> ESeq (s, Const 0) *)

let unNx expr = expr
let unCx expr = expr

type level = {
  parent: level option;
  frame: F.t
} [@@deriving show { with_path = false }]

(* The [level] part will be necessary later for calculating static links,
   when the variable is accessed from a (possibly) different level *)
type access = level * F.access [@@deriving show]

let outermost =
  let label = Temp.mk_label None in
  let frame = F.mk ~label ~formals:[] in
  { parent = None; frame }

let rec frames level =
  match level.parent with
  | None -> [level.frame]
  | Some parent -> level.frame :: frames parent

let frames_path level =
  level |> frames |> List.map ~f:F.id

(* In the semantic analysis phase [trans_dec] creates a
   new "nesting level" for each function by calling the [new_level],
   which in turn calls the [Frame.mk] to make a new frame.

   [Semant] module keeps this [level] (along with the
   label of the functions' machine-code entry point) in
   its [FunEntry] data structure *)

let new_level ~parent ~label ~formals =
  (* The first "formal" is static link (SL) which
     is a frame pointer (FP) that we'll use for
     calculating the variable address, when the variable is
     accessed from a nested level/scope/frame *)
  let static_link = true in
  let formals = static_link :: formals in
  let frame = F.mk ~label ~formals in
  { parent; frame }

(* Returns formals (excluding the static link) *)
let formals level =
  (* Exclude static link (SL) *)
  F.formals level.frame
  |> List.tl_exn
  |> List.map ~f:(fun access -> level, access)

(* When [Semant] proceses a local variable declaration at
   some level, it calls [alloc_local] to create the
   variable in this level. The result is [access].

   Later, when the variable is used in an expression,
   [Semant] hands this [access] back to the [Translate] module in
   order to generate the machine code to access the variable *)

let alloc_local ~level ~escapes =
  let access = F.alloc_local level.frame ~escapes in
  level, access

(* Helper function to simplify
   construction of conditional expressions *)
let cjump left op right =
  Cx (fun (t, f) -> Ir.(CJump { op = relop_of_op op; left; right; t; f }))

let e_unit  = Ex Ir.(~$ 0)
let e_nil   = Ex Ir.(~$ 0)
let e_int n = Ex Ir.(~$ n)

(* A string literal in the Tiger language is the constant
   address of a segment of memory initialized to the proper characters.
   In assembly language a label is used to refer to this address from
   the middle of some sequence of instructions. At some other place in
   the assembly-language program, the definition of that label appears,
   followed by the assembly-language pseudo-instruction to reserve and
   initialize a block of memory to the appropriate characters.

   For each string literal [s], the [Translate] module makes a
   new [label] [l], and returns the [Ir.Name l].
   It also puts the assembly-language fragment [Frame.String (l, s)] onto
   a global list of such fragments to be handed to the code emitter
   (see p.163, p.169 and p.262 of the Tiger-book) *)
let e_string s =
  let l = Temp.mk_label None in
  (* TODO: fragments := F.String (l, s) :: !fragments; *)
  (* TODO: Lookup the given [s] in existing fragments before adding a new one *)
  Ex Ir.(~: l)

(* Integer arithmetic is easy to translate (see p.161).
   It's easy to convince yourself that each arithmetic
   operator in [Syntax.op] corresponds to the [Ir.binop] by
   looking at the [Syntax] and [Ir] modules.
   We add a [binop_of_op] function to the [Ir] module to
   be more explicit about that *)
let e_binop (l, op, r) =
  Ex Ir.(BinOp (unEx l, binop_of_op op, unEx r))

(* Making "simple" [Cx] expressions from [Syntax.expr] and
   [Syntax.op] could be done with the [Ir.CJump] operator *)
let e_relop (l, op, r) =
  cjump (unEx l) op (unEx r)

(* The [e_simple_var] must produce a chain of
   [Mem] and [BinOp(Const i, Plus, ...)] nodes to fetch
   static links for all frames between the level of use
   (the [level] passed to [e_simple_var]) and the level of
   definition (the [level] within the [access]).
   See the [Sl.follow] function for details *)

let e_simple_var (access, level) = Ex Ir.(~$ 0)
let e_field_var (expr, name, fields) = Ex Ir.(~$ 0)
let e_subscript_var (expr, sub) = Ex Ir.(~$ 0)

let e_record _ = Ex Ir.(~$ 0)
let e_array _ = Ex Ir.(~$ 0)
let e_cond _ = Ex Ir.(~$ 0)
let e_loop _ = Ex Ir.(~$ 0)
let e_break _ = Ex Ir.(~$ 0)
let e_call _ = Ex Ir.(~$ 0)
let e_assign _ = Ex Ir.(~$ 0)
let e_seq _ = Ex Ir.(~$ 0)
let e_let _ = Ex Ir.(~$ 0)

let dummy_expr () = Ex (Ir.Const 1)

(** Helper module for pretty printing translated expressions *)
module Printer = struct
  let print_expr = function
    | Ex expr -> Ir_printer.print_expr expr
    | Nx stmt -> Ir_printer.print_stmt stmt
    | Cx gen_stmt -> "Cx gen_stmt"
end
