open Core_kernel

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

let unEx expr = expr
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
  frame: Frame.t
} [@@deriving show { with_path = false }]

(* Helper function to calc number of frames
   between the [inner] and [outer] levels.
   We need it to
  *)
let dist ~inner ~outer =
  let rec go inner outer i =
    if Frame.(inner.frame = outer.frame)
    then i
    else begin
      match inner.parent with
      | Some lev -> go lev outer (i + 1)
      | None -> failwith "Nested level without parent"
    end
  in go inner outer 0

(* the [level] part will be necessary later for calculating static links,
   when the variable is accessed from a (possibly) different level *)
type access = level * Frame.access [@@deriving show]

(* static link -- pointer to / address of the frame of
   the function statically enclosing current function *)

let outermost =
  let label = Temp.mk_label None in
  let frame = Frame.mk ~label ~formals:[] in
  { parent = None; frame }

let rec stack_frames level =
  match level.parent with
  | None -> [level.frame]
  | Some parent -> level.frame :: stack_frames parent

let stack_frames_path level =
  level |> stack_frames |> List.map ~f:Frame.id

(* in the semantic analysis phase [trans_dec] creates a
   new "nesting level" for each function by calling the [new_level],
   which in turn calls the [Frame.mk] to make a new frame.

   [Semant] module keeps this [level] (along with the
   label of the functions' machine-code entry point) in
   its [FunEntry] data structure *)

let new_level ~parent ~label ~formals =
  let formals = true :: formals in
  let frame = Frame.mk ~label ~formals in
  { parent; frame }

(* Returns formals (excluding the static link) *)
let formals level =
  (* exclude the SL *)
  Frame.formals level.frame
  |> List.tl_exn
  |> List.map ~f:(fun access -> level, access)

(* When [Semant] proceses a local variable declaration at
   some level, it calls [alloc_local] to create the
   variable in this level. The result is [access].

   Later, when the variable is used in an expression,
   [Semant] hands this [access] back to the [Translate] module in
   order to generate the machine code to access the variable *)

let alloc_local ~level ~escapes =
  let access = Frame.alloc_local level.frame ~escapes in
  level, access

(* Following static link (SL).
   We need to use static links to access variables
   declared at an outer level of static scope.

   For example, to access some variable [x] which is
   delcared somewhere outside of the current level/scope the
   generated IR code should look like:

   Mem(BinOp(Const k_n, Plus,
   Mem(BinOp(Const k_n-1, Plus,
   ...
   Mem(BinOp(Cons k_1, Plus,
   Temp fp))))))

   where k_1,...,k_n-1 are the various SL offsets in
   nested functions, and k_n is the offset of
   our variable [x] in its own frame *)

let rec follow_sl = function
  | 0 -> Ir.Temp Frame.fp
  | k -> Ir.(Mem(BinOp(Const Frame.word_size, Plus, follow_sl (k - 1))))

(* The [simple_var] must produce a chain of
   [Mem] and [BinOp(Const i, Plus, ...)] nodes to fetch
   static links for all frames between the level of use
   (the [level] passed to [simple_var]) and the level of definition
   (the [level] within the [access]) *)

let simple_var (access, level) = Ex (Ir.Const 1)
let field_var (expr, name, fields) = Ex (Ir.Const 1)
let subscript_var (expr, sub) = Ex (Ir.Const 1)

let dummy_expr () = Ex (Ir.Const 1)

(** Helper module for pretty printing translated expressions *)
module Printer = struct
  let print_expr = function
    | Ex expr -> Ir_printer.print_expr expr
    | Nx stmt -> Ir_printer.print_stmt stmt
    | Cx gen_stmt -> "Cx gen_stmt"
end
