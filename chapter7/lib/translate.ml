open Core_kernel

module F = Frame
module Frag = Fragment.Store

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
  (** "Conditional" represented as a function from
      label-pair to [Ir.stmt]. If you pass it a
      true-destination and a false-destination,
      it will make a statement that evaluates some
      conditionals and then jumps to one of the destinations
      (the statement will never "fall through") *)
  | Cx of (Temp.label * Temp.label -> Ir.stmt)
[@@deriving show { with_path = false }]

(* Sometimes we will have an expression of one kind and
   we will need to convert it to an equivalent
   expression of another kind. It is helpful to have
   the following 3 conversion functions: *)

(* Converts any [expr] to [Ir.expr]
   (something that returns a result) *)
let unEx expr =
  let open Ir in
  match expr with
  (* It is already [expr], nothing to do *)
  | Ex e -> e
  (* Convert conditional expression of type
     [Temp.label * Temp.label -> Ir.stmt] to [Ir.expr] *)
  | Cx cond ->
    (* We keep the result in [r] temporary (register) *)
    let r = Temp.mk () in
    let t = Temp.mk_label None in (* true-branch label *)
    let f = Temp.mk_label None in (* false-branch label *)
    (* Sequence of [Ir.stmt] instructions that
       sets the [r] temp, which is our return value *)
    let stmt = seq
        [ ~*r <<< ~$1 (* Initially set the [r]esult temp (register) to 1 *)
        ; cond(t, f)  (* Evaluates conditional and jumps to the [t] or [f] label *)
        ; ~|f         (* False-branch *)
        ; ~*r <<< ~$0 (* Set the [r]esult to 0 *)
        ; ~|t         (* Do nothing ([r]esult is already set to 1) *)
        ] in
    ESeq (stmt, ~*r)
  (* We return 0 in case of "no result" [Ir.stmt] *)
  | Nx s -> ESeq (s, ~$0)

(* Convert any [expr] to [Ir.stmt] *)
let unNx expr =
  let open Ir in
  match expr with
  (* Evaluate [e] and discard the result *)
  | Ex e -> Expr e
  (* It is already [stmt], nothing to do *)
  | Nx s -> s
  (* In either case jump to the label [l]
     (which means that we just continue execution) *)
  | Cx cond ->
    let l = Temp.mk_label None in
    let stmt = cond(l, l) in
    Seq(stmt, ~|l)

(* Convert any [expr] (except [Nx]) to
   [Temp.label * Temp.label -> Ir.stmt] *)
let unCx expr =
  let open Ir in
  match expr with
  (* In case of 0 always jump to the false-branch *)
  | Ex (Const 0) -> fun (_, f) -> ~:f <|~ [f]
  (* In case of 1 always jump to the true-branch *)
  | Ex (Const 1) -> fun (t, _) -> ~:t <|~ [t]
  (* Convert the reqular [expr] as:
     if e = 0
     then jump to the false-branch
     else jump to the true-branch *)
  | Ex e -> fun (f, t) -> (* <- Notice the opposite order of labels *)
    cjump e Eq ~$0 t f
  (* Nothing to do *)
  | Cx cond -> cond
  (* Impossible *)
  (* TODO: add a more reasonable error msg *)
  | Nx _ -> failwith "unCx can not convert Nx (statement) to Cx (conditional)"

type level = {
  parent: level option;
  frame: F.t
} [@@deriving show { with_path = false }]

(* The [level] part will be necessary later for calculating static links,
   when the variable is accessed from a (possibly) different level *)
type access = level * F.access
[@@deriving show { with_path = false }]

let outermost =
  let label = Temp.mk_label None in
  let frame = F.mk ~label ~formals:[] in
  { parent = None; frame }

let rec parent_frames level =
  match level.parent with
  | None -> [level.frame]
  | Some parent -> level.frame :: parent_frames parent

let frames_path level =
  level |> parent_frames |> List.map ~f:F.id

let nesting_depth level =
  level |> parent_frames |> List.length

(* In the semantic analysis phase [trans_dec] creates a
   new "nesting level" for each function by calling the [new_level],
   which in turn calls the [Frame.mk] to make a new frame.

   [Semant] module keeps this [level] (along with the
   label of the functions' machine-code entry point) in
   its [FunEntry] data structure *)

let new_level ~parent ~label ~formals =
  (* The first "formal" is static link (SL) which
     is a frame pointer (FP) that we'll use for
     calculating the variable address, when the
     variable is accessed from a nested level/scope *)
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

module Sl = struct
  (* One thing to notice:

     We always know that SL is the first in the
     list of formals of a stack frame, so it always
     looks like [InFrame 0]. This means that we can simplify our
     formula to the following degenerated case:

     Mem(k_n <+> Mem(K_n-1 <+> ... <+> Mem(K_1 + FP)))
     where
     K_n = K_n-1 = ... K_1 = 0

     So the resulting expression becomes just:

     Mem(Mem(...Mem(FP)))

     But let's implement a general-form of it *)

  let rec follow ~cur ~def =
    if F.(cur.frame = def.frame)
    then
      (* Variable is defined in the current scope/level *)
      Ir.(~*F.fp)
    else
      (* Variable is defined in an outer scope/level *)
      match F.formals cur.frame with
      | sl :: _ ->
        (match cur.parent with
         | None ->
           failwith "Nested level without parent"
         | Some parent ->
           let addr = follow ~cur:parent ~def in
           F.access_expr sl ~addr
        )
      | [] ->
        failwith "No static link in formals"
end

let cx_cjump l op r =
  Cx (fun (t, f) -> Ir.cjump l op r t f)

let e_unit  = Ex Ir.(~$0)
let e_nil   = Ex Ir.(~$0)
let e_int n = Ex Ir.(~$n)

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
   a global list of such fragments (see the [Fragment.Store]) to be
   handed to the code emitter (see p.163, p.169 and p.262 of the Tiger-book) *)
let e_string s =
  Ex (Frag.push_string s)

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
  cx_cjump (unEx l) op (unEx r)

(* The [e_simple_var] must produce a chain of
   [Mem] and [BinOp(Const i, Plus, ...)] nodes to fetch
   static links for all frames between the level of use
   (the [level] passed to [e_simple_var]) and the level of
   definition (the [level] within the [access]).
   See the [Sl.follow] function for details *)
let e_simple_var ((var_level, access), level) =
  let addr = Sl.follow ~cur:level ~def:var_level in
  Ex (F.access_expr access ~addr)

let e_subscript_var expr sub =
  let e = unEx expr in
  let i = unEx sub in
  Ex Ir.(indexed e i F.word_size)

(* You can read it like this:
   [record.field = expr] *)
let e_field_var expr i =
  let open Ir in
  let r = Temp.mk () in
  (*[e] is the field initial value *)
  let e = unEx expr in
  (* Field offset in memory *)
  let offset = i * F.word_size in
  let stmt = (~*r <+> ~$offset) <<< e in
  Ex (ESeq (stmt, ~*r))

(* - [record] is the record label
   - [i]      is the field index
   - [expr]   is the field initializer *)
let e_record_field i expr ~record =
  Ir.((~*record <+> ~$(i * F.word_size)) <<< unEx expr)

(* The simplest way to create a record consisting of [n]
   fields is to call an external memory-allocation function
   that returns a pointer to an [n]-word area into a new temporary [r] *)
let e_record fields =
  let open Ir in
  (* Record label *)
  let record = Temp.mk () in
  (* Total number of words required for a record *)
  let size = List.length fields * F.word_size in
  (* [Ir.expr] for external call to allocate the required amount of memory *)
  let call = F.external_call Runtime.initRecord [~$size] in
  let init_record = ~*record <<< call in
  let init_fields = List.mapi fields ~f:(e_record_field ~record) in
  let stmt = seq @@ init_record :: init_fields in
  Ex (ESeq (stmt, ~*record))

(* This refers to an external function [Runtime.initArray] which
   is written in a language such as C or assembly language *)
let e_array (size, init) =
  let args = [unEx size; unEx init] in
  Ex (F.external_call Runtime.initArray args)

let e_cond (cond_expr, then_expr, else_expr) =
  let open Ir in
  let t = Temp.mk_label None in (* True-branch label *)
  let f = Temp.mk_label None in (* False-branch label *)
  (* [cond_expr] can't be [Nx] (no-value), so
     we may simply apply the [unCx] to it *)
  let cond = unCx cond_expr in
  (* The basic idea here is to make [else_expr] match [then_expr] (or vice versa).
     So if [then_expr] is [Ex] then we know that our conditional
     expression should return some value, hence we should
     use [unEx] to make sure that our else-branch also returns
     a value by converting it to [Ex] (of course if it exists).
     Same thing for [Nx] (statements) and [Cx] (conditionals) *)
  match then_expr, else_expr with
  (* "then" is an expression (that returns a value),
      so we need produce an [ESeq] here
      (a sequence of statements that returns something) *)
  | Ex then_ex, Some else_e ->
    let r = Temp.mk () in (* Result label *)
    let x = Temp.mk_label None in (* Exit/join label *)
    let stmt = seq
        [ cond(t, f) (* [stmt] that jumps to [t] or [f] label *)
        ; ~|t (* if cond is true *)
        ; ~*r <<< then_ex (* r := then_ex *)
        (*  --*); ~:x <|~ [x] (* jump to the [x] label *)
        (* |  *); ~|f (* else *)
        (* |  *); ~*r <<< unEx else_e (* r := unEx(else_e) *)
        (*  ->*); ~|x (* exit/join *)
        ] in
    Ex (ESeq(stmt, ~*r))
  (* "then" is a "no result" (with "else" branch) *)
  | Nx then_nx, Some else_e ->
    let x = Temp.mk_label None in
    let stmt = seq
        [ cond(t, f)
        ; ~|t
        ; then_nx
        (*  --*); ~:x <|~ [x]
        (* |  *); ~|f
        (* |      In this case we don't care what the
           |      [else_e] actually is, so just [unNx] it
           |      (we always discard the result of [else_e])
           |  *); unNx else_e
        (*  ->*); ~|x
        ] in
    Nx stmt
  (* "then" is a "no result" (without "else" branch) *)
  | Nx then_nx, None ->
    let stmt = seq
        [ cond(t, f)
        ; ~|t
        ; then_nx
        ; ~|f
        ] in
    Nx stmt
  (* As noted in the book, the [Cx] case is special (see p.162)
     and it makes sense to recognize and handle it seprately with care *)
  (* 1. "then" is a conditional expr, "else" is a "no-value" (statement).
        This is an impossible case and the [Semant] module should fail in such cases *)
  | Cx _, Some (Nx _) ->
    failwith "Translation phase has detected that \
              then-branch is a conditional expression, but else-branch is a statement. \
              Looks like semantic analysis is broken."
  (* 1. "then" is a conditional expr,
        "else" is a regular or conditional expr *)
  | Cx then_cx, Some (Ex _ | Cx _ as else_e) ->
    (* In case of [else_e] is a regular expression - convert it to [Cx] *)
    let else_cx = unCx else_e in
    let stmt (t', f') = seq
        [ cond(t, f)           (* CJump(op1, a1, b1, t, f) *)
        ; ~|t; then_cx(t', f') (* t: CJump(op2, a2, b2, t', f') *)
        ; ~|f; else_cx(t', f') (* f: CJump(op3, a3, b2, t', f') or CJump(Eq, else_e, 0, t', f') *)
        ] in
    Cx stmt
  | then_ex, None ->
    let stmt = seq
        [ cond(t, f)
        ; ~|t
        ; unNx then_ex
        ; ~|f
        ] in
    Ex (ESeq (stmt, ~$0))

(* If a [Break] statement occurs within the [body_expr]
   (and not nested within any interior [While] statements),
   the translation is simply a [Jump] to the [done_l].

   So that [tr_expr] can translate [Break] statements, the [Env.t] will
   have a new field "break" that is the "done" label of
   the nearest enclosing loop. The [Translate.e_break] function
   takes this "done" label and uses it to generate a [Jump] statement.

   When [Semant.tr_expr] is recursively calling itself in
   nonloop contexts, it can simply pass down the same
   env (with a "break" field) passed to it.

   test:
     if not(condition) goto done
     body
     goto test
   done:

   Note that we rewrite the AST of [For] to the
   corresponding [While] in the [Semant] module
   (in the [Syntax_rewriter] actually), so we don't
   have to worry about the difference between them *)
let e_loop (cond_expr, body_expr, done_l) =
  let open Ir in
  let cond_l = Temp.mk_label None in
  let body_l = Temp.mk_label None in
  let cond_e = unEx cond_expr in
  (* cond_e = 0 ? jump done_l : jump body_l *)
  let cond_j = cjump cond_e Eq ~$0 done_l body_l in
  let body_e = unNx body_expr in
  let stmt = seq
      [ ~|cond_l (* condition test label *)
      ; cond_j (* if not(cond_expr) goto done_l *)
      ; ~|body_l (* body label *)
      ; body_e (* body expression *)
      ; ~:cond_l <|~ [cond_l]
      ; ~|done_l (* "done" label *)
      ] in
  Nx stmt

(* Here [l] is the "done" label of the
   nearest enclosing loop (see p.165 of the Tiger book
   and the [loop] function definition above) *)
let e_break l =
  Nx Ir.(~:l <|~ [l])

let e_call (label, args) (cur_level, def_level) is_proc =
  let open Ir in
  let args_e = List.map args ~f:unEx in
  let args_e =
    match def_level.parent with
    | None -> args_e
    | Some def ->
      let sl = Sl.follow ~cur:cur_level ~def in
      sl :: args_e
  in
  let call = Call (~:label, args_e) in
  if is_proc then Nx (Expr call) else Ex call

let e_assign (dst, src) =
  Nx Ir.(unEx dst <<< unEx src)

let rec e_seq = function
  (* No expressions given, the result is [Const 0] *)
  | [] -> Ex Ir.(~$0)
  (* There is a single expression, so use it as the result *)
  | e :: [] -> e
  (* All expressions except the last one are
     evaluated for side-effects. Result is the last [expr] *)
  | e :: es -> Ex Ir.(ESeq (unNx e, unEx (e_seq es)))

(* Translate assignment expressions, then translate the [body] *)
let e_let (dec_exprs, body_expr) =
  let open Ir in
  let dec_es = seq @@ List.map dec_exprs ~f:unNx in
  let body_e = unEx body_expr in
  Ex (ESeq (dec_es, body_e))

let e_dummy () = Ex (Ir.Const 1)

let proc_entry_exit ({ frame; _ }, body) =
  let open Ir in
  (* Generate an [Ir.Move] instruction to put the result of
     evaluating the [body] in the [Frame.rv1] (return register) *)
  let stmt = ~*F.rv1 <<< unEx body in
  (* Augment the [body] with a "view shift" instructions
     (see p.261 of the Tiger-book) *)
  let body = F.proc_entry_exit1 (frame, stmt) in
  (* Make a new fragment and push it into our mutable storage *)
  Frag.push_proc { frame; body }

(** Helper module for pretty printing translated expressions *)
module Printer = struct
  let print_expr = function
    | Ex expr ->
      sprintf "ex:\n%s" (Ir_printer.print_expr expr)
    | Nx stmt ->
      sprintf "nx:\n%s" (Ir_printer.print_stmt stmt)
    | Cx gen_stmt ->
      let l = Temp.mk_label (Some "left") in
      let r = Temp.mk_label (Some "right") in
      let stmt = gen_stmt (l, r) in
      sprintf "cx:\n(%s, %s) ->\n%s"
        (Temp.print_label l)
        (Temp.print_label r)
        (Ir_printer.print_stmt stmt)
end
