open Ir

module L = List

(* There are certain aspects of the IR language that do not
   correspond exactly with machine languages, and some aspects
   of IR language interfere with compile-time optimization analyses.

   For example, it is useful to be able to evaluate the
   subexpressions of an expression in any order.
   Here is the list of possible subexpressions:

   - BinOp (l_expr, op, r_expr)
   - Mem addr_expr

   - Call (name_expr, args_expr_list)
   - ESeq (stmt, result_expr)

   But the subexpressions of [Ir.expr] can contain side effects:
   [Ir.ESeq] and [Ir.Call] nodes that contain assignment statements and
   perform input/output. If IR expressions did not contain
   [Ir.ESeq] and [Ir.Call] nodes, then the order of evaluation would not matter.

   How to eliminate [Ir.ESeq] nodes?
   The idea is to lift [Ir.ESeq] nodes higher and higher in
   the tree, until they can become [Ir.Seq] nodes.

   For each kind of [Ir.stmt] or [Ir.expr] we can identify
   the subexpressions and apply rewriting rules to them.

   The transformation is done in 3 stages:

   1. Linearize:
      [Ir.stmt] is rewritten into a list of canonical trees [Ir.stmt list] without
      [Ir.Seq (s1, s2) : Ir.stmt] or [Ir.ESeq (s, e) : Ir.stmt] nodes.

   2. Basic blocks:
      The [Ir.stmt list] is groupped into a set [Ir.stmt list] of basic blocks,
      which contain no internal jumps or labels (sequences of straight-line code).

   3. Trace schedule:
      Resulting basic blocks [(Ir.stmt list) list] are ordered into a set of
      traces (in which [Ir.CJump] is followed by its [false] label). *)

(* From an arbitrary [Ir.stmt] statement, produce an
   [Ir.stmt list] of cleaned statements satisfying the following properties:
	 - No [Ir.Seq]'s or [Ir.ESeq]'s
	 - The parent of every [Ir.Call] is an [Ir.Exp] or a [Ir.Move (Ir.Temp t, ...)] *)
let linearize stmt = []

let basic_blocks stmts =
  (Temp.mk_label None, [])

let trace_schedule (stmts, label) = []

(* Joins two statements [s1] and [s2] ignoring any side-effect-only statements that
   look like [Expr (Const a)] -- the [Ir.nop] statement (which does nothing) *)
let join s1 s2 =
  match s1, s2 with
  | s1, Expr (Const _) -> s1
  | Expr (Const _), s2 -> s2
  | s1, s2 -> Seq (s1, s2)

let (++) s1 s2 = join s1 s2

(* Takes a [Ir.expr list] and returns a [(Ir.stmt, Ir.expr list)].
   The first element [Ir.stmt] contains all the things that
   must be executed before the expression list [Ir.expr list].
   This includes all the statement-parts of the [Ir.ESeq]'s, as well
   as any expressions to their left with which they did not commute.

   Remember, if [Ir.stmt] and [Ir.expr] don't "commute" then we
   can't change the order of their evaluation.

   Examples:
   --------

   [e1; e2; ESeq(s, e3)]

   [s] must be pulled leftward past [e2] and [e1]
   ([~~] is an operator for [commute])

   (0) If they all commute our [reorder] function will return
       [stmt, exprs] where:

       stmt  := s
       exprs := [e1; e2; e3]

   (1) If [s ~~ e1 = true] and [s ~~ e2 = false]

          s e1 == e1 s
          s e2 <> e2 s

       stmt  := Seq(Move(t1, e1), Seq(Move(t2, e2), s))
       exprs := [Temp t1; Temp t2; e3]

   (2) If [s ~~ e1 = false] and [s ~~ e2 = true]

          s e1 <> e1 s
          s e2 == e2 s

       stmt  := Seq(Move(t1, e1), s)
       exprs := [Temp t1; e2; e3]

   (3) If all don't commute - same as (1)

   Moving calls to the top level:
   -----------------------------

   Each function returns its result in the same dedicated
   return-value register [Frame.rv1]. Thus, if we have something like
   [BinOp (Call ..., Plus, Call ...)] then the second call will
   overwrite the [Frame.rv1] register before [Plus] can be executed.
   The idea is to assign each return value immediately
   into a fresh temporary register *)
let rec reorder = function
  | [] ->
     (nop, [])
  | (Call _ as e) :: es ->
     let t = Temp.mk () in
     (* This technique generates extra [Move] instructions,
        which our register allocator can clean up (see chapter 11 of the Tiger-book) *)
     reorder @@ ESeq (~*t <<< e, ~*t) :: es
  | e :: es ->
     let (s1, e1) = do_expr e in
     let (s2, es) = reorder es in
     if s2 <.> e
     then
       (s1 ++ s2, e1 :: es)
     else
       let t = Temp.mk () in
       (s1 ++ (~*t <<< e1) ++ s2, ~*t :: es)

(* Takes an [Ir.expr list] of subexpressions and a
   [build : Ir.expr list -> Ir.stmt] function.
   It pulls all [Ir.ESeq]'s out of the [Ir.expr list],
   yielding a statement [s] that contains all the
   statements from the [Ir.ESeq]'s and a list [l] of
   cleaned-up expressions. Then it makes [Ir.Seq (s, build l)] *)
and reorder_stmt exprs build =
  let (s, e) = reorder exprs in
  s ++ build e

(* Same thing, but returns a pair [(Ir.stmt, Ir.expr)], where
   the first element is a statement containing all the side-effects
   pulled out of [expr] and the second one is [build l] *)
and reorder_expr exprs build =
  let (s, e) = reorder exprs in
  (s, build e)

and do_stmt = function
  | Seq (s1, s2) ->
     do_stmt s1 ++ do_stmt s2
  | Jump (e, l) ->
     reorder_stmt [e] (fun es -> Jump (L.hd es, l))
  | CJump { op; left; right; t; f } ->
     reorder_stmt [left; right]
       (fun es -> CJump { op; left = L.hd es; right = L.(hd (tl es)); t; f})
  | Move (Temp t, Call (name, args)) ->
     reorder_stmt (name :: args)
       (fun es -> Move (Temp t, Call (L.hd es, L.tl es)))
  | Move (Temp t, src) ->
     reorder_stmt [src] (fun es -> Move (Temp t, L.hd es))
  | Move (Mem addr, e) ->
     reorder_stmt [addr; e]
       (fun es -> Move (Mem (L.hd es), (L.(hd (tl es)))))
  | Move (ESeq (s, e1), e2) ->
     do_stmt (Seq (s, Move (e1, e2)))
  | Expr (Call (name, args)) ->
     reorder_stmt (name :: args)
       (fun es -> Expr (Call (L.hd es, L.tl es)))
  | Expr e ->
     reorder_stmt [e] (fun es -> Expr (L.hd es))
  | e ->
     reorder_stmt [] (fun _ -> e)

and do_expr = function
  | BinOp (l, op, r) ->
     reorder_expr [l; r] (fun es -> BinOp (L.hd es, op, L.(hd (tl es))))
  | Mem addr ->
     reorder_expr [addr] (fun es -> Mem (L.hd es))
  | ESeq (s, e) ->
     let s1 = do_stmt s in
     let (s2, e) = do_expr e in
     (s1 ++ s2, e)
  | Call (name, args) ->
     reorder_expr (name :: args) (fun es -> Call (L.hd es, L.tl es))
  | e ->
     reorder_expr [] (fun _ -> e)
