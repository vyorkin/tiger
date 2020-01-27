open Ir

module L = List

(* It is useful to be able to evaluate the subexpressions of
   an expression in any order. But the subexpressions of
   [Ir.expr] can contain side effects: [Ir.ESeq] and [Ir.Call]
   nodes that contain assignment statements and perform input/output.
   If IR expressions did not contain [Ir.ESeq] and [Ir.Call] nodes,
   then the order of evaluation would not matter *)

(* The idea is to lift [Ir.ESeq] nodes higher and higher in
   the tree, until they can become [Ir.Seq] nodes *)

(* For each kind of [Ir.stmt] or [Ir.expr] we can identify
   the subexpressions and apply rewriting rules to them  *)

(* From an arbitrary Tree statement, produce a list of cleaned trees
   satisfying the following properties:
	 1. No [Ir.Seq]'s or [Ir.ESeq]'s
	 2. The parent of every [Ir.Call] is an
      [Ir.Exp](..) or a [Ir.Move(Ir.Temp t, ...)] *)
let linearize stmt = []

let basic_blocks stmts label = []

let trace_schedule stmts label = []

(* When there are no [Ir.ESeq]'s we will use
   this [nop] statement (which does nothing) *)
let nop = Expr ~$0

(* Joins two statements [x] and [y] ignoring
   any side-effect-only statements that look like [Expr (Const a)]
   ([nop] statement, which does nothing) *)
let join x y =
  match x, y with
  | x, Expr (Const _) -> x
  | Expr (Const _), y -> y
  | x, y -> Seq (x, y)

let (++) x y = join x y

(* Takes a [Ir.expr list] and returns a [(Ir.stmt, Ir.expr list)].
   The first element [Ir.stmt] contains all the things that
   must be executed before the expression list [Ir.expr list].
   This includes all the statement-parts of the [Ir.ESeq]'s, as well
   as any expressions to their left with which they did not commute.

   Remember, if [Ir.stmt] and [Ir.expr] don't "commute" then we
   can't change the order of their evaluation.

   Example:
   --------

   [e1; e2; ESeq(s, e3)]

   [s] must be pulled leftward past [e2] and [e1].

   (0) If they all commute our [reorder] function will return
       [stmt, exprs] where:

       stmt  := s
       exprs := [e1; e2; e3]

   (1) If [commute s e1 = true] and [commute s e2 = false]

          s e1 == e1 s
          s e2 <> e2 s

       stmt  := Seq(Move(t1, e1), Seq(Move(t2, e2), s))
       exprs := [Temp t1; Temp t2; e3]

   (2) If [commute s e1 = false] and [commute s e2 = true]

          s e1 <> e1 s
          s e2 == e2 s

       stmt  := Seq(Move(t1, e1), s)
       exprs := [Temp t1; e2; e3]

   (3) If all don't commute - same as (1) *)
let rec reorder exprs =
  (nop, exprs)
(* Takes an [Ir.expr list] of subexpressions and a
   [build : Ir.expr list -> Ir.stmt] function.
   It pulls all [Ir.ESeq]'s out of the [exprs],
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
     reorder_expr (name :: args)
       (fun es -> Call (L.hd es, L.tl es))
  | e ->
     reorder_expr [] (fun _ -> e)
