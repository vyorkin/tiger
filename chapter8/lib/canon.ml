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

(* Checks if the given statement [s] and expression [e] commute.
   Commute means that we can change the order of the evaluation of [s] and [e].

   For example, it the following case we can not change the evaluation order:

   s := Move (Mem x, y)
   e := BinOp (Plus, Mem x, z)

   Because [s] and [e] don't commute in the example above ([e] depends on [s]).

   We cannot always tell if [stmt] and [expr] commute.
   For example, whether [Move (Mem x, y)] commutes with [Mem z]
   depends on whether [x = z] (if we're modifying the same memory
   address then they don't commute), which we cannot always determine at
   compile time. So we conservatively approximate whether statements commute.

   It makes it possible to identify and justify special cases like:

   [BinOp(Const n, op, ESeq(s, e)) = ESeq(s, BinOp(Const n, op, e))]

   (For more info see p.176 of the Tiger book) *)
let commute s e =
  match s, e with
  (* "Empty" statement commutes with any expression *)
  | Expr (Const _), _ -> true
  (* Label commutes with any statement  *)
  | _, Name _ -> true
  (* Constant commutes with any statement *)
  | _, Const _ -> true
  (* Anything else is assumed not to commute *)
  | _, _ -> false

let (<.>) s e = commute s e

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

   (1) If [s ~~ e1] and [not (s ~~ e2)]

          s e1 == e1 s
          s e2 <> e2 s

       stmt  := Seq(Move(t1, e1), Seq(Move(t2, e2), s))
       exprs := [Temp t1; Temp t2; e3]

   (2) If [not (s ~~ e1)] and [s ~~ e2]

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

(* [Ir.expr list -> (Ir.stmt * Ir.expr list)]
   Split a given [Ir.expr list] by pulling-out side-effectful
   statements out of it and extracting a new cleaned-up expressions *)
let rec reorder = function
  | [] ->
     (nop, [])
  (* If this is a [Call] expresion then we rewrite it by
     assigning its return value a fresh [Temp.t] *)
  | (Call _ as call) :: es ->
     let t = Temp.mk () in
     (* This technique generates extra [Move] instructions, which our
        register allocator can clean up (see chapter 11 of the Tiger-book) *)
     reorder @@ ESeq (~*t <<< call, ~*t) :: es
  (* If this is a sequence of expressions, then... *)
  | e :: es ->
     (* Split the "head" expression [e] by pulling-out a statement [s1]
        containing all the side-effects and extracting a new cleaned-up expression [e1] *)
     let (s1, e1) = do_expr e in
     (* Do the same thing for the rest of the expressions [es] recursively *)
     let (s2, es') = reorder es in
     (* If (all the side-effectful statements in) [s2] and our cleaned-up "head" expression
        [e1] commute (which means that we can change the order of their evaluation) *)
     if s2 <.> e1
     then
       (* Then we can [join] our side-effectful statements
          and the cleaned-up expressions *)
       (s1 ++ s2, e1 :: es')
     else
       (* Otherwise it is possible that side-effectful statements in [s2]
          alter/affect the result produced by [e1]. To break this dependency we
          add another [Move] statement between the [s1] and [s2] that
          assigns [e1] to a new temporary [Temp.t] *)
       let t = Temp.mk () in
       (s1 ++ (~*t <<< e1) ++ s2, ~*t :: es)

(* Takes an [Ir.expr list] of subexpressions and a
   [build : Ir.expr list -> Ir.stmt] function.
   It pulls all [Ir.ESeq]'s out of the [Ir.expr list],
   yielding a statement [s] that contains all the statements
   from the [Ir.ESeq]'s and a list [l] of cleaned-up expressions.
   Then it [join]'s them by making an [Ir.Seq (s, build l)] *)
and reorder_stmt exprs build =
  let (s, l) = reorder exprs in
  s ++ build l

(* Same thing, but returns a pair [(Ir.stmt, Ir.expr)], where
   the first element is a statement containing all the side-effects
   pulled out of [Ir.expr list] and the second one is [build l] *)
and reorder_expr exprs build =
  let (s, l) = reorder exprs in
  (s, build l)

and do_stmt = function
  | Seq (s1, s2) ->
     do_stmt s1 ++ do_stmt s2
  | Jump (e, l) ->
     reorder_stmt [e] (fun es -> L.hd es <|~ l)
  | CJump { op; left; right; t; f } ->
     reorder_stmt [left; right]
       (fun es -> CJump { op; left = L.hd es; right = L.(hd (tl es)); t; f})
  | Move (Temp t, Call (name, args)) ->
     reorder_stmt (name :: args)
       (fun es -> ~*t <<< Call (L.hd es, L.tl es))
  | Move (Temp t, src) ->
     reorder_stmt [src] (fun es -> ~*t <<< L.hd es)
  | Move (Mem addr, e) ->
     reorder_stmt [addr; e]
       (fun es -> ~@(L.hd es) <<< (L.(hd (tl es))))
  | Move (ESeq (s, e1), e2) ->
     do_stmt (Seq (s, e1 <<< e2))
  | Expr (Call (name, args)) ->
     reorder_stmt (name :: args)
       (fun es -> Expr (Call (L.hd es, L.tl es)))
  | Expr e ->
     reorder_stmt [e] (fun es -> Expr (L.hd es))
  | e ->
     reorder_stmt [] (fun _ -> e)

and do_expr = function
  | BinOp (l, op, r) ->
     reorder_expr [l; r]
       (fun es -> BinOp (L.hd es, op, L.(hd (tl es))))
  | Mem addr ->
     reorder_expr [addr] (fun es -> ~@(L.hd es))
  | ESeq (s, e) ->
     (* Pull-out side-effectful statements out of [s] *)
     let s1 = do_stmt s in
     (* Split the expression [e] by pulling-out side-effectful
        statements [s2] out of it and extracting a new cleaned-up expression [e'] *)
     let (s2, e') = do_expr e in
     (* Join side-effects, return [e'] expression *)
     (s1 ++ s2, e')
  | Call (name, args) ->
     reorder_expr (name :: args)
       (fun es -> Call (L.hd es, L.tl es))
  | e ->
     reorder_expr [] (fun _ -> e)

(* From an arbitrary [Ir.stmt] statement, produce an [Ir.stmt list] of
   cleaned statements satisfying the following properties:
	 - No [Ir.Seq]'s or [Ir.ESeq]'s
	 - The parent of every [Ir.Call] is an [Ir.Exp] or a [Ir.Move (Ir.Temp t, ...)] *)
let linearize stmt =
  let rec linear = function
    | Seq (s1, s2), ss -> linear (s1, linear (s2, ss))
    | s1, s2 -> s1 :: s2
  in linear (do_stmt stmt, [])

let basic_blocks stmts =
  (Temp.mk_label None, [])

let trace_schedule (stmts, label) = []
