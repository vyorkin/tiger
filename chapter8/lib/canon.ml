(* It is useful to be able to evaluate the subexpressions of
   an expression in any order. But the subexpressions of
   [Ir.expr] can contain side effects: [Ir.ESeq] and [Ir.Call]
   nodes that contain assignment statements and perform input/output.
   If IR expressions did not contains [Ir.ESeq] and [Ir.Call] nodes,
   then the order of evaluation would not matter *)

(* We cannot always tell if [Ir.stmt] and [Ir.expr] commute.
   For example, whether [Move(Mem(x), y)] commutes with [Mem(z)]
   depends on whether [x = z], which we cannot always determine at
   compile time. So we conservatively approximate whether statements commute.

   It makes it possible to identify and justify special cases like:

   [BinOp(Const n, op, ESeq(s, e)) = ESeq(s, BinOp(Const n, op, e))] *)
let commute s e =
  let open Ir in
  match s, e with
  (* "Empty" statement commutes with any expression *)
  | Expr (Const _), _ -> true
  (* Label commutes with any statement  *)
  | _, Name _ -> true
  (* Constant commutes with any statement *)
  | _, Const _ -> true
  (* Anything else is assumed not to commute *)
  | _, _ -> false

(* The idea is to lift [Ir.ESeq] nodes higher and higher in
   the tree, until they can become [Ir.Seq] nodes *)

(* For each kind of [Ir.stmt] or [Ir.expr] we can identify
   the subexpressions and apply rewriting rules to them  *)

let linearize stmt = []

(* 4) Temporaries and memory locations assigned by
      [s] are not referenced by [e1] *)

let basic_blocks (stmts, label) = []

let trace_schedule (stmts, label) = []
