(* It is useful to be able to evaluate the subexpressions of
   an expression in any order. But the subexpressions of
   [Ir.expr] can contain side effects: [Ir.ESeq] and [Ir.Call]
   nodes that contain assignment statements and perform input/output.
   If IR expressions did not contains [Ir.ESeq] and [Ir.Call] nodes,
   then the order of evaluation would not matter *)

(* The idea is to lift [Ir.ESeq] nodes higher and higher in
   the tree, until they can become [Ir.Seq] nodes *)

(* For each kind of [Ir.stmt] or [Ir.expr] we can identify
   the subexpressions and apply rewriting rules to them  *)

let linearize stmt = []

(* 4) Temporaries and memory locations assigned by
      [s] are not referenced by [e1] *)

let basic_blocks (stmts, label) = []

let trace_schedule (stmts, label) = []
