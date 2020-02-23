open Core_kernel
open Ir

module S = Symbol
module ST = Symbol_table
module L = List

(* Map to store basic blocks (where key is a corresponding label) *)
module BlockMap = Map.Make (Symbol)

(* Block is a list of statements *)
type block = stmt list

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

   But the subexpressions of [expr] can contain side effects:
   [ESeq] and [Call] nodes that contain assignment statements and
   perform input/output. If IR expressions did not contain
   [ESeq] and [Call] nodes, then the order of evaluation would not matter.

   How to eliminate [ESeq] nodes?
   The idea is to lift [ESeq] nodes higher and higher in
   the tree, until they can become [Seq] nodes.

   For each kind of [stmt] or [expr] we can identify
   the subexpressions and apply rewriting rules to them.

   The transformation is done in 3 stages:

   1. Linearize:
      [stmt] is rewritten into a list of canonical trees [stmt list] without
      [Seq (s1, s2) : stmt] or [ESeq (s, e) : stmt] nodes.

   2. Basic blocks:
      The [stmt list] is groupped into a set [stmt list] of basic blocks,
      which contain no internal jumps or labels (sequences of straight-line code).

   3. Trace schedule:
      Resulting basic blocks [(stmt list) list] are ordered into a set of
      traces (in which [CJump] is followed by its [false] label). *)

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

let (<-->) s e = commute s e

(* Joins two statements [s1] and [s2] ignoring any side-effect-only statements that
   look like [Expr (Const a)] -- the [nop] statement (which does nothing) *)
let join s1 s2 =
  match s1, s2 with
  | s1, Expr (Const _) -> s1
  | Expr (Const _), s2 -> s2
  | s1, s2 -> Seq (s1, s2)

let (++) s1 s2 = join s1 s2

(* Takes a [expr list] and returns a [(stmt, expr list)].
   The first element [stmt] contains all the things that
   must be executed before the expression list [expr list].
   This includes all the statement-parts of the [ESeq]'s, as well
   as any expressions to their left with which they did not commute.

   Remember, if [stmt] and [expr] don't "commute" then we
   can't change the order of their evaluation.

   Examples:
   --------

   [e1; e2; ESeq(s, e3)]

   [s] must be pulled leftward past [e2] and [e1]
   ([<-->] is an operator for [commute])

   (0) If they all commute our [reorder] function will return
       [stmt, exprs] where:

       stmt  := s
       exprs := [e1; e2; e3]

   (1) If [s <--> e1] and [not (s <--> e2)]

          s e1 == e1 s
          s e2 <> e2 s

       stmt  := Seq(Move(t1, e1), Seq(Move(t2, e2), s))
       exprs := [Temp t1; Temp t2; e3]

   (2) If [not (s <--> e1)] and [s <--> e2]

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

(* Splits a given [expr list] by pulling-out side-effectful
   statements and extracting a new cleaned-up expressions

  [expr list -> (stmt * expr list)] *)
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
     let (s1, e1) = pull_expr e in
     (* Do the same thing for the rest of the expressions [es] recursively *)
     let (s2, es') = reorder es in
     (* If (all the side-effectful statements in) [s2] and our cleaned-up "head" expression
        [e1] commute (which means that we can change the order of their evaluation) *)
     if s2 <--> e1
     then
       (* Then we can [join] our side-effectful statements
          and the cleaned-up expressions *)
       (s1 ++ s2, e1 :: es')
     else
       (* Otherwise it is possible that side-effectful statements in [s2]
          alter/affect the result produced by [e1]. To break this dependency we
          add another [Move] statement between the [s1] and [s2] that
          assigns [e1] to a new temporary [t] *)
       let t = Temp.mk () in
       (s1 ++ (~*t <<< e1) ++ s2, ~*t :: es)

(* Takes an [expr list] of subexpressions and a
   [build : expr list -> stmt] function that
   constructs a resulting cleaned-up expression.
   It pulls all [ESeq]'s out of the [expr list],
   yielding a statement [s] that contains all the statements
   from the [ESeq]'s and a list [l] of cleaned-up expressions.
   Then it [join]'s them by making an [Seq (s, build l)] *)
and reorder_stmt exprs build =
  let (s, l) = reorder exprs in
  s ++ build l

(* Same thing, but returns a pair [(stmt, expr)], where
   the first element is a statement containing all the side-effects
   pulled out of [expr list] and the second one is [build l] *)
and reorder_expr exprs build =
  let (s, l) = reorder exprs in
  (s, build l)

and pull_stmt = function
  | Seq (s1, s2) ->
     pull_stmt s1 ++ pull_stmt s2
  | Jump (e, l) ->
     reorder_stmt [e] (fun es -> L.hd_exn es <|~ l)
  | CJump { op; left; right; t; f } ->
     reorder_stmt [left; right]
       (fun es -> CJump { op; left = L.hd_exn es; right = L.(hd_exn (tl_exn es)); t; f})
  | Move (Temp t, Call (name, args)) ->
     reorder_stmt (name :: args)
       (fun es -> ~*t <<< Call (L.hd_exn es, L.tl_exn es))
  | Move (Temp t, src) ->
     reorder_stmt [src] (fun es -> ~*t <<< L.hd_exn es)
  | Move (Mem addr, e) ->
     reorder_stmt [addr; e]
       (fun es -> ~@(L.hd_exn es) <<< (L.(hd_exn (tl_exn es))))
  | Move (ESeq (s, e1), e2) ->
     pull_stmt (Seq (s, e1 <<< e2))
  | Expr (Call (name, args)) ->
     reorder_stmt (name :: args)
       (fun es -> Expr (Call (L.hd_exn es, L.tl_exn es)))
  | Expr e ->
     reorder_stmt [e] (fun es -> Expr (L.hd_exn es))
  | e ->
     reorder_stmt [] (fun _ -> e)

and pull_expr = function
  | BinOp (l, op, r) ->
     reorder_expr [l; r]
       (fun es -> BinOp (L.hd_exn es, op, L.(hd_exn (tl_exn es))))
  | Mem addr ->
     reorder_expr [addr] (fun es -> ~@(L.hd_exn es))
  | ESeq (s, e) ->
     (* Pull-out side-effectful statements out of [s] *)
     let s1 = pull_stmt s in
     (* Split the expression [e] by pulling-out side-effectful
        statements [s2] and extracting a new cleaned-up expression [e'] *)
     let (s2, e') = pull_expr e in
     (* Join side-effects, return [e'] expression *)
     (s1 ++ s2, e')
  | Call (name, args) ->
     reorder_expr (name :: args)
       (fun es -> Call (L.hd_exn es, L.tl_exn es))
  | e ->
     reorder_expr [] (fun _ -> e)

let%expect_test "reorder empty" =
  let result = reorder [] in
  print_string ([%show: stmt * expr list] result);
  [%expect {| ((Expr (Const 0)), []) |}]

let%expect_test "reorder call" =
  let name = Temp.mk_label (Some "f") in
  let args = [~$1] in
  let stmts =
    [ Call (~:name, args)
    ] in
  let result = reorder stmts in
  print_string ([%show: stmt * expr list] result);
  [%expect {|
    ((Move ((Temp (17, None)),
        (Call ((Name { id = 5; name = "f" }), [(Const 1)])))),
     [(Temp (17, None))]) |}]

(* let%expect_test "reorder" =
 *   let stmts =
 *     [
 *     ] in
 *   let result = reorder stmts in
 *   [%expect {| |}] *)

(* Flattens the given [stmt] by removing
   [Seq]'s and placing result into [acc] *)
let rec linear stmt acc =
  match stmt with
  | Seq (s1, s2) -> linear s1 (linear s2 acc)
  | s -> s :: acc

let%expect_test "linear" =
  let l = Temp.mk_label (Some "a") in
  let t = Temp.mk () in
  let stmts = [~:l <|~ [l]; ~|l; ~*t <<< ~$1] in
  let actual = linear (seq stmts) [] in
  print_string ([%show: stmt list] actual);
  [%expect {|
     [(Jump ((Name { id = 6; name = "a" }), [{ id = 6; name = "a" }]));
       (Label { id = 6; name = "a" }); (Move ((Temp (18, None)), (Const 1)))] |} ]

(* From an arbitrary [stmt] statement, produce an [stmt list] of
   cleaned statements satisfying the following properties:
   - No [Seq]'s or [ESeq]'s
   - The parent of every [Call] is an [Expr] or a [Move (Temp t, ...)] *)
let linearize stmt =
  linear (pull_stmt stmt) []

let rec mk_blocks stmts acc ~done_label =
  match stmts with
  (* Whenever a [Label] is found, a new block is started *)
  | Label _ as label :: rest ->
     (* Iterates through the rest of [stmts] *)
     let rec next stmts block =
       match stmts with
       (* Whenever [Jump] or [CJump] is found, a block
          is ended (and the next block is started) *)
       | (Jump _ as s) :: ss | (CJump _ as s) :: ss ->
          end_block ss (s :: block)
       (* Whenever a [Label] is found and the current block in not ended,
          a new block should be started. Thus, we should append a new
          [Jump] statement to the next block's label [l] *)
       | Label l :: _ ->
          let jump = ~:l <|~ [l] in
          (* In the [next] iteration will see this new [Jump]
             statement and call the [end_block] function *)
          next (jump :: stmts) block
       (* Whenever it is a regular statement, we add it to a current block *)
       | s :: ss ->
          next ss (s :: block)
       (* No more statements left, so this is the last block.
          When flow of the program reaches the end of the last block,
          the epologue show follow. But it is inconvenient to have a
          "special" block that must come last and that has no [Jump] at the end.
          Thus, we put a [Jump] to the [done_label] at the end of the last block *)
       | [] ->
          let jump = ~:done_label <|~ [done_label] in
          next [jump] block

     (* Makes a new block, adds it to a list of
        basic blocks that we're accumulating *)
     and end_block stmts rest =
       (* We build blocks by adding statements to the head,
          so we have to reverse the list at the end *)
       let block = List.rev rest in
       mk_blocks stmts (block :: acc) ~done_label

     in
     (* A new block starts with the [label] we've found *)
     let new_block = [label] in
     next rest new_block
  (* Nothing left to do. We were adding blocks to the
     head of the list, so we have to reverse their order *)
  | [] ->
     List.rev acc
  (* If any block has been left without a [Label],
     a new label is invented an stuck there *)
  | ss ->
     let label = Temp.mk_label None in
     mk_blocks (~|label :: ss) acc ~done_label

(* From a list of cleaned statements (see the [linearize] function),
   produce a list of basic blocks satisfying the following properties:
   - Every block begins with a [Label]
   - A [Label] appears only at the beginning of a block
   - Any [Jump] or [CJump] is the last [stmt] in a block
   - Every block ends with a [Jump] or [CJump]
   - Also produce the [done_label] to which control will be passed upon exit *)
let basic_blocks stmts =
  (* A label to which control will be passed upon exit *)
  let done_label = Temp.mk_label None in
  let block_list = mk_blocks stmts [] ~done_label in
  (done_label, block_list)

(* The basic blocks can be arranged in any order, and the result
   of executing the program will be the same - every block ends
   with a jump to the appropriate place. We can take advantage of
   this to choose an ordering of the blocks satisfying the condition
   that each [CJump] is followed by its false label.

   At the same time, we can also arrange that many of the unconditional
   [Jump]'s are immediately followed by their target label. This will
   allow the deletion of these jumps, which will make the compiled
   program run a bit faster *)

(* A trace is a sequence of statements that could be consecutively
   executed during the execution of the program. It can include
   conditional branches.

   For our purposes each block must be in exactly one trace.
   To minimize the number of [Jump]'s from one trace to another,
   we would like to have as few traces as possible *)

(* Helper functions for managing blocks + tracing.
   We use the [find_exn] and [add_exn] functions here for additional safety *)

(** Lookup a basic with the given key in the map *)
let find_block map key = (* 'a t -> Key.t -> 'a option *)
  Trace.Canon.find_block key;
  BlockMap.find map key

(** Add a new block to the map **)
let add_block map key data = (* 'a t -> Key.t -> 'a -> 'a t *)
  Trace.Canon.add_block key;
  BlockMap.add_exn map ~key ~data

(* Helper function that is used to
   fill-in a symbol map of basic blocks *)
let enter_block map = function
  (* Each basic block starts with a label *)
  | (Label s :: _) as b -> add_block map s b
  | _ -> map

(*
   * [map] - contains all the basic blocks
   * [block] - a basic block that we're currently analyzing
   * [label] - a beginning of the [block]
   * [rest]  - remaining blocks (basic blocks left to analyze)
*)
let rec trace ~map block label rest =
  (* Start a new (empty) trace *)
  let map = add_block map label [] in
  (* Get all [stmt]'s of the basic block except the last one *)
  let most = L.drop_last_exn block in
  (* Let's look at the last [stmt] *)
  match L.last_exn block with
  (* Unconditional jump to some [label'] *)
  | Jump (Name label', _) ->
     (* Lookup the corresponding basic block
        (one that starts with [label']) *)
     (match find_block map label' with
      (* If it is not empty *)
      | Some ((_ :: _) as block') ->
         (* Skip the jump and just concat/join the previous
            statements with the statements we get by building a
            trace for the [block'] and the [rest] of blocks *)
         most @ trace ~map block' label rest
      (* If it is an empty basic block
         (or if it doesn't exist in our [map] which
         means it is a jump to some unknown label/address) *)
      | _ ->
         (* Just leave it as is and continue building our trace *)
         (* TODO: We might want to handle the [None] case
                  and [failWith "Destination label doesn't exists"] *)
         block @ trace_next ~map rest)
  (* If the last [stmt] of the current basic block is a conditional jump *)
  | CJump { op; left; right; t; f; } ->
     let t_block = find_block map t in
     let f_block = find_block map f in
     (match t_block, f_block with
      (* If the basic block of the false-branch is not empty
         (note that we consider the false-branch first) *)
      | _, Some ((_::_) as block') ->
         (* Concat/join statements of the current block
            (that ends with a [CJump] statement) with the statements
            we get by building trace for basic block of the
            false-branch and the [rest] of blocks *)
         block @ trace ~map block' label rest
      (* If the false-branch of [CJump] is empty (or doesn't exist,
         which I think should result in error) *)
      | Some ((_::_) as block'), _ ->
         (* We switch the [true] and [false] labels and negate the condition *)
         let inv_cjump = CJump { op = not_rel op; left; right; t; f } in
         most @ [inv_cjump] @ trace ~map block' label rest
      (*  *)
      | _, _ ->
         (* We invent a new false-label [l] and rewrite the
            single [CJump] statemtnt as three statements to achieve the
            condition that the [CJump] is followed by its false-label *)
         let l = Temp.mk_label None in
         let stmts =
           [ CJump { left; op; right; f = t; t = f }
           ; ~|l
           ; ~:l <|~ [l]
           ] in
         most @ stmts @ trace_next ~map rest
     )
  (* Unconditional jump to some destination which
     is represented by some [expr] other name [Name] *)
  | Jump _ ->
     block @ trace_next ~map rest
  | _ ->
     failwith "Basic block does not end with conditional or unconditional jump statement"

(* Starts with some basic block and follows a chain of jumps,
   marking each block and appending it to the trace *)
and trace_next ~map = function
  (* Head of the head/current block is [Label] *)
  | (Label label :: _) as block :: rest ->
     (* If this label is a beginning of basic block *)
     (match find_block map label with
      (* Yes - Build a trace *)
      | Some _ -> trace ~map block label rest
      (* No - keep looking for a beginning of basic block *)
      | None -> trace_next ~map rest)
  | [] -> []
  | _ -> failwith "Basic block does not start with a label"

(* From a list of basic blocks produces a list of statements such that:
   - Every [CJump { t; f; _ }] is immediately followed by [Label f].
   - The blocks are reordered to satisfy the property mentioned above
   - In this reordering as many [Jump (Name lab)] statements
     as possible are eliminated by falling through into [Label lab].

     The algorithm:
     --------------

     Put all the blocks of the program into a list [Q].
     while [Q] is not emtpy:
       - Start a new (empty) trace [T].
       - Remove the head block [b] from [Q].
       - While [b] is not marked:
         - Mark [b] and append it to the end of the current trace [T]
         - Examine the successors of [b] (the blocks to which [b] branches)
         - If there is any unmarked successor [c]:
           [b <- c]
      End the current trace [T] *)
let trace_schedule (done_label, blocks) =
  (* Fill-in a map of labels and their corresponding basic blocks *)
  let init = BlockMap.empty in
  let map = List.fold_left blocks ~init ~f:enter_block in
  (* Build trace *)
  let result = trace_next ~map blocks in
  (* Trace ends with a [done_label] to which control will be passed upon exit *)
  result @ [~|done_label]
