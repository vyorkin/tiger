module S = Symbol
module L = Location

(** A [For] statement can be expressed
    using other kinds of statements:

    for i := lo to hi
      do body

    can be rewritten to

    let var i := lo
        var limit := hi
     in while i <= limit
          do (
            body;
            i := i + 1
          )
    end

    See p.166 of the Tiger-book *)
let rewrite_for var lo hi body escapes =
  let open Syntax in
  let open L in
  Trace.SyntaxRewriting.rewrite_for var lo hi body escapes;
  let i = SimpleVar var in
  let limit_sym = ~?(S.mk "limit") in
  let limit = SimpleVar limit_sym in
  (* var i := lo *)
  let i_dec =
    { var_name = var;
      var_typ = None;
      init = lo;
      escapes;
    } in
  (* var limit := hi *)
  let limit_dec =
    { var_name = limit_sym;
      var_typ = None;
      init = hi;
      escapes = ref false;
    } in
  (* let var i := ...
         var limit := ... *)
  let decs =
    [ VarDec ~?i_dec
    ; VarDec ~?limit_dec
    ] in
  (* i < limit *)
  let cond = Op (~?(Var ~?i), ~?Le, ~?(Var ~?limit)) in
  (* i + 1 *)
  let incr = Op (~?(Var ~?i), ~?Plus, ~?(Int ~?1)) in
  (* i := i + 1 *)
  let assign = Assign (~?i, ~?incr) in
  (* body; i := i + 1 *)
  let body = Seq [body; ~?assign] in
  (* while i <= limit
       do (body; i := i + 1) *)
  let loop = While (~?cond, ~?body) in
  ~?(Let (decs, ~?loop))
